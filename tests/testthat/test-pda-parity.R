# CI pins the unmodified Penncil/pda revision in R-CMD-check.yaml.
officialPda <- function(name) {
  if (!requireNamespace("pda", quietly = TRUE)) {
    if (nzchar(Sys.getenv("FEDERATEDLEARNING_PDA_REF"))) {
      stop("The pinned PDA reference is required in CI")
    }
    skip("pda is not installed")
  }
  # An installed package with an incompatible interface must fail, not skip.
  get(name, envir = asNamespace("pda"), inherits = FALSE)
}

pdaAdapFunctions <- function() {
  estimate <- officialPda("ADAP.estimate")
  env <- new.env(parent = environment(estimate))
  # These helpers are nested inside upstream ADAP.estimate, not in its namespace.
  # Evaluate their original function definitions without rewriting their bodies.
  for (name in c("expit", "NLogLik", "Lgradient", "Lgradient2", "soft", "coordi")) {
    definitions <- Filter(function(expr) {
      is.call(expr) &&
        (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) &&
        identical(expr[[2]], as.name(name))
    }, as.list(body(estimate))[-1L])
    if (length(definitions) != 1L) stop("Official PDA no longer defines ", name)
    eval(definitions[[1]], envir = env)
  }
  env
}

test_that("CI uses the pinned official PDA reference", {
  expect_type(officialPda("ADAP.estimate"), "closure")
  ref <- Sys.getenv("FEDERATEDLEARNING_PDA_REF")
  if (nzchar(ref)) {
    description <- utils::packageDescription("pda")
    expect_identical(description$RemoteSha, ref)
    expect_identical(description$RemoteRepo, "pda")
    expect_identical(tolower(description$RemoteUsername), "penncil")
  }
})

test_that("PDA is optional locally but cannot be silently skipped in reference CI", {
  guard <- officialPda
  scope <- new.env(parent = environment(guard))
  scope$requireNamespace <- function(...) FALSE
  scope$Sys.getenv <- function(...) ""
  environment(guard) <- scope
  expect_condition(guard("ADAP.initialize"), class = "skip")
  scope$Sys.getenv <- function(...) "pinned-reference"
  expect_error(guard("ADAP.initialize"), "pinned PDA reference is required")
})

test_that("ADAP logistic primitives match the official nested functions", {
  reference <- pdaAdapFunctions()
  set.seed(101)
  x <- cbind(1, matrix(rnorm(36), nrow = 12))
  y <- rbinom(12, 1, 0.4)
  beta <- c(-0.1, 0.2, -0.05, 0.03)
  for (design in list(x, Matrix::Matrix(x, sparse = TRUE))) {
    expect_equal(.logisticNegGradient(beta, design, y),
      as.numeric(reference$Lgradient(beta, x, y)), tolerance = 1e-12)
    expect_equal(unname(.logisticNegHessian(beta, design)),
      unname(reference$Lgradient2(beta, x)), tolerance = 1e-12)
    expect_equal(.logisticNegHessianDiag(beta, design),
      as.numeric(diag(reference$Lgradient2(beta, x))), tolerance = 1e-12)
    expect_equal(.negLogLikMean(beta, design, y),
      reference$NLogLik(beta, x, y), tolerance = 1e-12)
  }
})

test_that("ADAP initialization matches official PDA with matched standardization", {
  initialize <- officialPda("ADAP.initialize")
  set.seed(103)
  x <- sweep(matrix(rnorm(600), nrow = 200), 2, c(0.2, 1, 5), "*")
  y <- rbinom(200, 1, stats::plogis(-1.2 + 5 * x[, 1] - x[, 2]))
  set.seed(104)
  reference <- initialize(data.frame(status = y, cbind(1, x)), list(), list(site_id = "site1"))
  expect_gt(abs(reference$bhat_i[[1]]), 0.1)
  expect_true(any(reference$bhat_i[-1] != 0))
  for (dense in c(TRUE, FALSE)) {
    set.seed(104)
    ours <- .clientUpdatePdaAdap(
      list(xMatrix = Matrix::Matrix(cbind(1, x), sparse = TRUE), yLabels = y, n = length(y)),
      list(phase = 0L),
      list(intercept = TRUE, standardize = TRUE, maxDenseInitCells = if (dense) Inf else 0)
    )
    expect_equal(ours$bhat, reference$bhat_i, tolerance = 1e-6)
    expect_equal(ours$n, reference$site_size)
  }
})

test_that("ADAP summaries use official site initialization files and sample-size weights", {
  derive <- officialPda("ADAP.derive")
  directory <- tempfile("pda-reference-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  sizes <- c(40L, 80L)
  betas <- list(c(-0.5, 0.2, -0.1, 0.3), c(-1, -0.1, 0.4, 0.1))
  sites <- c("site1", "site2")
  config <- list(site_id = sites[[1]], dir = directory)
  for (i in seq_along(sites)) {
    pda::pdaPut(list(site = sites[[i]], site_size = sizes[[i]],
      bhat_i = betas[[i]], Vhat_i = rep(sizes[[i]], 4)),
      paste0(sites[[i]], "_initialize"), config,
      upload_without_confirm = TRUE, silent_message = TRUE, digits = 16)
  }
  initial <- .serverRoundPdaAdap(.serverInitPdaAdap(list(p = 3L)),
    Map(function(beta, n) list(bhat = beta, n = n), betas, sizes), list())$state
  betaBar <- as.numeric(do.call(cbind, betas) %*% (sizes / sum(sizes)))
  expect_equal(initial$betaBar, betaBar, tolerance = 1e-12)
  set.seed(102)
  reference <- reports <- vector("list", length(sites))
  for (i in seq_along(sites)) {
    x <- cbind(1, matrix(rnorm(sizes[[i]] * 3), nrow = sizes[[i]]))
    y <- rbinom(sizes[[i]], 1, 0.4)
    reference[[i]] <- derive(data.frame(status = y, x), list(sites = sites), config)
    client <- list(xMatrix = Matrix::Matrix(x, sparse = TRUE), yLabels = y, n = sizes[[i]])
    reports[[i]] <- .clientUpdatePdaAdap(client, initial, list(intercept = TRUE))
    expect_equal(reports[[i]]$grad, as.numeric(reference[[i]]$logL_D1), tolerance = 1e-12)
    expect_equal(unname(reports[[i]]$Hess), unname(reference[[i]]$logL_D2), tolerance = 1e-12)
    for (mode in c("first", "diag")) {
      reduced <- .clientUpdatePdaAdapReduced(client,
        list(phase = 1L, betaBar = betaBar, adapReducedMode = mode), list(intercept = TRUE))
      expect_equal(reduced$grad, as.numeric(reference[[i]]$logL_D1), tolerance = 1e-12)
      # Diagonal ADAP is our extension, compared to the diagonal of official full curvature.
      if (mode == "diag") expect_equal(reduced$HessDiag,
        as.numeric(diag(reference[[i]]$logL_D2)), tolerance = 1e-12)
    }
  }
  aggregated <- .serverRoundPdaAdap(initial, reports, list())$state
  weights <- sizes / sum(sizes)
  expect_equal(aggregated$globalGrad,
    Reduce("+", Map(function(d, w) as.numeric(d$logL_D1) * w, reference, weights)), tolerance = 1e-12)
  expect_equal(unname(aggregated$globalHess),
    unname(Reduce("+", Map(function(d, w) d$logL_D2 * w, reference, weights))), tolerance = 1e-12)
})

test_that("ODAL derivatives match official PDA with the opposite likelihood sign", {
  derive <- officialPda("ODAL.derive")
  set.seed(105)
  x <- cbind(1, matrix(rnorm(40), nrow = 10))
  y <- rbinom(10, 1, 0.5)
  beta <- c(-0.1, 0.2, 0.05, -0.03, 0.08)
  reference <- derive(data.frame(status = y, x), list(beta_init = beta), list(site_id = "site1"))
  expect_equal(.logisticNegGradient(beta, Matrix::Matrix(x, sparse = TRUE), y),
    -as.numeric(reference$logL_D1), tolerance = 1e-12)
  expect_equal(unname(.logisticNegHessian(beta, Matrix::Matrix(x, sparse = TRUE))),
    unname(-as.matrix(reference$logL_D2)), tolerance = 1e-12)
})

test_that("ADAP uses official quadratic components but excludes the intercept from its lambda range", {
  reference <- pdaAdapFunctions()
  set.seed(106)
  x <- cbind(1, matrix(rnorm(90), nrow = 30))
  y <- rbinom(30, 1, 0.4)
  betaBar <- c(-0.3, 0.1, -0.2, 0.05)
  betaEval <- c(-0.2, 0.2, 0, 0.1)
  globalGrad <- as.numeric(reference$Lgradient(betaBar, x, y)) + c(0.03, -0.04, 0.02, 0.01)
  globalHess <- reference$Lgradient2(betaBar, x) + diag(c(0.01, 0.02, 0.03, 0.04))
  hEval <- reference$Lgradient2(betaEval, x)
  correction <- globalHess - reference$Lgradient2(betaBar, x)
  B <- hEval + correction
  a <- as.numeric(reference$Lgradient(betaEval, x, y) - t(betaEval) %*% hEval +
    globalGrad - reference$Lgradient(betaBar, x, y) - t(betaBar) %*% correction)
  ours <- .adapSurrogateComponents(betaEval, betaBar,
    Matrix::Matrix(x, sparse = TRUE), y, globalGrad, globalHess)
  expect_equal(ours$aTilde, a, tolerance = 1e-12)
  expect_equal(unname(ours$B), unname(B), tolerance = 1e-12)
  coordinateScores <- abs(a + as.numeric((B - diag(diag(B))) %*% betaBar))
  # Unlike upstream's range, ours does not use the unpenalized intercept score.
  lambdaMax <- max(coordinateScores[-1])
  expect_lt(lambdaMax, max(coordinateScores))
  path <- .pdaAdapLambdaSeq(Matrix::Matrix(x, sparse = TRUE), y,
    betaEval, betaBar, globalGrad, globalHess, gridLen = 100L)
  expect_equal(path, rev(exp(seq(log(1e-4 * lambdaMax), log(lambdaMax), length.out = 100L))),
    tolerance = 1e-12)
})

test_that("compiled quadratic lasso agrees with official PDA coordinate descent", {
  reference <- pdaAdapFunctions()
  a <- c(0.4, -0.8, 0.2)
  full <- matrix(c(3, 0.2, -0.1, 0.2, 2, 0.3, -0.1, 0.3, 1.5), 3)
  for (B in list(full, diag(diag(full)))) {
    for (lambda in c(0, 0.15, 1)) {
      invisible(utils::capture.output(expected <- reference$coordi(a, B, rep(0, 3), lambda)))
      expect_identical(expected$message, "Successful convergence")
      ours <- .coordDescentQuadraticLasso(a, B, rep(0, 3), lambda, maxIter = 500L, tol = 1e-12)
      # Upstream stops at a fixed 1e-4 change in its quadratic objective.
      expect_equal(ours, as.numeric(expected$betainit), tolerance = 1e-3)
      objective <- function(beta) sum(a * beta) + sum(beta * (B %*% beta)) / 2 + lambda * sum(abs(beta[-1]))
      expect_lte(abs(objective(ours) - objective(expected$betainit)), 1e-6)
      expect_lt(abs(as.numeric(a + B %*% ours)[[1]]), 1e-7)
    }
  }
})
