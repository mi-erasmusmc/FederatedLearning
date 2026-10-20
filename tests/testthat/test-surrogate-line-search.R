test_that("composite backtracking uses the true objective and unpenalized intercept", {
  objective <- function(b) binaryLogLoss(rep(b[1], 20), rep(c(0, 1), 10), TRUE) + 0.1 * abs(b[2])
  start <- c(-6, 0.5)
  candidate <- c(100, 0)
  gradient <- c(plogis(start[1]) - 0.5, 0)
  step <- .surrogateArmijoStep(start, candidate, gradient, .1, c(FALSE, TRUE), objective)
  expect_true(step$accepted)
  expect_gt(step$backtracks, 0)
  expect_lt(step$alpha, 1)
  expect_lt(step$objectiveAfter, step$objectiveBefore)
  expect_equal(step$predictedDecrease, sum(gradient * (candidate - start)) - 0.05)
  expect_lte(step$objectiveAfter,
    step$objectiveBefore + 1e-4 * step$alpha * step$predictedDecrease + 1e-14)
  expect_equal(step$beta, start + step$alpha * (candidate - start))
})

test_that("backtracking retains failures and exact full-step zeros", {
  objective <- function(b) sum((b - c(1, 0))^2) / 2 + abs(b[2])
  step <- .surrogateArmijoStep(c(0, 1), c(1, 0), c(-1, 1), 1, c(FALSE, TRUE), objective)
  expect_identical(step$beta, c(1, 0))
  expect_equal(step$alpha, 1)
  failed <- .surrogateArmijoStep(c(0, 1), c(-1, 1), c(-1, 1), 1, c(FALSE, TRUE), objective)
  expect_false(failed$accepted)
  expect_identical(failed$failureReason, "non_descent_surrogate_direction")
  expect_identical(failed$beta, c(0, 1))
  failed <- .surrogateArmijoStep(0, 1, -1, 0, FALSE,
    function(b) if (b == 0) 0 else Inf, maxBacktracks = 3L)
  expect_false(failed$accepted)
  expect_identical(failed$failureReason, "surrogate_line_search_failed")
  expect_equal(failed$backtracks, 3)
  expect_error(.surrogateArmijoStep(0, NA, -1, 0, FALSE, identity), "Invalid")
  expect_error(.surrogateArmijoStep(0, 1, -1, -1, FALSE, identity), "Invalid")
})

lineSearchFixture <- function() {
  x <- cbind(1, rep(c(-1, 0, 1), each = 40))
  y <- rep(c(0, 1, 0, 1, 1, 1, 0, 0), 15)
  bar <- c(0, 0)
  list(xDesign = x, y = y, betaLead = c(-6, 0.5), betaBar = bar,
    globalGrad = .logisticNegGradient(bar, x, y), globalHess = .logisticNegHessian(bar, x),
    lambda = .1, maxOuter = 100L, maxInner = 100L, tol = 1e-8, returnDetails = TRUE)
}

test_that("sparse and dense backtracked fits agree without changing default CD", {
  args <- lineSearchFixture()
  ordinary <- do.call(.fitPdaAdapSurrogate, args)
  expect_identical(do.call(.fitPdaAdapSurrogate, c(args, list(outerLineSearch = "none"))), ordinary)
  dense <- do.call(.fitPdaAdapSurrogate, c(args, list(outerLineSearch = "armijo")))
  args$xDesign <- Matrix::Matrix(args$xDesign, sparse = TRUE)
  sparse <- do.call(.fitPdaAdapSurrogate, c(args, list(outerLineSearch = "armijo")))
  expect_false(.adapFitFailed(dense))
  expect_false(.adapFitFailed(sparse))
  expect_equal(sparse$beta, dense$beta, tolerance = 1e-8)
  expect_lt(max(abs(sparse$beta)), 1e-6)
  expect_true(any(sparse$outerTrace$backtracks > 0))
  expect_true(all(sparse$outerTrace$objectiveAfter <= sparse$outerTrace$objectiveBefore + 1e-14))
  expect_lt(sparse$kktMaxAbs, 1e-6)
  expect_identical(sparse$beta[2], 0)
  expect_error(do.call(.fitPdaAdapSurrogate, c(args,
    list(outerLineSearch = "armijo", traceDiagnostics = TRUE))), "without traceDiagnostics")
  expect_error(do.call(.fitPdaAdapSurrogate, c(args,
    list(outerLineSearch = "armijo", strictCorrection = "prox"))), "without traceDiagnostics")
  expect_error(do.call(.fitPdaAdapSurrogate, c(args, list(outerLineSearch = "bad"))), "arg")
})

test_that("backtracking does not change correction policy or repair negative curvature", {
  args <- lineSearchFixture()
  args$betaLead <- args$betaBar
  args$globalHess <- args$globalHess - diag(.01, 2)
  default <- do.call(.fitPdaAdapSurrogate, c(args, list(outerLineSearch = "armijo")))
  expect_identical(default$failureReason, "negative_C_eigenvalue")
  fit <- do.call(.fitPdaAdapSurrogate, c(args,
    list(outerLineSearch = "armijo", curvatureAction = "report")))
  expect_false(.adapFitFailed(fit))
  expect_lt(fit$C_eigen_min, 0)
  expect_equal(fit$rho, 0)
  args$globalHess <- -diag(1, 2)
  failed <- do.call(.fitPdaAdapSurrogate, c(args,
    list(outerLineSearch = "armijo", curvatureAction = "report")))
  expect_true(.adapFitFailed(failed))
  expect_match(failed$failureReason, "non_positive_coordinate_curvature")
})

test_that("complete CV and final fitting capture the explicit line-search choice", {
  args <- lineSearchFixture()
  x <- Matrix::Matrix(args$xDesign, sparse = TRUE)
  data <- list(xMatrix = x, yLabels = args$y, n = length(args$y), rowId = seq_along(args$y))
  state <- list(phase = 2L, leadIndex = 1L, betaLead = args$betaBar, betaBar = args$betaBar,
    globalGrad = args$globalGrad, globalHess = args$globalHess - diag(.01, 2),
    totalN = 10 * data$n, lambdaSeq = c(.2, .1, .01), adapMethodName = "ADAP2")
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(FederatedLearning.localId = 1L)
  on.exit(options(old), add = TRUE)
  config <- list(intercept = TRUE, foldsK = 5L, cvSeed = 42L, lambdaSearch = "optimize",
    lambdaSearchMaxEvals = 5L, adapCvDiagnostics = TRUE, maxOuter = 100L, maxInner = 100L,
    tol = 1e-7, adapCurvatureAction = "report", adapOuterLineSearch = "armijo",
    surrogateCaptureDirectory = directory)
  config <- .prepareSurrogateCapture(config, "ADAP2")
  fit <- .clientUpdatePdaAdap(data, state, config)
  expect_true(all(fit$cvValid))
  inputs <- lapply(list.files(config$surrogateCaptureDirectory, "^input-", full.names = TRUE), readRDS)
  expect_length(inputs, 5 * length(fit$lambdaSeq) + 1L)
  expect_true(all(vapply(inputs, function(x) identical(x$settings$outerLineSearch, "armijo"), logical(1))))
  final <- inputs[[which(vapply(inputs, function(x) x$context$phase == "final", logical(1)))]]
  expect_equal(final$inputs$quadraticCorrection, -diag(.01, 2))
  expect_equal(final$inputs$lambda, fit$selectedLambda)
  config$adapSolveStyle <- "pda"
  config$surrogateCaptureDirectory <- NULL
  expect_error(.clientUpdatePdaAdap(data, state, config), "requires the exact")
})

test_that("backtracked fits match independent bounded and local-solution controls", {
  e <- new.env(parent = globalenv())
  for (file in c("auditCapturedSurrogates.R", "compareSurrogateSolvers.R")) {
    path <- test_path("..", "..", "extras", file)
    skip_if_not(file.exists(path), "Diagnostic scripts unavailable")
    sys.source(path, e)
  }
  controls <- e$surrogateSolverControls()
  for (name in c("bounded_full", "bounded_lasso", "bounded_rare_near", "indefinite_near", "indefinite_near_lasso")) {
    f <- controls[[name]]
    i <- f$inputs
    before <- rlang::hash(f)
    globalHess <- if (is.null(i$globalHess)) {
      e$.captureAuditTerms(f$data$xDesign, f$data$y, i$betaBar)$hessian
    } else i$globalHess
    fit <- .fitPdaAdapSurrogate(
      Matrix::Matrix(f$data$xDesign, sparse = TRUE), f$data$y, i$betaStart, i$betaBar,
      i$globalGrad, globalHess, i$lambda, maxOuter = 1000L, maxInner = 1000L,
      tol = 1e-10, returnDetails = TRUE, curvatureAction = "report", outerLineSearch = "armijo")
    expect_identical(rlang::hash(f), before)
    expect_false(.adapFitFailed(fit), info = name)
    audit <- e$.captureAuditState(f$data, i, fit$beta)
    expect_lt(audit$kktMaxAbs, 1e-6)
    target <- if (is.null(f$oracle)) f$localStationaryPoint else f$oracle
    expect_equal(fit$beta, target, tolerance = 1e-4, info = name)
    expect_lte(max(fit$outerTrace$objectiveAfter - fit$outerTrace$objectiveBefore), 1e-12)
  }
})

test_that("backtracking does not mislabel a saturated rare-outcome fit as converged", {
  x <- Matrix::Matrix(matrix(1, 100, 1), sparse = TRUE)
  y <- rep(0:1, c(98, 2))
  bar <- stats::qlogis(.02)
  fit <- .fitPdaAdapSurrogate(x, y, 8, bar,
    .logisticNegGradient(bar, x, y), .logisticNegHessian(bar, x), 0,
    returnDetails = TRUE, curvatureAction = "report", outerLineSearch = "armijo")
  expect_true(.adapFitFailed(fit))
  expect_match(.adapFitFailureReason(fit), "zero_unpenalized_coordinate_curvature")
  expect_true(all(is.finite(fit$beta)))
  expect_gt(fit$kktMaxAbs, 1e-4)
  expect_true(any(fit$outerTrace$backtracks > 0))
  expect_true(all(fit$outerTrace$objectiveAfter <= fit$outerTrace$objectiveBefore))
})
