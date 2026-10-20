test_that("public ADAP names dispatch to the documented fitters", {
  skip_if_not_installed("withr")
  calls <- list()
  record <- function(name) {
    function(...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- list(name = name, args = args)
      list(beta = c(-1, .1, -.2), converged = TRUE)
    }
  }
  local_mocked_bindings(
    .fitPdaAdapSurrogate = record("full"),
    .fitPdaAdapFirstOrderSurrogate = record("first"),
    .fitPdaAdapRemoteDiagSurrogate = record("diag"),
    .fitPdaAdapPdaProx = record("legacy"),
    .package = "FederatedLearning"
  )
  withr::local_options(FederatedLearning.localId = 1L)
  client <- list(xMatrix = matrix(seq_len(12) / 12, 6, 2),
    yLabels = rep(0:1, 3), n = 6L)
  run <- function(method, config = list()) {
    config <- c(list(p = 2L, lambda = .1, intercept = FALSE), config)
    algorithm <- .getAlgorithm(method)
    state <- algorithm$serverInit(config)
    state$phase <- 2L
    state$leadIndex <- 1L
    state$globalGrad <- rep(0, 3)
    state$globalHess <- diag(3)
    state$globalHessDiag <- rep(1, 3)
    algorithm$clientUpdate(client, state, config)
    calls[[length(calls)]]
  }
  for (method in c("ADAP", "ADAP2")) {
    call <- run(method)
    expect_identical(call$name, "full")
    expect_identical(call$args$strictCorrection, "exact")
  }
  expect_identical(run("ADAP1")$name, "first")
  expect_identical(run("ADAPDiag")$name, "diag")
  fullLegacy <- run("ADAP_PDA")
  expect_identical(fullLegacy$name, "legacy")
  expect_true(fullLegacy$args$useFull)
  diagLegacy <- run("ADAPDiag", list(adapDiagStyle = "pda"))
  expect_identical(diagLegacy$name, "legacy")
  expect_false(diagLegacy$args$useFull)
  expect_length(calls, 6L)
})

test_that("legacy pda updates use simultaneous diagonal steps, not cyclic CD", {
  x <- cbind(1, c(0, 1, 0, 1))
  y <- c(0, 0, 1, 1)
  beta <- c(0, 0)
  gradient <- c(.1, -.2)
  hessian <- matrix(c(1, .5, .5, 1), 2)
  lambda <- .05
  fit <- .fitPdaAdapPdaProx(x, y, beta, gradient, hessian,
    lambda, maxIter = 1L, returnDetails = TRUE)
  expect_equal(fit$beta, c(-.1, .15), tolerance = 1e-12)
  cyclic <- .coordDescentQuadraticLasso(gradient, hessian, beta,
    lambda, maxIter = 1L, returnDetails = TRUE)
  expect_equal(cyclic$beta, c(-.1, .2), tolerance = 1e-12)
})

test_that("exact CV freezes initialization and removes validation derivatives only when requested", {
  x <- cbind(1, seq(-1, 1, length.out = 12), rep(c(0, 1, 2), 4))
  y <- rep(c(0, 1, 0, 0), 3)
  lead <- 1:8
  validation <- c(2L, 5L, 7L)
  train <- setdiff(lead, validation)
  betaBar <- c(-.7, .2, -.1)
  betaLead <- c(-.4, .1, .05)
  terms <- function(rows) .logisticNegGradientHessian(betaBar, x[rows, ], y[rows])
  global <- terms(1:12)
  retained <- terms(setdiff(1:12, validation))
  local_mocked_bindings(
    .fitPdaStyleLocalLasso = function(...) stop("CV must not refit initialization"),
    .pdaAdapSurrogateLeadCv = function(..., betaInit, makeFoldInfo, fitFold) {
      info <- list(fold = 1L, idxVal = validation, idxTr = train,
        nVal = length(validation))
      info <- c(info, makeFoldInfo(info))
      fitFold(info, lambda = .02, warmStart = betaInit)
    },
    .fitPdaAdapSurrogate = function(...) list(...),
    .fitPdaAdapFirstOrderSurrogate = function(...) list(...),
    .fitPdaAdapRemoteDiagSurrogate = function(...) list(...),
    .package = "FederatedLearning"
  )
  for (adjustment in c("leaveValOut", "pda")) {
    args <- list(xDesign = x[lead, ], y = y[lead], betaLead = betaLead,
      betaBar = betaBar, globalGrad = global$gradient, lambdaSeq = .02,
      totalN = 12, globalAdjustment = adjustment)
    expected <- if (adjustment == "leaveValOut") retained else global
    fits <- list(
      full = do.call(.pdaAdapLeadCv, c(args, list(globalHess = global$hessian))),
      first = do.call(.pdaAdapFirstLeadCv, args),
      diag = do.call(.pdaAdapDiagLeadCv, c(args, list(globalHessDiag = diag(global$hessian))))
    )
    for (fit in fits) {
      expect_equal(fit$betaBar, betaBar)
      expect_equal(fit$betaLead, betaLead)
      expect_equal(fit$betaInit, betaLead)
      expect_equal(fit$xDesign, x[train, ])
      expect_equal(fit$y, y[train])
      expect_equal(fit$globalGrad, expected$gradient, tolerance = 1e-12)
    }
    expect_equal(fits$full$globalHess, expected$hessian, tolerance = 1e-12)
    expect_equal(fits$diag$globalHessDiag, diag(expected$hessian), tolerance = 1e-12)
  }
  expect_gt(max(abs(global$gradient - retained$gradient)), .01)
})

test_that("ADAP deviance selection pools log loss over held-out observations", {
  x <- cbind(1, seq(-1, 1, length.out = 17))
  y <- rep(0:1, length.out = 17)
  losses <- sizes <- numeric(3)
  cv <- .pdaAdapSurrogateLeadCv(x, y, betaInit = c(0, 0), lambdaSeq = .1,
    foldsK = 3L, seed = 8L,
    makeFoldInfo = function(info) list(),
    fitFold = function(info, ...) {
      beta <- c(-info$fold, .2)
      eta <- as.numeric(x[info$idxVal, ] %*% beta)
      losses[info$fold] <<- mean(log1p(exp(eta)) - y[info$idxVal] * eta)
      sizes[info$fold] <<- length(info$idxVal)
      list(beta = beta, converged = TRUE)
    })
  expect_equal(cv$scores, weighted.mean(losses, sizes), tolerance = 1e-12)
  expect_gt(abs(mean(losses) - weighted.mean(losses, sizes)), 1e-4)
})

test_that("ODAL averages local coefficients by sample size, not inverse variance", {
  reports <- list(list(bhat = c(-1, .2), n = 20),
    list(bhat = c(-2, .8), n = 80))
  state <- .serverRoundODAL(.serverInitODAL(list(p = 1L)), reports, list())$state
  expect_equal(state$betaBar, c(-1.8, .68))
  expect_identical(state$leadIndex, 2L)
  # Upstream meta initialization uses a different weight for each coordinate.
  variances <- rbind(c(.1, .8), c(.4, .2))
  coefficients <- do.call(rbind, lapply(reports, `[[`, "bhat"))
  meta <- colSums(coefficients / variances) / colSums(1 / variances)
  expect_equal(meta, c(-1.2, .68))
  expect_gt(max(abs(state$betaBar - meta)), .5)
})
