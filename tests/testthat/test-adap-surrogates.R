test_that("ADAP reduced variants are registered", {
  expect_type(FederatedLearning:::.getAlgorithm("ADAP1"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAPDiag"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP_PDA"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP2"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ODAL"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAPDiagClosedForm"), "list")
})

expect_lasso_kkt <- function(beta, grad, lambda, penalize = NULL, tol = 1e-4) {
  if (is.null(penalize)) {
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
  }
  expect_true(all(is.finite(beta)))
  expect_true(all(is.finite(grad)))
  for (j in seq_along(beta)) {
    if (!isTRUE(penalize[j])) {
      expect_lte(abs(grad[j]), tol)
    } else if (abs(beta[j]) > sqrt(tol)) {
      expect_lte(abs(grad[j] + lambda * sign(beta[j])), tol)
    } else {
      expect_lte(abs(grad[j]), lambda + tol)
    }
  }
}

make_adap_phase2_fixture <- function(n = 30L, p = 4L, seed = 100L) {
  set.seed(seed)
  x <- matrix(stats::rnorm(n * p), nrow = n)
  eta <- -0.2 + 0.5 * x[, 1] - 0.25 * x[, 2]
  y <- stats::rbinom(n, 1, stats::plogis(eta))
  xSparse <- Matrix::Matrix(x, sparse = TRUE)
  xDesign <- cbind(Matrix::Matrix(1, n, 1, sparse = TRUE), xSparse)
  betaBar <- rep(0, p + 1L)
  betaLead <- rep(0.05, p + 1L)
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, xDesign, y)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, xDesign)
  list(
    clientData = list(xMatrix = xSparse, yLabels = y, n = n),
    xDesign = xDesign,
    y = y,
    betaBar = betaBar,
    betaLead = betaLead,
    globalGrad = globalGrad,
    globalHess = globalHess,
    globalHessDiag = diag(globalHess),
    totalN = n
  )
}

test_that("logistic gradient and Hessian match finite differences", {
  set.seed(10)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(48), nrow = 12)), sparse = TRUE)
  y <- rbinom(12, 1, 0.4)
  beta <- c(-0.2, 0.1, -0.05, 0.08, 0.03)
  eps <- 1e-5

  grad <- FederatedLearning:::.logisticNegGradient(beta, x, y)
  hess <- FederatedLearning:::.logisticNegHessian(beta, x)
  fdGrad <- vapply(seq_along(beta), function(j) {
    step <- rep(0, length(beta))
    step[j] <- eps
    (
      FederatedLearning:::.negLogLikMean(beta + step, x, y) -
        FederatedLearning:::.negLogLikMean(beta - step, x, y)
    ) / (2 * eps)
  }, numeric(1))
  fdHess <- vapply(seq_along(beta), function(j) {
    step <- rep(0, length(beta))
    step[j] <- eps
    (
      FederatedLearning:::.logisticNegGradient(beta + step, x, y) -
        FederatedLearning:::.logisticNegGradient(beta - step, x, y)
    ) / (2 * eps)
  }, numeric(length(beta)))

  expect_equal(grad, fdGrad, tolerance = 1e-6)
  expect_equal(hess, fdHess, tolerance = 1e-5)
  expect_equal(diag(hess), FederatedLearning:::.logisticNegHessianDiag(beta, x), tolerance = 1e-12)
  expect_equal(hess, t(hess), tolerance = 1e-12)
})

test_that("quadratic lasso coordinate descent satisfies KKT conditions", {
  B <- matrix(
    c(
      3.0, 0.2, -0.1,
      0.2, 2.0, 0.3,
      -0.1, 0.3, 1.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  a <- c(0.4, -0.8, 0.2)
  lambda <- 0.15
  penalize <- c(FALSE, TRUE, TRUE)
  beta <- FederatedLearning:::.coordDescentQuadraticLasso(
    aTilde = a,
    B = B,
    betaInit = c(0, 0, 0),
    lambda = lambda,
    maxIter = 500,
    tol = 1e-12,
    penalize = penalize
  )
  grad <- as.numeric(a + B %*% beta)

  expect_lt(abs(grad[1]), 1e-7)
  for (j in which(penalize)) {
    if (abs(beta[j]) > 1e-7) {
      expect_lt(abs(grad[j] + lambda * sign(beta[j])), 1e-7)
    } else {
      expect_lte(abs(grad[j]), lambda + 1e-7)
    }
  }
})

test_that("ADAPDiag keeps full local curvature with diagonal remote Hessian correction", {
  set.seed(1)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(40), nrow = 10)), sparse = TRUE)
  y <- rbinom(10, 1, 0.4)
  betaEval <- c(-0.2, 0.1, 0.05, -0.1, 0.2)
  betaBar <- c(-0.1, 0.04, 0.03, -0.02, 0.08)
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y) + c(0.01, -0.02, 0.03, 0.02, -0.01)
  globalHessDiag <- FederatedLearning:::.logisticNegHessianDiag(betaBar, x) + c(0.01, 0.02, 0.03, 0.04, 0.05)
  diagOnly <- FederatedLearning:::.adapLocalFullRemoteDiagSurrogateComponents(
    betaEval = betaEval,
    betaBar = betaBar,
    xDesign = x,
    y = y,
    globalGrad = globalGrad,
    globalHessDiag = globalHessDiag
  )
  hEval <- FederatedLearning:::.logisticNegHessian(betaEval, x)
  hBarDiag <- FederatedLearning:::.logisticNegHessianDiag(betaBar, x)
  expectedB <- hEval + diag(globalHessDiag - hBarDiag, length(globalHessDiag), length(globalHessDiag))
  expectedA <- FederatedLearning:::.logisticNegGradient(betaEval, x, y) -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    FederatedLearning:::.logisticNegGradient(betaBar, x, y) -
    betaBar * (globalHessDiag - hBarDiag)

  expect_equal(diagOnly$B, expectedB, tolerance = 1e-12)
  expect_equal(diagOnly$aTilde, expectedA, tolerance = 1e-12)
})

test_that("ADAP1 surrogate is first-order local likelihood plus global gradient shift", {
  set.seed(2)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(30), nrow = 10)), sparse = TRUE)
  y <- rbinom(10, 1, 0.5)
  betaEval <- c(0.2, -0.1, 0.05, 0.1)
  betaBar <- c(0.1, -0.02, 0.01, 0.04)
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y) + c(0.03, -0.01, 0.02, -0.04)

  comp <- FederatedLearning:::.adapFirstOrderSurrogateComponents(
    betaEval = betaEval,
    betaBar = betaBar,
    xDesign = x,
    y = y,
    globalGrad = globalGrad
  )
  hEval <- FederatedLearning:::.logisticNegHessian(betaEval, x)
  expected <- FederatedLearning:::.logisticNegGradient(betaEval, x, y) -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    FederatedLearning:::.logisticNegGradient(betaBar, x, y)

  expect_equal(comp$aTilde, expected, tolerance = 1e-12)
  expect_equal(comp$B, hEval, tolerance = 1e-12)
})

test_that("full ADAP surrogate components use full Hessian correction", {
  set.seed(3)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(40), nrow = 10)), sparse = TRUE)
  y <- rbinom(10, 1, 0.5)
  betaEval <- c(0.1, -0.05, 0.03, 0.08, -0.02)
  betaBar <- c(0.05, -0.02, 0.01, 0.03, -0.01)
  hBar <- FederatedLearning:::.logisticNegHessian(betaBar, x)
  hEval <- FederatedLearning:::.logisticNegHessian(betaEval, x)
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y) +
    c(0.01, -0.02, 0.015, 0.005, -0.01)
  globalHess <- hBar + diag(c(0.04, 0.03, 0.02, 0.01, 0.05), 5)

  comp <- FederatedLearning:::.adapSurrogateComponents(
    betaEval = betaEval,
    betaBar = betaBar,
    xDesign = x,
    y = y,
    globalGrad = globalGrad,
    globalHess = globalHess
  )
  expectedB <- hEval + globalHess - hBar
  expectedA <- FederatedLearning:::.logisticNegGradient(betaEval, x, y) -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    FederatedLearning:::.logisticNegGradient(betaBar, x, y) -
    as.numeric(t(betaBar) %*% (globalHess - hBar))

  expect_equal(comp$B, expectedB, tolerance = 1e-12)
  expect_equal(comp$aTilde, expectedA, tolerance = 1e-12)
})

test_that("lambda grids are finite, positive, and ordered from large to small", {
  set.seed(4)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(40), nrow = 10)), sparse = TRUE)
  y <- rbinom(10, 1, 0.5)
  betaLead <- rep(0.05, ncol(x))
  betaBar <- rep(0.02, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, x)
  globalHessDiag <- diag(globalHess)

  grids <- list(
    FederatedLearning:::.pdaAdapLambdaSeq(x, y, betaLead, betaBar, globalGrad, globalHess, gridLen = 8),
    FederatedLearning:::.pdaAdapFirstLambdaSeq(x, y, betaLead, betaBar, globalGrad, gridLen = 8),
    FederatedLearning:::.pdaAdapDiagLambdaSeq(x, y, betaLead, betaBar, globalGrad, globalHessDiag, gridLen = 8),
    FederatedLearning:::.leadLambdaRange(x, y, betaBar, globalGrad, globalHess, intercept = TRUE)$lambdaSeq
  )

  for (lambdaSeq in grids) {
    expect_true(all(is.finite(lambdaSeq)))
    expect_true(all(lambdaSeq > 0))
    expect_true(all(diff(lambdaSeq) <= 0))
  }
})

test_that("ADAP and ODAL phase aggregation uses sample-size weights and configured lead site", {
  b1 <- c(0.1, 0.2, -0.1)
  b2 <- c(0.3, -0.2, 0.4)
  ns <- c(10, 30)
  expectedBeta <- as.numeric(cbind(b1, b2) %*% (ns / sum(ns)))

  adapState <- FederatedLearning:::.serverInitPdaAdap(list(p = 2, lambda = 0.1))
  adapRound0 <- FederatedLearning:::.serverRoundPdaAdap(
    adapState,
    list(list(bhat = b1, n = ns[1]), list(bhat = b2, n = ns[2])),
    list(leadIndex = 1)
  )
  odalState <- FederatedLearning:::.serverInitODAL(list(p = 2))
  odalRound0 <- FederatedLearning:::.serverRoundODAL(
    odalState,
    list(list(bhat = b1, n = ns[1]), list(bhat = b2, n = ns[2])),
    list(leadIndex = 1)
  )

  expect_equal(adapRound0$state$betaBar, expectedBeta)
  expect_equal(adapRound0$state$betaLead, b1)
  expect_equal(adapRound0$state$leadIndex, 1)
  expect_equal(odalRound0$state$betaBar, expectedBeta)
  expect_equal(odalRound0$state$leadIndex, 1)

  g1 <- c(0.1, 0.0, -0.2)
  g2 <- c(0.3, 0.2, 0.4)
  H1 <- diag(c(1, 2, 3))
  H2 <- matrix(c(2, 0.1, 0, 0.1, 4, 0.2, 0, 0.2, 6), 3)
  expectedGrad <- as.numeric(cbind(g1, g2) %*% (ns / sum(ns)))
  expectedHess <- H1 * ns[1] / sum(ns) + H2 * ns[2] / sum(ns)

  adapRound1 <- FederatedLearning:::.serverRoundPdaAdap(
    adapRound0$state,
    list(list(grad = g1, Hess = H1, n = ns[1]), list(grad = g2, Hess = H2, n = ns[2])),
    list()
  )
  odalRound1 <- FederatedLearning:::.serverRoundODAL(
    odalRound0$state,
    list(list(grad = g1, Hess = H1, n = ns[1]), list(grad = g2, Hess = H2, n = ns[2])),
    list()
  )

  expect_equal(adapRound1$state$globalGrad, expectedGrad)
  expect_equal(adapRound1$state$globalHess, expectedHess)
  expect_equal(odalRound1$state$otherGrad, expectedGrad)
  expect_equal(odalRound1$state$otherHess, expectedHess)
})

test_that("ADAP reduced phase aggregation distinguishes first-order and diagonal modes", {
  ns <- c(5, 15)
  b1 <- c(0.1, 0.0, 0.2)
  b2 <- c(0.3, 0.4, -0.1)
  g1 <- c(0.1, -0.2, 0.3)
  g2 <- c(0.2, 0.1, -0.1)
  h1 <- c(1, 2, 3)
  h2 <- c(4, 5, 6)
  weights <- ns / sum(ns)

  firstState <- FederatedLearning:::.serverInitPdaAdap1(list(p = 2, lambda = 0.1))
  first0 <- FederatedLearning:::.serverRoundPdaAdapReduced(
    firstState,
    list(list(bhat = b1, n = ns[1]), list(bhat = b2, n = ns[2])),
    list()
  )
  first1 <- FederatedLearning:::.serverRoundPdaAdapReduced(
    first0$state,
    list(list(grad = g1, n = ns[1]), list(grad = g2, n = ns[2])),
    list()
  )
  diagState <- FederatedLearning:::.serverInitPdaAdapDiag(list(p = 2, lambda = 0.1))
  diag0 <- FederatedLearning:::.serverRoundPdaAdapReduced(
    diagState,
    list(list(bhat = b1, n = ns[1]), list(bhat = b2, n = ns[2])),
    list()
  )
  diag1 <- FederatedLearning:::.serverRoundPdaAdapReduced(
    diag0$state,
    list(list(grad = g1, HessDiag = h1, n = ns[1]), list(grad = g2, HessDiag = h2, n = ns[2])),
    list()
  )

  expect_equal(first1$state$globalGrad, as.numeric(cbind(g1, g2) %*% weights))
  expect_null(first1$state$globalHessDiag)
  expect_equal(first1$report$communicationNumbers, length(g1) * 2)
  expect_equal(diag1$state$globalGrad, as.numeric(cbind(g1, g2) %*% weights))
  expect_equal(diag1$state$globalHessDiag, as.numeric(cbind(h1, h2) %*% weights))
  expect_equal(diag1$report$communicationNumbers, length(g1) * 2 + length(h1) * 2)
})

test_that("ADAP_PDA uses pda-style lambda range and proximal update", {
  set.seed(11)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(60), nrow = 15)), sparse = TRUE)
  y <- rbinom(15, 1, 0.45)
  betaBar <- rep(0, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y) + c(0.01, -0.02, 0.03, -0.01, 0.02)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, x) + diag(rep(0.01, ncol(x)))

  lambdaSeq <- FederatedLearning:::.pdaAdapPdaLambdaSeq(
    globalGrad = globalGrad,
    nLead = nrow(x),
    p = ncol(x),
    gridLen = 6L
  )
  expect_equal(lambdaSeq[1], max(abs(globalGrad[-1])), tolerance = 1e-12)
  expect_true(all(diff(lambdaSeq) < 0))

  fit <- FederatedLearning:::.fitPdaAdapPdaProx(
    xDesign = x,
    y = y,
    beta0 = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambda = lambdaSeq[3],
    useFull = TRUE,
    maxIter = 50L,
    tol = 1e-8
  )
  expect_length(fit, ncol(x))
  expect_true(all(is.finite(fit)))
})

test_that("ADAPDiag can switch from local-full diagonal correction to pda diagonal solving", {
  set.seed(12)
  x <- Matrix::Matrix(matrix(rnorm(120), nrow = 24), sparse = TRUE)
  y <- rbinom(24, 1, stats::plogis(0.2 + 0.4 * x[, 1] - 0.2 * x[, 2]))
  clientData <- list(xMatrix = x, yLabels = y, n = length(y))
  betaBar <- rep(0, ncol(x) + 1L)
  xDesign <- cbind(1, x)
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, xDesign, y)
  globalHessDiag <- FederatedLearning:::.logisticNegHessianDiag(betaBar, xDesign)
  broadcast <- list(
    phase = 2L,
    adapReducedMode = "diag",
    leadIndex = 1L,
    betaBar = betaBar,
    betaLead = rep(0.1, length(betaBar)),
    globalGrad = globalGrad,
    globalHessDiag = globalHessDiag,
    totalN = length(y),
    lambdaSeq = c(0.01)
  )

  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 1L)
  localFull <- FederatedLearning:::.clientUpdatePdaAdapReduced(
    clientData,
    broadcast,
    list(intercept = FALSE, lambda = 0.01, maxOuter = 3L, maxInner = 20L, foldsK = 3L)
  )
  pdaDiag <- FederatedLearning:::.clientUpdatePdaAdapReduced(
    clientData,
    modifyList(broadcast, list(adapDiagStyle = "pda")),
    list(intercept = FALSE, lambda = 0.01, maxIter = 20L, foldsK = 3L)
  )

  expect_length(localFull$w, length(betaBar))
  expect_length(pdaDiag$w, length(betaBar))
  expect_true(all(is.finite(localFull$w)))
  expect_true(all(is.finite(pdaDiag$w)))
  expect_equal(localFull$selectedLambda, 0.01)
  expect_equal(localFull$lambdaSeq, 0.01)
  expect_true(is.na(localFull$cvScores))
  expect_equal(pdaDiag$selectedLambda, 0.01)
  expect_equal(pdaDiag$lambdaSeq, 0.01)
  expect_true(is.na(pdaDiag$cvScores))
  expect_false(isTRUE(all.equal(localFull$w, pdaDiag$w, tolerance = 1e-10)))
})

test_that("fixed ADAP lambda bypasses lead-site cross-validation in all ADAP styles", {
  fixture <- make_adap_phase2_fixture(seed = 15L)
  fixedLambda <- 0.03
  baseBroadcast <- list(
    phase = 2L,
    leadIndex = 1L,
    betaBar = fixture$betaBar,
    betaLead = fixture$betaLead,
    globalGrad = fixture$globalGrad,
    globalHess = fixture$globalHess,
    globalHessDiag = fixture$globalHessDiag,
    totalN = fixture$totalN,
    lambdaSeq = c(0.2, 0.1, 0.05)
  )
  config <- list(
    intercept = FALSE,
    lambda = fixedLambda,
    foldsK = 3L,
    maxIter = 25L,
    maxOuter = 3L,
    maxInner = 25L,
    tol = 1e-6
  )
  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 1L)

  updates <- list(
    full = FederatedLearning:::.clientUpdatePdaAdap(fixture$clientData, baseBroadcast, config),
    pda = FederatedLearning:::.clientUpdatePdaAdap(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapSolveStyle = "pda")),
      config
    ),
    first = FederatedLearning:::.clientUpdatePdaAdapReduced(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapReducedMode = "first")),
      config
    ),
    diag = FederatedLearning:::.clientUpdatePdaAdapReduced(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapReducedMode = "diag")),
      config
    ),
    diagPda = FederatedLearning:::.clientUpdatePdaAdapReduced(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapReducedMode = "diag", adapDiagStyle = "pda")),
      config
    )
  )

  for (update in updates) {
    expect_equal(update$selectedLambda, fixedLambda)
    expect_equal(update$lambdaSeq, fixedLambda)
    expect_true(is.na(update$cvScores))
    expect_length(update$w, length(fixture$betaBar))
    expect_true(all(is.finite(update$w)))
  }
})

test_that("lead-site ADAP variants select lambdas from configured CV path", {
  fixture <- make_adap_phase2_fixture(seed = 16L)
  lambdaSeq <- c(0.12, 0.04)
  baseBroadcast <- list(
    phase = 2L,
    leadIndex = 1L,
    betaBar = fixture$betaBar,
    betaLead = fixture$betaLead,
    globalGrad = fixture$globalGrad,
    globalHess = fixture$globalHess,
    globalHessDiag = fixture$globalHessDiag,
    totalN = fixture$totalN,
    lambdaSeq = lambdaSeq
  )
  config <- list(
    intercept = FALSE,
    foldsK = 3L,
    cvSeed = 44L,
    maxIter = 30L,
    maxOuter = 4L,
    maxInner = 30L,
    tol = 1e-6
  )
  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 1L)

  updates <- list(
    full = FederatedLearning:::.clientUpdatePdaAdap(fixture$clientData, baseBroadcast, config),
    first = FederatedLearning:::.clientUpdatePdaAdapReduced(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapReducedMode = "first")),
      config
    ),
    diag = FederatedLearning:::.clientUpdatePdaAdapReduced(
      fixture$clientData,
      modifyList(baseBroadcast, list(adapReducedMode = "diag")),
      config
    )
  )

  for (update in updates) {
    expect_true(update$selectedLambda %in% lambdaSeq)
    expect_equal(update$lambdaSeq, lambdaSeq)
    expect_length(update$cvScores, length(lambdaSeq))
    expect_true(all(is.finite(update$cvScores)))
    expect_length(update$w, length(fixture$betaBar))
    expect_true(all(is.finite(update$w)))
  }
})

test_that("cached ADAP surrogate terms match uncached calculations", {
  fixture <- make_adap_phase2_fixture(seed = 17L)
  betaEval <- fixture$betaLead
  betaBar <- fixture$betaBar
  gradBar <- FederatedLearning:::.logisticNegGradient(betaBar, fixture$xDesign, fixture$y)
  hBar <- FederatedLearning:::.logisticNegHessian(betaBar, fixture$xDesign)
  hBarDiag <- diag(hBar)

  fullUncached <- FederatedLearning:::.adapSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHess
  )
  fullCached <- FederatedLearning:::.adapSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHess,
    gradBar = gradBar,
    hBar = hBar
  )
  firstUncached <- FederatedLearning:::.adapFirstOrderSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad
  )
  firstCached <- FederatedLearning:::.adapFirstOrderSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad,
    gradBar = gradBar
  )
  diagUncached <- FederatedLearning:::.adapDiagSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHessDiag
  )
  diagCached <- FederatedLearning:::.adapDiagSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHessDiag,
    gradBar = gradBar,
    hBarDiag = hBarDiag
  )
  localFullUncached <- FederatedLearning:::.adapLocalFullRemoteDiagSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHessDiag
  )
  localFullCached <- FederatedLearning:::.adapLocalFullRemoteDiagSurrogateComponents(
    betaEval, betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHessDiag,
    gradBar = gradBar,
    hBarDiag = hBarDiag
  )

  expect_equal(fullCached, fullUncached, tolerance = 1e-12)
  expect_equal(firstCached, firstUncached, tolerance = 1e-12)
  expect_equal(diagCached, diagUncached, tolerance = 1e-12)
  expect_equal(localFullCached, localFullUncached, tolerance = 1e-12)
})

test_that("ADAP warm starts do not change CV scores after convergence", {
  fixture <- make_adap_phase2_fixture(n = 24L, p = 3L, seed = 18L)
  lambdaSeq <- c(0.08, 0.03)
  foldsK <- 3L
  seed <- 19L
  maxOuter <- 30L
  maxInner <- 80L
  tol <- 1e-8

  set.seed(seed)
  folds <- sample(rep_len(seq_len(foldsK), length(fixture$y)))
  manualScores <- vapply(lambdaSeq, function(lambda) {
    foldLoss <- vapply(seq_len(foldsK), function(fold) {
      idxVal <- which(folds == fold)
      nVal <- length(idxVal)
      idxTr <- which(folds != fold)
      gradVal <- FederatedLearning:::.logisticNegGradient(
        fixture$betaBar,
        fixture$xDesign[idxVal, , drop = FALSE],
        fixture$y[idxVal]
      )
      hessVal <- FederatedLearning:::.logisticNegHessian(
        fixture$betaBar,
        fixture$xDesign[idxVal, , drop = FALSE]
      )
      denom <- max(fixture$totalN - nVal, 1L)
      fit <- FederatedLearning:::.fitPdaAdapSurrogate(
        xDesign = fixture$xDesign[idxTr, , drop = FALSE],
        y = fixture$y[idxTr],
        betaLead = fixture$betaLead,
        betaBar = fixture$betaBar,
        globalGrad = (fixture$globalGrad * fixture$totalN - gradVal * nVal) / denom,
        globalHess = (fixture$globalHess * fixture$totalN - hessVal * nVal) / denom,
        lambda = lambda,
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol,
        betaInit = fixture$betaLead
      )
      FederatedLearning:::.negLogLikMean(fit, fixture$xDesign[idxVal, , drop = FALSE], fixture$y[idxVal])
    }, numeric(1))
    mean(foldLoss)
  }, numeric(1))
  warm <- FederatedLearning:::.pdaAdapLeadCv(
    xDesign = fixture$xDesign,
    y = fixture$y,
    betaLead = fixture$betaLead,
    betaBar = fixture$betaBar,
    globalGrad = fixture$globalGrad,
    globalHess = fixture$globalHess,
    lambdaSeq = lambdaSeq,
    totalN = fixture$totalN,
    foldsK = foldsK,
    seed = seed,
    maxOuter = maxOuter,
    maxInner = maxInner,
    tol = tol
  )

  expect_equal(warm$scores, manualScores, tolerance = 1e-6)
  expect_equal(warm$lambda, lambdaSeq[which.min(manualScores)])
})

test_that("ADAP optimizers satisfy quadratic KKT checks on well-conditioned data", {
  fixture <- make_adap_phase2_fixture(n = 34L, p = 4L, seed = 20L)
  lambda <- 0.04
  penalize <- rep(TRUE, length(fixture$betaBar))
  penalize[1] <- FALSE

  fits <- list(
    full = FederatedLearning:::.fitPdaAdapSurrogate(
      fixture$xDesign, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHess, lambda,
      maxOuter = 60L, maxInner = 120L, tol = 1e-9
    ),
    first = FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
      fixture$xDesign, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, lambda,
      maxOuter = 60L, maxInner = 120L, tol = 1e-9
    ),
    diag = FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
      fixture$xDesign, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHessDiag, lambda,
      maxOuter = 60L, maxInner = 120L, tol = 1e-9
    )
  )

  fullComp <- FederatedLearning:::.adapSurrogateComponents(
    fits$full, fixture$betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHess
  )
  firstComp <- FederatedLearning:::.adapFirstOrderSurrogateComponents(
    fits$first, fixture$betaBar, fixture$xDesign, fixture$y, fixture$globalGrad
  )
  diagComp <- FederatedLearning:::.adapLocalFullRemoteDiagSurrogateComponents(
    fits$diag, fixture$betaBar, fixture$xDesign, fixture$y, fixture$globalGrad, fixture$globalHessDiag
  )

  expect_lasso_kkt(fits$full, as.numeric(fullComp$aTilde + fullComp$B %*% fits$full), lambda, penalize, tol = 5e-4)
  expect_lasso_kkt(fits$first, as.numeric(firstComp$aTilde + firstComp$B %*% fits$first), lambda, penalize, tol = 5e-4)
  expect_lasso_kkt(fits$diag, as.numeric(diagComp$aTilde + diagComp$B %*% fits$diag), lambda, penalize, tol = 5e-4)
})

test_that("ADAP optimizers stay finite on rare and near-separated data", {
  set.seed(20)
  n1 <- 25L
  n2 <- 7L
  y1 <- c(rep(0L, n1 - 1L), 1L)
  y2 <- c(rep(0L, n2 - 2L), 1L, 1L)
  make_x <- function(y) {
    cbind(
      sep = ifelse(y == 1L, 8, -8) + stats::rnorm(length(y), sd = 0.05),
      zeros = 0,
      ones = 1,
      large = stats::rnorm(length(y), sd = 50),
      noise = stats::rnorm(length(y))
    )
  }
  x1 <- Matrix::Matrix(cbind(1, make_x(y1)), sparse = TRUE)
  x2 <- Matrix::Matrix(cbind(1, make_x(y2)), sparse = TRUE)
  betaBar <- rep(0, ncol(x1))
  betaLead <- rep(0.01, ncol(x1))
  terms1 <- FederatedLearning:::.logisticNegGradientHessian(betaBar, x1, y1)
  terms2 <- FederatedLearning:::.logisticNegGradientHessian(betaBar, x2, y2)
  weights <- c(n1, n2) / (n1 + n2)
  globalGrad <- terms1$gradient * weights[1] + terms2$gradient * weights[2]
  globalHess <- terms1$hessian * weights[1] + terms2$hessian * weights[2]
  globalHessDiag <- diag(globalHess)
  lambda <- 0.05

  fits <- list(
    full = FederatedLearning:::.fitPdaAdapSurrogate(
      x1, y1, betaLead, betaBar, globalGrad, globalHess, lambda,
      maxOuter = 40L, maxInner = 100L, tol = 1e-8
    ),
    first = FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
      x1, y1, betaLead, betaBar, globalGrad, lambda,
      maxOuter = 40L, maxInner = 100L, tol = 1e-8
    ),
    diag = FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
      x1, y1, betaLead, betaBar, globalGrad, globalHessDiag, lambda,
      maxOuter = 40L, maxInner = 100L, tol = 1e-8
    ),
    pda = FederatedLearning:::.fitPdaAdapPdaProx(
      x1, y1, betaBar, globalGrad, globalHess, lambda,
      useFull = TRUE, maxIter = 100L, tol = 1e-8
    )
  )
  expect_true(all(vapply(fits, function(beta) all(is.finite(beta)), logical(1))))
})

test_that("PDA-style ADAP methods complete simulated multi-site workflows", {
  set.seed(13)
  makeClient <- function(n, shift) {
    x <- matrix(rnorm(n * 4), nrow = n)
    eta <- shift + 0.5 * x[, 1] - 0.35 * x[, 2] + 0.15 * x[, 3]
    list(
      xMatrix = Matrix::Matrix(x, sparse = TRUE),
      yLabels = rbinom(n, 1, stats::plogis(eta)),
      n = n
    )
  }
  clients <- list(makeClient(35, -0.2), makeClient(32, 0.1), makeClient(30, 0.3))

  runMethod <- function(method, extraConfig = list()) {
    algo <- FederatedLearning:::.getAlgorithm(method)
    config <- utils::modifyList(
      list(
        p = 4L,
        intercept = FALSE,
        leadIndex = 1L,
        lambda = 0.02,
        lambdaSeq = c(0.05, 0.02),
        foldsK = 3L,
        cvSeed = 14L,
        maxIter = 60L,
        maxOuter = 5L,
        maxInner = 60L,
        tol = 1e-6
      ),
      extraConfig
    )
    state <- algo$serverInit(config)
    reports <- NULL
    serverReport <- list()
    old <- getOption("FederatedLearning.localId")
    on.exit(options(FederatedLearning.localId = old), add = TRUE)
    for (round in seq_len(4L)) {
      reports <- lapply(seq_along(clients), function(i) {
        options(FederatedLearning.localId = i)
        algo$clientUpdate(clients[[i]], state, config)
      })
      srv <- algo$serverRound(state, reports, config)
      state <- srv$state
      serverReport <- srv$report
      if (isTRUE(serverReport$done)) {
        break
      }
    }
    list(state = state, report = serverReport)
  }

  for (method in c("ADAP_PDA", "ADAP", "ADAPDiag", "ADAP1")) {
    out <- runMethod(method)
    expect_true(isTRUE(out$report$done))
    expect_length(out$report$w, 5L)
    expect_true(all(is.finite(out$report$w)))
    expect_equal(out$report$leadIndex, 1L)
    expect_true(is.finite(out$report$communicationNumbers))
  }

  diagPda <- runMethod("ADAPDiag", list(adapDiagStyle = "pda"))
  expect_true(isTRUE(diagPda$report$done))
  expect_length(diagPda$report$w, 5L)
  expect_true(all(is.finite(diagPda$report$w)))
})

test_that("ODAL lead-site client update only solves on selected lead site", {
  x <- Matrix::Matrix(cbind(1, c(-1, -0.5, 0.5, 1)), sparse = TRUE)
  y <- c(0L, 0L, 1L, 1L)
  clientData <- list(xMatrix = x, yLabels = y, n = length(y))
  broadcast <- list(
    phase = 2L,
    leadIndex = 1L,
    betaBar = c(0, 1),
    otherGrad = c(0, 0),
    otherHess = diag(c(0.1, 0.1))
  )

  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 2L)
  expect_null(FederatedLearning:::.clientUpdateODAL(clientData, broadcast, list(intercept = TRUE)))
  options(FederatedLearning.localId = 1L)
  out <- FederatedLearning:::.clientUpdateODAL(
    clientData,
    broadcast,
    list(intercept = TRUE, optimMaxit = 50L)
  )

  expect_type(out$w, "double")
  expect_length(out$w, 2)
  expect_true(is.finite(out$value))
})

test_that("ADAP2 guards full Hessian size, reuses cache, and aggregates diagonal summaries", {
  expect_error(
    FederatedLearning:::.serverInitADAP2(list(p = 4, intercept = TRUE, hessian = "full", maxFullHessianP = 4)),
    "full Hessian requested"
  )

  key <- "unit-test-adap2-cache"
  if (exists(key, envir = FederatedLearning:::.adap2CacheEnv, inherits = FALSE)) {
    rm(list = key, envir = FederatedLearning:::.adap2CacheEnv)
  }
  entry <- list(
    p = 3L,
    leadIndex = 2L,
    beta0 = c(0.1, 0.2, 0.3),
    Gother = c(0.01, -0.02, 0.03),
    Hother = c(1, 2, 3),
    hessian = "diag",
    foldsK = 4L
  )
  assign(key, entry, envir = FederatedLearning:::.adap2CacheEnv)
  cached <- FederatedLearning:::.serverInitADAP2(list(
    p = 2,
    intercept = TRUE,
    hessian = "diag",
    cacheKey = key,
    request = "fit"
  ))
  expect_equal(cached$phase, 2L)
  expect_equal(cached$leadIndex, 2L)
  expect_equal(cached$Gother, entry$Gother)
  rm(list = key, envir = FederatedLearning:::.adap2CacheEnv)

  state <- FederatedLearning:::.serverInitADAP2(list(p = 2, intercept = TRUE, hessian = "diag"))
  round0 <- FederatedLearning:::.serverRoundADAP2(
    state,
    list(list(bhat = c(0, 1, 2), n = 10), list(bhat = c(1, 2, 3), n = 30)),
    list()
  )
  g1 <- c(0.1, 0.2, 0.3)
  g2 <- c(0.2, 0.1, -0.1)
  h1 <- c(1, 2, 3)
  h2 <- c(2, 4, 6)
  round1 <- FederatedLearning:::.serverRoundADAP2(
    round0$state,
    list(list(grad = g1, Hdiag = h1, n = 10), list(grad = g2, Hdiag = h2, n = 30)),
    list(request = "fit")
  )
  weights <- c(10, 30) / 40
  gsum <- as.numeric(cbind(g1, g2) %*% weights)
  hsum <- as.numeric(cbind(h1, h2) %*% weights)
  lead <- round0$state$leadIndex

  expect_equal(round1$state$Gother, gsum - cbind(g1, g2)[, lead])
  expect_equal(round1$state$Hother, hsum - cbind(h1, h2)[, lead])
  expect_equal(round1$state$hessianDim, "3")
})
