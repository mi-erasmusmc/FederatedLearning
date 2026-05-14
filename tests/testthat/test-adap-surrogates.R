test_that("ADAP reduced variants are registered", {
  expect_type(FederatedLearning:::.getAlgorithm("ADAP1"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAPDiag"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP2"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("Prox-ADAP"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("C-ADAP"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ADAP_PDA"), "list")
  expect_type(FederatedLearning:::.getAlgorithm("ODAL"), "list")
})

test_that("ADAP_PDA uses the public PDA full-quadratic coordinate descent solver", {
  state <- FederatedLearning:::.serverInitPdaAdapPda(list(p = 3L))
  expect_equal(state$adapSolveStyle, "fullQuadratic")
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

test_that("compiled quadratic coordinate descent matches the R fallback", {
  set.seed(41)
  z <- matrix(rnorm(25), nrow = 5)
  B <- crossprod(z) / nrow(z) + diag(0.1, 5)
  a <- rnorm(5)
  betaInit <- rnorm(5)
  penalize <- c(FALSE, TRUE, TRUE, FALSE, TRUE)

  expected <- FederatedLearning:::.coordDescentQuadraticLasso(
    aTilde = a,
    B = Matrix::Matrix(B, sparse = FALSE),
    betaInit = betaInit,
    lambda = 0.07,
    maxIter = 80L,
    tol = 1e-11,
    penalize = penalize
  )
  actual <- FederatedLearning:::.coordDescentQuadraticLasso(
    aTilde = a,
    B = B,
    betaInit = betaInit,
    lambda = 0.07,
    maxIter = 80L,
    tol = 1e-11,
    penalize = penalize
  )

  expect_equal(actual, expected, tolerance = 1e-12)
})

test_that("quadratic coordinate descent fails clearly on invalid curvature", {
  B <- diag(c(1, -0.1, 2))
  a <- c(0.1, -0.2, 0.3)
  betaInit <- c(0, 0, 0)
  penalize <- c(FALSE, TRUE, TRUE)

  expect_error(
    FederatedLearning:::.coordDescentQuadraticLasso(
      aTilde = a,
      B = B,
      betaInit = betaInit,
      lambda = 0.1,
      penalize = penalize
    ),
    "non_positive_coordinate_curvature"
  )
  details <- FederatedLearning:::.coordDescentQuadraticLasso(
    aTilde = a,
    B = Matrix::Matrix(B, sparse = FALSE),
    betaInit = betaInit,
    lambda = 0.1,
    penalize = penalize,
    returnDetails = TRUE
  )
  expect_false(details$converged)
  expect_equal(details$failureReason, "non_positive_coordinate_curvature")
  expect_equal(details$beta, betaInit)
})

test_that("quadratic coordinate descent handles zero penalized curvature deliberately", {
  betaInit <- c(0, 0, 0)
  penalize <- c(FALSE, TRUE, TRUE)

  bounded <- FederatedLearning:::.coordDescentQuadraticLasso(
    aTilde = c(0.1, 0.05, 0.3),
    B = diag(c(1, 0, 2)),
    betaInit = betaInit,
    lambda = 0.1,
    penalize = penalize
  )
  expect_true(all(is.finite(bounded)))
  expect_equal(bounded[2], 0)

  expect_error(
    FederatedLearning:::.coordDescentQuadraticLasso(
      aTilde = c(0.1, -0.2, 0.3),
      B = diag(c(1, 0, 2)),
      betaInit = betaInit,
      lambda = 0.1,
      penalize = penalize
    ),
    "zero_coordinate_curvature_unbounded"
  )
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

test_that("ADAP2 surrogate matches global derivatives at expansion point", {
  fixture <- make_adap_phase2_fixture(n = 36L, p = 4L, seed = 301L)
  comp <- FederatedLearning:::.adapSurrogateComponents(
    betaEval = fixture$betaBar,
    betaBar = fixture$betaBar,
    xDesign = fixture$xDesign,
    y = fixture$y,
    globalGrad = fixture$globalGrad,
    globalHess = fixture$globalHess
  )
  gradAtBar <- as.numeric(comp$aTilde + comp$B %*% fixture$betaBar)
  expect_equal(gradAtBar, fixture$globalGrad, tolerance = 1e-12)
  expect_equal(comp$B, fixture$globalHess, tolerance = 1e-12)
})

test_that("C-ADAP surrogate matches global derivatives and has PSD remote curvature", {
  lead <- make_adap_phase2_fixture(n = 31L, p = 4L, seed = 302L)
  remote <- make_adap_phase2_fixture(n = 27L, p = 4L, seed = 303L)
  betaBar <- rep(0.03, length(lead$betaBar))
  leadTerms <- FederatedLearning:::.logisticNegGradientHessian(betaBar, lead$xDesign, lead$y)
  remoteTerms <- FederatedLearning:::.logisticNegGradientHessian(betaBar, remote$xDesign, remote$y)
  weights <- c(length(lead$y), length(remote$y)) / (length(lead$y) + length(remote$y))
  globalGrad <- weights[1] * leadTerms$gradient + weights[2] * remoteTerms$gradient
  globalHess <- weights[1] * leadTerms$hessian + weights[2] * remoteTerms$hessian

  comp <- FederatedLearning:::.adapSurrogateComponents(
    betaEval = betaBar,
    betaBar = betaBar,
    xDesign = lead$xDesign,
    y = lead$y,
    globalGrad = globalGrad,
    globalHess = globalHess,
    gradBar = leadTerms$gradient,
    hBar = leadTerms$hessian,
    leadWeight = weights[1]
  )
  gradAtBar <- as.numeric(comp$aTilde + comp$B %*% betaBar)
  remoteCorrection <- globalHess - weights[1] * leadTerms$hessian

  expect_equal(gradAtBar, globalGrad, tolerance = 1e-12)
  expect_equal(comp$B, globalHess, tolerance = 1e-12)
  expect_gte(FederatedLearning:::.adapEigenRange(remoteCorrection)$min, -1e-10)

  betaCurrent <- betaBar + seq_along(betaBar) * 0.01
  currentLead <- FederatedLearning:::.logisticNegHessian(betaCurrent, lead$xDesign)
  currentB <- weights[1] * currentLead + remoteCorrection
  expect_gte(FederatedLearning:::.adapEigenRange(currentB)$min, -1e-10)
})

test_that("Prox-ADAP spectral shift makes exact ADAP correction PSD", {
  C <- diag(c(-0.2, 0.05, 0.3))
  H <- diag(c(0.4, 0.2, 0.1))
  shift <- FederatedLearning:::.adapProxShift(H, C, tau = 1e-8)
  shifted <- C + diag(shift$rho, nrow(C), ncol(C))

  expect_gt(shift$rho, 0)
  expect_gte(FederatedLearning:::.adapEigenRange(shifted)$min, shift$epsilonFloor - 1e-12)
  expect_gte(FederatedLearning:::.adapEigenRange(H + shifted)$min, shift$epsilonFloor - 1e-12)
})

test_that("exact ADAP2 fails before solving when Hessian correction has negative curvature", {
  fixture <- make_adap_phase2_fixture(n = 30L, p = 4L, seed = 304L)
  badGlobalHess <- fixture$globalHess - diag(0.25, nrow(fixture$globalHess))
  fit <- FederatedLearning:::.fitPdaAdapSurrogate(
    fixture$xDesign,
    fixture$y,
    fixture$betaLead,
    fixture$betaBar,
    fixture$globalGrad,
    badGlobalHess,
    lambda = 0.02,
    returnDetails = TRUE
  )

  expect_true(FederatedLearning:::.adapFitFailed(fit))
  expect_equal(FederatedLearning:::.adapFitFailureReason(fit), "negative_C_eigenvalue")
  expect_lt(fit$C_eigen_min, 0)
})

test_that("exact ADAPDiag fails before solving when diagonal correction is negative", {
  fixture <- make_adap_phase2_fixture(n = 30L, p = 4L, seed = 305L)
  hBarDiag <- FederatedLearning:::.logisticNegHessianDiag(fixture$betaBar, fixture$xDesign)
  badGlobalHessDiag <- hBarDiag
  badGlobalHessDiag[2] <- hBarDiag[2] - 0.1
  fit <- FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
    fixture$xDesign,
    fixture$y,
    fixture$betaLead,
    fixture$betaBar,
    fixture$globalGrad,
    badGlobalHessDiag,
    lambda = 0.02,
    returnDetails = TRUE
  )

  expect_true(FederatedLearning:::.adapFitFailed(fit))
  expect_equal(FederatedLearning:::.adapFitFailureReason(fit), "negative_diagonal_correction")
  expect_lt(fit$correction_diag_min, 0)
})

test_that("ADAP1 max-iteration non-convergence is reported as failure", {
  fixture <- make_adap_phase2_fixture(n = 30L, p = 4L, seed = 306L)
  fit <- FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
    fixture$xDesign,
    fixture$y,
    fixture$betaLead,
    fixture$betaBar,
    fixture$globalGrad,
    lambda = 0.001,
    maxOuter = 1L,
    maxInner = 1L,
    tol = 0,
    returnDetails = TRUE
  )

  expect_true(FederatedLearning:::.adapFitFailed(fit))
  expect_equal(FederatedLearning:::.adapFitFailureReason(fit), "max_outer_no_convergence")
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
    FederatedLearning:::.pdaAdapDiagLambdaSeq(x, y, betaLead, betaBar, globalGrad, globalHessDiag, gridLen = 8)
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

test_that("ODAL local initialization uses explicit ridge fallback for singular coefficients", {
  skip_if_not_installed("glmnet")

  x <- Matrix::Matrix(
    cbind(
      1,
      c(0, 0, 1, 1, 0, 1),
      c(0, 0, 1, 1, 0, 1)
    ),
    sparse = TRUE
  )
  y <- c(0, 0, 1, 1, 0, 1)

  expect_error(
    FederatedLearning:::.clientUpdateODAL(
      clientData = list(xMatrix = x, yLabels = y, n = length(y)),
      serverBroadcast = list(phase = 0L),
      config = list(intercept = TRUE, standardize = FALSE)
    ),
    "glm returned non-finite"
  )

  report <- suppressWarnings(
    FederatedLearning:::.clientUpdateODAL(
      clientData = list(xMatrix = x, yLabels = y, n = length(y)),
      serverBroadcast = list(phase = 0L),
      config = list(intercept = TRUE, standardize = FALSE, odalInit = "ridgeFallback")
    )
  )

  expect_length(report$bhat, ncol(x))
  expect_true(all(is.finite(report$bhat)))
  expect_equal(report$n, length(y))
})

test_that("AUC lambda tie-break prefers stronger regularization", {
  lambdaSeq <- c(1e-2, 1e-4, 1e-6)
  idx <- FederatedLearning:::.adapBestLambdaIndex(
    scores = c(0.6, 0.6, 0.6),
    lambdaSeq = lambdaSeq,
    metric = "auc",
    tieTolerance = 1e-8
  )
  expect_equal(lambdaSeq[idx], 1e-2)
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

test_that("ADAP_PDA public solver uses full quadratic lambda range and coordinate descent", {
  set.seed(11)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(60), nrow = 15)), sparse = TRUE)
  y <- rbinom(15, 1, 0.45)
  betaBar <- rep(0, ncol(x))
  betaLead <- rep(0.05, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y) + c(0.01, -0.02, 0.03, -0.01, 0.02)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, x) + diag(rep(0.01, ncol(x)))

  lambdaSeq <- FederatedLearning:::.pdaAdapLambdaSeq(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    gridLen = 6L
  )
  expect_true(all(diff(lambdaSeq) < 0))

  fit <- FederatedLearning:::.fitPdaAdapSurrogate(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambda = lambdaSeq[3],
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-8
  )
  expect_length(fit, ncol(x))
  expect_true(all(is.finite(fit)))
})

test_that("ADAP full-quadratic lead CV supports bounded log-lambda search", {
  set.seed(13)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(160), nrow = 40)), sparse = TRUE)
  y <- rbinom(40, 1, stats::plogis(0.1 + 0.6 * x[, 2] - 0.4 * x[, 3]))
  betaBar <- rep(0, ncol(x))
  betaLead <- rep(0, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, x)
  lambdaSeq <- FederatedLearning:::.pdaAdapLambdaSeq(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    gridLen = 20L
  )

  cv <- FederatedLearning:::.pdaAdapLeadCv(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambdaSeq = lambdaSeq,
    totalN = length(y),
    foldsK = 3L,
    seed = 13L,
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-6,
    search = "optimize",
    searchTol = log(2)
  )

  expect_true(is.finite(cv$lambda))
  expect_true(cv$lambda >= min(lambdaSeq))
  expect_true(cv$lambda <= max(lambdaSeq))
  expect_true(length(cv$scores) < length(lambdaSeq))
  expect_equal(length(cv$scores), length(cv$lambdaSeq))
  expect_true(all(is.finite(cv$scores)))
})

test_that("ADAP1 and ADAPDiag lead CV support bounded log-lambda search", {
  set.seed(14)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(200), nrow = 40)), sparse = TRUE)
  y <- rbinom(40, 1, stats::plogis(-0.1 + 0.5 * x[, 2] - 0.3 * x[, 4]))
  betaBar <- rep(0, ncol(x))
  betaLead <- rep(0.01, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y)
  globalHessDiag <- FederatedLearning:::.logisticNegHessianDiag(betaBar, x)

  firstLambdaSeq <- FederatedLearning:::.pdaAdapFirstLambdaSeq(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    gridLen = 20L
  )
  diagLambdaSeq <- FederatedLearning:::.pdaAdapDiagLambdaSeq(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHessDiag = globalHessDiag,
    gridLen = 20L
  )

  firstCv <- FederatedLearning:::.pdaAdapFirstLeadCv(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    lambdaSeq = firstLambdaSeq,
    totalN = length(y),
    foldsK = 3L,
    seed = 14L,
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-6,
    search = "optimize",
    searchTol = log(2),
    maxEvals = 8L
  )
  diagCv <- FederatedLearning:::.pdaAdapDiagLeadCv(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHessDiag = globalHessDiag,
    lambdaSeq = diagLambdaSeq,
    totalN = length(y),
    foldsK = 3L,
    seed = 14L,
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-6,
    search = "optimize",
    searchTol = log(2),
    maxEvals = 8L
  )

  for (cv in list(firstCv, diagCv)) {
    expect_true(is.finite(cv$lambda))
    expect_true(length(cv$scores) < 20L)
    expect_equal(length(cv$scores), length(cv$lambdaSeq))
    expect_true(all(is.finite(cv$scores)))
  }
  expect_true(firstCv$lambda >= min(firstLambdaSeq))
  expect_true(firstCv$lambda <= max(firstLambdaSeq))
  expect_true(diagCv$lambda >= min(diagLambdaSeq))
  expect_true(diagCv$lambda <= max(diagLambdaSeq))
})

test_that("ADAP lead CV evaluates lambda paths from strongest regularization", {
  x <- Matrix::Matrix(cbind(1, seq(-1, 1, length.out = 12)), sparse = TRUE)
  y <- rep(c(0, 1), 6)
  lambdaSeq <- c(0.01, 0.1, 0.001)
  calls <- numeric()

  cv <- FederatedLearning:::.pdaAdapSurrogateLeadCv(
    xDesign = x,
    y = y,
    betaInit = c(0, 0),
    lambdaSeq = lambdaSeq,
    foldsK = 1L,
    seed = 1L,
    search = "grid",
    selectionMetric = "deviance",
    makeFoldInfo = function(info) list(),
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
      calls <<- c(calls, lambda)
      list(beta = c(0, 0), converged = TRUE, failureReason = "")
    }
  )

  expect_equal(calls, sort(lambdaSeq, decreasing = TRUE))
  expect_equal(cv$lambdaSeq, sort(lambdaSeq, decreasing = TRUE))
})

test_that("ADAP auto search does not warm-start from failed weaker fits", {
  x <- Matrix::Matrix(cbind(1, seq(-1, 1, length.out = 12)), sparse = TRUE)
  y <- rep(c(0, 1), 6)
  lambdaSeq <- c(0.1, 1e-6)
  calls <- list()

  cv <- FederatedLearning:::.pdaAdapSurrogateLeadCv(
    xDesign = x,
    y = y,
    betaInit = c(0, 0),
    lambdaSeq = lambdaSeq,
    foldsK = 1L,
    seed = 2L,
    search = "optimize",
    searchTol = log(1.01),
    maxEvals = 4L,
    selectionMetric = "deviance",
    makeFoldInfo = function(info) list(),
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
      calls[[length(calls) + 1L]] <<- list(lambda = lambda, warmStart = warmStart)
      if (lambda < 1e-5) {
        return(list(beta = c(999, 999), converged = FALSE, failureReason = "beta_abs_too_large"))
      }
      list(beta = c(lambda, -lambda), converged = TRUE, failureReason = "")
    }
  )

  tried <- vapply(calls, `[[`, numeric(1), "lambda")
  starts <- lapply(calls, `[[`, "warmStart")
  expect_equal(tried[1], max(lambdaSeq), tolerance = 1e-12)
  expect_equal(starts[[1]], c(0, 0))
  expect_true(any(tried < 1e-5))
  for (i in seq_along(calls)[tried > 1e-5 & seq_along(calls) > 1L]) {
    expect_false(any(starts[[i]] == 999))
  }
  expect_true(all(cv$valid[cv$lambdaSeq > 1e-5]))
})

test_that("ADAP auto search uses strong-to-weak quadratic proposals", {
  x <- Matrix::Matrix(matrix(1, nrow = 12, ncol = 1), sparse = TRUE)
  y <- rep(c(0, 1), 6)
  lambdaSeq <- c(0.1, 0.01, 0.001, 1e-6)
  calls <- numeric()
  target <- log(0.004)

  cv <- FederatedLearning:::.pdaAdapSurrogateLeadCv(
    xDesign = x,
    y = y,
    betaInit = 0,
    lambdaSeq = lambdaSeq,
    foldsK = 1L,
    seed = 3L,
    search = "optimize",
    searchTol = log(1.01),
    maxEvals = 4L,
    selectionMetric = "deviance",
    makeFoldInfo = function(info) list(),
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
      calls <<- c(calls, lambda)
      score <- 1e-4 + (log(lambda) - target)^2
      beta <- 2 * acosh(exp(score))
      list(beta = beta, converged = TRUE, failureReason = "")
    }
  )

  expect_equal(calls[1], 0.1, tolerance = 1e-12)
  expect_equal(calls[2], 0.01, tolerance = 1e-12)
  expect_equal(calls[3], 0.001, tolerance = 1e-12)
  expect_false(any(calls[seq_len(min(3L, length(calls)))] == min(lambdaSeq)))
  expect_equal(unname(calls[4]), 0.004, tolerance = 1e-8)
  expect_equal(unname(cv$lambda), 0.004, tolerance = 1e-8)
})

test_that("ADAP auto search records successful scalar lambdas as valid", {
  x <- Matrix::Matrix(cbind(1, seq(-1, 1, length.out = 20)), sparse = TRUE)
  y <- rep(c(0, 1), 10)

  cv <- FederatedLearning:::.pdaAdapSurrogateLeadCv(
    xDesign = x,
    y = y,
    betaInit = c(0, 0),
    lambdaSeq = c(0.02, 0.002, 0.0002),
    foldsK = 5L,
    seed = 1L,
    search = "optimize",
    maxEvals = 3L,
    selectionMetric = "deviance",
    collectDiagnostics = TRUE,
    makeFoldInfo = function(info) list(),
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
      list(beta = c(0, 0), converged = TRUE, failureReason = "")
    }
  )

  expect_equal(cv$valid, rep(TRUE, length(cv$valid)))
})

test_that("ADAP lead CV can select lambda by AUC", {
  set.seed(21)
  n <- 120L
  age <- stats::rnorm(n)
  sex <- stats::rbinom(n, 1, 0.5)
  y <- stats::rbinom(n, 1, stats::plogis(-3.2 + 1.4 * age + 0.8 * sex))
  x <- Matrix::Matrix(cbind(1, age, sex), sparse = TRUE)
  betaBar <- rep(0, ncol(x))
  betaLead <- rep(0.01, ncol(x))
  globalGrad <- FederatedLearning:::.logisticNegGradient(betaBar, x, y)
  globalHess <- FederatedLearning:::.logisticNegHessian(betaBar, x)
  globalHessDiag <- FederatedLearning:::.logisticNegHessianDiag(betaBar, x)
  lambdaSeq <- c(1e-3, 1e-4, 1e-6)

  firstCv <- FederatedLearning:::.pdaAdapFirstLeadCv(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    lambdaSeq = lambdaSeq,
    totalN = length(y),
    foldsK = 3L,
    seed = 22L,
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-7,
    search = "grid",
    selectionMetric = "auc",
    globalAdjustment = "leaveValOut"
  )
  diagCv <- FederatedLearning:::.pdaAdapDiagLeadCv(
    xDesign = x,
    y = y,
    betaLead = betaLead,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHessDiag = globalHessDiag,
    lambdaSeq = lambdaSeq,
    totalN = length(y),
    foldsK = 3L,
    seed = 22L,
    maxOuter = 80L,
    maxInner = 100L,
    tol = 1e-7,
    search = "grid",
    selectionMetric = "auc",
    globalAdjustment = "leaveValOut"
  )
  pdaCv <- FederatedLearning:::.pdaAdapPdaLeadCv(
    xDesign = x,
    y = y,
    betaBar = betaBar,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambdaSeq = lambdaSeq,
    foldsK = 3L,
    seed = 22L,
    maxIter = 60L,
    tol = 1e-7,
    selectionMetric = "auc"
  )

  expect_true(firstCv$lambda %in% lambdaSeq)
  expect_true(diagCv$lambda %in% lambdaSeq)
  expect_true(pdaCv$lambda %in% lambdaSeq)
  expect_equal(
    firstCv$scores[match(firstCv$lambda, lambdaSeq)],
    max(firstCv$scores[firstCv$valid], na.rm = TRUE)
  )
  expect_equal(
    diagCv$scores[match(diagCv$lambda, lambdaSeq)],
    max(diagCv$scores[diagCv$valid], na.rm = TRUE)
  )
  expect_equal(pdaCv$scores[match(pdaCv$lambda, lambdaSeq)], max(pdaCv$scores, na.rm = TRUE))
  expect_gt(max(firstCv$scores[firstCv$valid], na.rm = TRUE), 0.5)
  expect_gt(max(diagCv$scores[diagCv$valid], na.rm = TRUE), 0.5)
  expect_gt(max(pdaCv$scores, na.rm = TRUE), 0.5)
})

test_that("ADAP lead CV can use a stratified row cap for large lead sites", {
  y <- c(rep(0L, 95), rep(1L, 5))
  idx <- FederatedLearning:::.adapCvSubset(y, maxRows = 20L, seed = 23L)

  expect_length(idx, 20L)
  expect_true(any(y[idx] == 1L))
  expect_true(any(y[idx] == 0L))
  expect_equal(idx, sort(idx))
  expect_equal(FederatedLearning:::.adapCvSubset(y, maxRows = Inf, seed = 23L), seq_along(y))
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
    list(intercept = FALSE, lambda = 0.01, maxOuter = 80L, maxInner = 100L, foldsK = 3L)
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
    maxOuter = 80L,
    maxInner = 100L,
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
    maxOuter = 80L,
    maxInner = 100L,
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
    tol = tol,
    globalAdjustment = "leaveValOut"
  )

  expect_equal(warm$scores, manualScores, tolerance = 1e-6)
  expect_equal(warm$lambda, lambdaSeq[which.min(manualScores)])
})

test_that("ADAP CV diagnostics capture fold-level solver and curvature details", {
  fixture <- make_adap_phase2_fixture(n = 30L, p = 3L, seed = 20L)
  cv <- FederatedLearning:::.pdaAdapLeadCv(
    xDesign = fixture$xDesign,
    y = fixture$y,
    betaLead = fixture$betaLead,
    betaBar = fixture$betaBar,
    globalGrad = fixture$globalGrad,
    globalHess = fixture$globalHess,
    lambdaSeq = c(0.1, 0.01),
    totalN = fixture$totalN,
    foldsK = 3L,
    search = "grid",
    collectDiagnostics = TRUE
  )

  expect_s3_class(cv$diagnostics, "data.frame")
  expect_equal(nrow(cv$diagnostics), 6L)
  expect_true(all(c(
    "lambda", "innerFold", "score", "rawDeviance", "outerIterations",
    "converged", "betaMaxAbs", "etaNonFinite", "BEigenMin",
    "BEigenNonPositive", "failureReason", "fitFailed",
    "innerObjective", "innerMaxAbsStep", "innerBacktracks",
    "failingCoordinate", "coordinateCurvature", "coordinateGradient",
    "failureDiagMin", "failureDiagMax", "failureDiagNonPositive"
  ) %in% names(cv$diagnostics)))
  expect_true(all(is.finite(cv$diagnostics$score)))
})

test_that("ADAP CV diagnostics report solver failure reasons", {
  fixture <- make_adap_phase2_fixture(n = 24L, p = 3L, seed = 22L)
  badHess <- fixture$globalHess - diag(10, ncol(fixture$globalHess))
  expect_error(
    FederatedLearning:::.pdaAdapLeadCv(
      xDesign = fixture$xDesign,
      y = fixture$y,
      betaLead = fixture$betaLead,
      betaBar = fixture$betaBar,
      globalGrad = fixture$globalGrad,
      globalHess = badHess,
      lambdaSeq = c(0.1),
      totalN = fixture$totalN,
      foldsK = 3L,
      search = "grid",
      collectDiagnostics = TRUE
    ),
    "lambda CV failed: no candidate lambda had successful inner fits"
  )
})

test_that("ADAP CV trace diagnostics are collected for full, diagonal, and first-order surrogates", {
  fixture <- make_adap_phase2_fixture(n = 30L, p = 3L, seed = 23L)
  traceFiles <- stats::setNames(
    file.path(tempdir(), paste0("adap-trace-", c("full", "diag", "first"), ".rds")),
    c("full", "diag", "first")
  )
  unlink(traceFiles)

  cvs <- list(
    full = FederatedLearning:::.pdaAdapLeadCv(
      xDesign = fixture$xDesign,
      y = fixture$y,
      betaLead = fixture$betaLead,
      betaBar = fixture$betaBar,
      globalGrad = fixture$globalGrad,
      globalHess = fixture$globalHess,
      lambdaSeq = c(0.1, 0.01),
      totalN = fixture$totalN,
      foldsK = 3L,
      search = "grid",
      collectTrace = TRUE,
      traceContext = list(method = "ADAP", task = "unit"),
      traceFile = traceFiles[["full"]]
    ),
    diag = FederatedLearning:::.pdaAdapDiagLeadCv(
      xDesign = fixture$xDesign,
      y = fixture$y,
      betaLead = fixture$betaLead,
      betaBar = fixture$betaBar,
      globalGrad = fixture$globalGrad,
      globalHessDiag = fixture$globalHessDiag,
      lambdaSeq = c(0.1, 0.01),
      totalN = fixture$totalN,
      foldsK = 3L,
      search = "grid",
      collectTrace = TRUE,
      traceContext = list(method = "ADAPDiag", task = "unit"),
      traceFile = traceFiles[["diag"]]
    ),
    first = FederatedLearning:::.pdaAdapFirstLeadCv(
      xDesign = fixture$xDesign,
      y = fixture$y,
      betaLead = fixture$betaLead,
      betaBar = fixture$betaBar,
      globalGrad = fixture$globalGrad,
      lambdaSeq = c(0.1, 0.01),
      totalN = fixture$totalN,
      foldsK = 3L,
      search = "grid",
      collectTrace = TRUE,
      traceContext = list(method = "ADAP1", task = "unit"),
      traceFile = traceFiles[["first"]]
    )
  )

  for (kind in names(cvs)) {
    expect_true(file.exists(traceFiles[[kind]]))
    diskTrace <- readRDS(traceFiles[[kind]])
    expect_false(isTRUE(diskTrace$failed))
    expect_equal(diskTrace$surrogateKind, kind)
    expect_equal(cvs[[kind]]$trace$surrogateKind, kind)
    expect_gt(length(diskTrace$fits), 0L)
    expect_s3_class(diskTrace$fits[[1]]$trace$rows, "data.frame")
    expect_gt(nrow(diskTrace$fits[[1]]$trace$rows), 0L)
    expect_gt(length(diskTrace$fits[[1]]$trace$snapshots), 0L)
    expect_true(all(c(
      "outerIteration", "failureReason", "betaAfterMaxAbs", "deltaMaxAbs",
      "etaAfterMaxAbs", "kktAfterMax", "BEigenMin", "BKappa",
      "innerIterations", "innerConverged", "failingCoordinate"
    ) %in% names(diskTrace$fits[[1]]$trace$rows)))
    expect_true(all(c("aTilde", "B", "betaBefore", "betaAfter", "fixed", "kktAfter") %in%
      names(diskTrace$fits[[1]]$trace$snapshots[[1]])))
  }
})

test_that("ADAP CV trace diagnostics are written when all candidate lambdas fail", {
  fixture <- make_adap_phase2_fixture(n = 24L, p = 3L, seed = 24L)
  badHess <- fixture$globalHess - diag(10, ncol(fixture$globalHess))
  traceFile <- tempfile(fileext = ".rds")

  expect_error(
    FederatedLearning:::.pdaAdapLeadCv(
      xDesign = fixture$xDesign,
      y = fixture$y,
      betaLead = fixture$betaLead,
      betaBar = fixture$betaBar,
      globalGrad = fixture$globalGrad,
      globalHess = badHess,
      lambdaSeq = c(0.1),
      totalN = fixture$totalN,
      foldsK = 3L,
      search = "grid",
      collectDiagnostics = TRUE,
      collectTrace = TRUE,
      traceContext = list(method = "ADAP", task = "unit"),
      traceFile = traceFile
    ),
    "lambda CV failed: no candidate lambda had successful inner fits"
  )
  expect_true(file.exists(traceFile))
  trace <- readRDS(traceFile)
  expect_true(isTRUE(trace$failed))
  expect_true(any(nzchar(trace$diagnostics$failureReason)))
  expect_equal(unique(trace$diagnostics$failureReason), "negative_C_eigenvalue")
})

test_that("compiled sparse ADAP surrogate solvers match dense R fallback", {
  fixture <- make_adap_phase2_fixture(n = 24L, p = 4L, seed = 21L)
  xSparse <- fixture$xDesign
  xDense <- as.matrix(xSparse)
  lambda <- 0.025

  sparseFits <- list(
    full = FederatedLearning:::.fitPdaAdapSurrogate(
      xSparse, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHess, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    ),
    first = FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
      xSparse, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    ),
    diag = FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
      xSparse, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHessDiag, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    )
  )

  denseFits <- list(
    full = FederatedLearning:::.fitPdaAdapSurrogate(
      xDense, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHess, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    ),
    first = FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
      xDense, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    ),
    diag = FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
      xDense, fixture$y, fixture$betaLead, fixture$betaBar,
      fixture$globalGrad, fixture$globalHessDiag, lambda,
      maxOuter = 35L, maxInner = 80L, tol = 1e-9
    )
  )

  expect_equal(sparseFits$full, denseFits$full, tolerance = 1e-8)
  expect_equal(sparseFits$first, denseFits$first, tolerance = 1e-8)
  expect_equal(sparseFits$diag, denseFits$diag, tolerance = 1e-8)
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

  exactFull <- FederatedLearning:::.fitPdaAdapSurrogate(
    x1, y1, betaLead, betaBar, globalGrad, globalHess, lambda,
    maxOuter = 40L, maxInner = 100L, tol = 1e-8, returnDetails = TRUE
  )
  expect_true(FederatedLearning:::.adapFitFailed(exactFull))
  firstExact <- FederatedLearning:::.fitPdaAdapFirstOrderSurrogate(
    x1, y1, betaLead, betaBar, globalGrad, lambda,
    maxOuter = 40L, maxInner = 100L, tol = 1e-8, returnDetails = TRUE
  )
  expect_true(FederatedLearning:::.adapFitFailed(firstExact))
  diagExact <- FederatedLearning:::.fitPdaAdapRemoteDiagSurrogate(
    x1, y1, betaLead, betaBar, globalGrad, globalHessDiag, lambda,
    maxOuter = 40L, maxInner = 100L, tol = 1e-8, returnDetails = TRUE
  )
  expect_true(FederatedLearning:::.adapFitFailed(diagExact))

  fits <- list(
    prox = FederatedLearning:::.fitPdaAdapSurrogate(
      x1, y1, betaLead, betaBar, globalGrad, globalHess, lambda,
      maxOuter = 80L, maxInner = 100L, tol = 1e-8,
      strictCorrection = "prox"
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
        maxOuter = 80L,
        maxInner = 100L,
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

  for (method in c("ADAP_PDA", "ADAP", "ADAP1", "Prox-ADAP", "C-ADAP")) {
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
