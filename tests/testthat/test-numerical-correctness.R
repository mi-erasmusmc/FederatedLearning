test_that("binary logistic objective is stable for extreme predictors", {
  eta <- c(-1000, -100, 0, 100, 1000)
  y <- c(0, 1, 0, 1, 0)

  loss <- FederatedLearning:::binaryLogLoss(eta, y, meanLoss = FALSE)

  expect_true(is.finite(loss))
  expect_equal(loss, 1100 + log(2), tolerance = 1e-10)
})

test_that("logistic objective names have explicit sign conventions", {
  x <- Matrix::Matrix(cbind(1, c(-1, 0, 2)), sparse = TRUE)
  y <- c(0, 1, 1)
  beta <- c(0.2, -0.5)
  eta <- as.numeric(x %*% beta)
  expectedNegLogLik <- sum(log1p(exp(eta)) - y * eta)

  expect_equal(
    FederatedLearning:::logisticNegLogLik(beta, x, y, meanLoss = FALSE),
    expectedNegLogLik,
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::logisticNegLogLik(beta, x, y, meanLoss = TRUE),
    expectedNegLogLik / length(y),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::logisticLoss(beta, x, y),
    -expectedNegLogLik,
    tolerance = 1e-12
  )
})

test_that("logistic gradient matches finite differences of stable mean objective", {
  set.seed(501)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(40), nrow = 10)), sparse = TRUE)
  y <- rbinom(10, 1, 0.4)
  beta <- c(0.2, -0.3, 0.1, 0.05, -0.15)
  eps <- 1e-6

  fdGrad <- vapply(seq_along(beta), function(j) {
    step <- rep(0, length(beta))
    step[[j]] <- eps
    (
      FederatedLearning:::logisticNegLogLik(beta + step, x, y, meanLoss = TRUE) -
        FederatedLearning:::logisticNegLogLik(beta - step, x, y, meanLoss = TRUE)
    ) / (2 * eps)
  }, numeric(1))

  expect_equal(
    FederatedLearning::gradLogistic(beta, x, y),
    fdGrad,
    tolerance = 1e-6
  )
})

test_that("gradient remains finite and bounded under extreme logits", {
  x <- Matrix::Matrix(diag(c(1000, 100, 1, 100, 1000)), sparse = TRUE)
  beta <- rep(1, 5)
  y <- c(0, 0, 1, 1, 1)

  grad <- FederatedLearning::gradLogistic(beta, x, y)
  gradCpp <- FederatedLearning:::logisticGradientCpp(methods::as(x, "dgCMatrix"), beta, y)

  expect_true(all(is.finite(grad)))
  expect_equal(gradCpp, grad, tolerance = 1e-12)
  expect_true(all(abs(grad) <= Matrix::colMeans(abs(x)) + 1e-12))
})

test_that("DualAvg C++ client update stays finite under extreme logits", {
  x <- Matrix::Matrix(diag(c(1000, 100, 1, 100, 1000)), sparse = TRUE)
  y <- c(0, 0, 1, 1, 1)
  clientData <- list(xMatrix = methods::as(x, "dgCMatrix"), yLabels = y, n = length(y))
  serverBroadcast <- list(z = rep(1, ncol(x)), r = 0L)
  config <- list(
    k = 2L,
    etaClient = 0.1,
    etaServer = 1,
    lambda = 0,
    intercept = FALSE
  )

  cppUpdate <- FederatedLearning::clientUpdateDualAveragingCpp(
    clientData,
    serverBroadcast,
    config
  )
  rUpdate <- FederatedLearning::clientUpdateDA(
    clientData,
    serverBroadcast,
    config
  )

  expect_true(all(is.finite(cppUpdate$delta)))
  expect_equal(cppUpdate, rUpdate, tolerance = 1e-12)
})

test_that("Hessian summaries remain finite under extreme logits", {
  x <- Matrix::Matrix(
    c(
      1, -1000, 0,
      1, -100, 1,
      1, 0, 2,
      1, 100, 3,
      1, 1000, 4
    ),
    ncol = 3,
    byrow = TRUE,
    sparse = TRUE
  )
  beta <- c(0, 1, -0.5)

  hess <- FederatedLearning:::.logisticNegHessian(beta, x)
  hessDiag <- FederatedLearning:::.logisticNegHessianDiag(beta, x)
  xDgC <- methods::as(x, "dgCMatrix")
  hessCpp <- FederatedLearning:::logisticHessianCpp(xDgC, beta)
  hessDiagCpp <- FederatedLearning:::logisticHessianDiagCpp(xDgC, beta)
  combinedFull <- FederatedLearning:::logisticGradientHessianCpp(xDgC, beta, c(0, 0, 1, 1, 1))
  combinedDiag <- FederatedLearning:::logisticGradientHessianDiagCpp(xDgC, beta, c(0, 0, 1, 1, 1))

  expect_true(all(is.finite(hess)))
  expect_true(all(is.finite(hessDiag)))
  expect_true(all(is.finite(hessCpp)))
  expect_true(all(is.finite(hessDiagCpp)))
  expect_equal(hessCpp, hess, tolerance = 1e-12)
  expect_equal(hessDiagCpp, hessDiag, tolerance = 1e-12)
  expect_equal(combinedFull$hessian, hess, tolerance = 1e-12)
  expect_equal(combinedDiag$hessianDiag, hessDiag, tolerance = 1e-12)
  expect_equal(diag(hess), hessDiag, tolerance = 1e-12)
  expect_true(all(hessDiag >= 0))
  expect_true(all(diag(hess)[[1]] <= 0.25 + 1e-12))
})

test_that("split-client negative log-likelihood equals centralized negative log-likelihood", {
  set.seed(502)
  x1 <- Matrix::Matrix(cbind(1, matrix(rnorm(30), nrow = 10)), sparse = TRUE)
  x2 <- Matrix::Matrix(cbind(1, matrix(rnorm(75), nrow = 25)), sparse = TRUE)
  y1 <- rbinom(10, 1, 0.3)
  y2 <- rbinom(25, 1, 0.6)
  beta <- c(-0.1, 0.2, -0.05, 0.03)

  centralized <- FederatedLearning:::logisticNegLogLik(
    beta,
    rbind(x1, x2),
    c(y1, y2),
    meanLoss = FALSE
  )
  splitTotal <- FederatedLearning:::logisticNegLogLik(beta, x1, y1, meanLoss = FALSE) +
    FederatedLearning:::logisticNegLogLik(beta, x2, y2, meanLoss = FALSE)
  weightedMean <- (
    length(y1) * FederatedLearning:::logisticNegLogLik(beta, x1, y1, meanLoss = TRUE) +
      length(y2) * FederatedLearning:::logisticNegLogLik(beta, x2, y2, meanLoss = TRUE)
  ) / (length(y1) + length(y2))

  expect_equal(splitTotal, centralized, tolerance = 1e-12)
  expect_equal(
    weightedMean,
    FederatedLearning:::logisticNegLogLik(beta, rbind(x1, x2), c(y1, y2), meanLoss = TRUE),
    tolerance = 1e-12
  )
})

test_that("mean gradients decompose by client sample-size weighting", {
  set.seed(503)
  x1 <- Matrix::Matrix(cbind(1, matrix(rnorm(24), nrow = 8)), sparse = TRUE)
  x2 <- Matrix::Matrix(cbind(1, matrix(rnorm(63), nrow = 21)), sparse = TRUE)
  y1 <- rbinom(8, 1, 0.25)
  y2 <- rbinom(21, 1, 0.65)
  beta <- c(0.05, -0.15, 0.2, -0.1)

  centralGrad <- FederatedLearning::gradLogistic(beta, rbind(x1, x2), c(y1, y2))
  weightedGrad <- (
    length(y1) * FederatedLearning::gradLogistic(beta, x1, y1) +
      length(y2) * FederatedLearning::gradLogistic(beta, x2, y2)
  ) / (length(y1) + length(y2))

  expect_equal(weightedGrad, centralGrad, tolerance = 1e-12)
})

test_that("orchestrator objective helper uses non-negative negative log-likelihood scale", {
  x <- Matrix::Matrix(cbind(1, c(-4, -1, 0.5, 2)), sparse = TRUE)
  y <- c(0, 0, 1, 1)
  beta <- c(0, 4)

  nll <- FederatedLearning:::logisticNegLogLik(beta, x, y, meanLoss = FALSE)
  oldLogLik <- FederatedLearning:::logisticLoss(beta, x, y)

  expect_true(nll >= 0)
  expect_equal(oldLogLik, -nll, tolerance = 1e-12)
})

test_that("dense and sparse logistic computations agree", {
  xDense <- cbind(
    1,
    matrix(
      c(
        -2, 0, 1,
        -1, 1, 0,
        0, 0, 0,
        1, 1, 1,
        2, 0, 3
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
  xSparse <- Matrix::Matrix(xDense, sparse = TRUE)
  y <- c(0, 0, 1, 1, 1)
  beta <- c(0.1, -0.2, 0.3, 0.05)

  expect_equal(
    FederatedLearning:::logisticNegLogLik(beta, xSparse, y, meanLoss = FALSE),
    FederatedLearning:::logisticNegLogLik(beta, xDense, y, meanLoss = FALSE),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning::gradLogistic(beta, xSparse, y),
    FederatedLearning::gradLogistic(beta, xDense, y),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::.logisticNegHessianDiag(beta, xSparse),
    FederatedLearning:::.logisticNegHessianDiag(beta, xDense),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::.logisticNegHessian(beta, xSparse),
    FederatedLearning:::.logisticNegHessian(beta, xDense),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::logisticGradientCpp(xSparse, beta, y),
    FederatedLearning::gradLogistic(beta, xDense, y),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::logisticHessianCpp(xSparse, beta),
    FederatedLearning:::.logisticNegHessian(beta, xDense),
    tolerance = 1e-12
  )
  combinedFull <- FederatedLearning:::logisticGradientHessianCpp(xSparse, beta, y)
  combinedDiag <- FederatedLearning:::logisticGradientHessianDiagCpp(xSparse, beta, y)
  expect_equal(
    combinedFull$gradient,
    FederatedLearning::gradLogistic(beta, xDense, y),
    tolerance = 1e-12
  )
  expect_equal(
    combinedFull$hessian,
    FederatedLearning:::.logisticNegHessian(beta, xDense),
    tolerance = 1e-12
  )
  expect_equal(
    combinedDiag$gradient,
    FederatedLearning::gradLogistic(beta, xDense, y),
    tolerance = 1e-12
  )
  expect_equal(
    combinedDiag$hessianDiag,
    FederatedLearning:::.logisticNegHessianDiag(beta, xDense),
    tolerance = 1e-12
  )
  sparseMoments <- FederatedLearning:::.clientMatrixMoments(xSparse, intercept = TRUE)
  denseMoments <- FederatedLearning:::.clientMatrixMoments(xDense, intercept = TRUE)
  expect_equal(sparseMoments$xMeans, denseMoments$xMeans, tolerance = 1e-12)
  expect_equal(sparseMoments$x2Means, denseMoments$x2Means, tolerance = 1e-12)
})

test_that("client matrix moments use E[X squared] for sparse binary and continuous features", {
  continuousMatrix <- Matrix::Matrix(
    c(
      1, 0, 2,
      1, 3, 0,
      1, 0, 4,
      1, 1, 0
    ),
    nrow = 4,
    byrow = TRUE,
    sparse = TRUE
  )
  continuousMoments <- FederatedLearning:::.clientMatrixMoments(continuousMatrix, intercept = TRUE)

  expect_false(isTRUE(all.equal(continuousMoments$x2Means, continuousMoments$xMeans^2)))
  expect_equal(continuousMoments$xMeans, c(0, 1, 1.5))
  expect_equal(continuousMoments$x2Means, c(0, 2.5, 5))

  binaryMatrix <- Matrix::Matrix(
    c(
      1, 0, 1,
      1, 1, 0,
      1, 0, 0,
      1, 1, 1
    ),
    nrow = 4,
    byrow = TRUE,
    sparse = TRUE
  )
  binaryMoments <- FederatedLearning:::.clientMatrixMoments(binaryMatrix, intercept = TRUE)

  expect_equal(binaryMoments$xMeans, c(0, 0.5, 0.5))
  expect_equal(binaryMoments$x2Means, c(0, 0.5, 0.5))
})

test_that("intercept moments are zeroed only when an intercept column is configured", {
  xMatrix <- Matrix::Matrix(
    c(
      1, 2,
      1, 4,
      1, 6
    ),
    nrow = 3,
    byrow = TRUE,
    sparse = TRUE
  )

  withIntercept <- FederatedLearning:::.clientMatrixMoments(xMatrix, intercept = TRUE)
  withoutIntercept <- FederatedLearning:::.clientMatrixMoments(xMatrix, intercept = FALSE)

  expect_equal(withIntercept$xMeans, c(0, 4))
  expect_equal(withIntercept$x2Means, c(0, 56 / 3))
  expect_equal(withoutIntercept$xMeans, c(1, 4))
  expect_equal(withoutIntercept$x2Means, c(1, 56 / 3))
})

test_that("variance from client moments is finite and clipped at zero", {
  xMatrix <- Matrix::Matrix(
    c(
      1, 0, 1, 1, 1e6,
      1, 0, 1, 1 + 1e-8, 1e6 + 100,
      1, 0, 1, 1 - 1e-8, 1e6 - 100,
      1, 0, 1, 1, 1e6 + 200
    ),
    nrow = 4,
    byrow = TRUE,
    sparse = TRUE
  )
  moments <- FederatedLearning:::.clientMatrixMoments(xMatrix, intercept = TRUE)
  variance <- FederatedLearning:::.clientMatrixVariance(moments$xMeans, moments$x2Means)
  sd <- sqrt(variance)

  expect_true(all(is.finite(variance)))
  expect_true(all(is.finite(sd)))
  expect_true(all(variance >= 0))
  expect_equal(variance[[1]], 0)
  expect_equal(variance[[2]], 0)
  expect_equal(variance[[3]], 0)
  expect_gt(variance[[5]], 0)

  expect_equal(
    FederatedLearning:::.clientMatrixVariance(c(1), c(1 - 1e-14)),
    0
  )
})
