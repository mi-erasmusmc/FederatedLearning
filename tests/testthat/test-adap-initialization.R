test_that("ADAP initialization retains an unpenalized intercept on both matrix paths", {
  set.seed(103)
  x <- matrix(rnorm(120), nrow = 40)
  y <- rbinom(40, 1, stats::plogis(0.2 + 0.5 * x[, 1]))
  for (dense in c(TRUE, FALSE)) {
    set.seed(104)
    fit <- .fitPdaStyleLocalLasso(Matrix::Matrix(x, sparse = TRUE),
      Matrix::Matrix(cbind(1, x), sparse = TRUE), y,
      list(maxDenseInitCells = if (dense) Inf else 0))
    expect_identical(fit$usedDensePdaPath, dense)
    expect_true(all(fit$beta[-1] == 0))
    expect_equal(fit$beta[[1]], stats::qlogis(mean(y)), tolerance = 1e-7)
    expect_gt(abs(fit$beta[[1]]), 0.5)
  }
})

test_that("dense and sparse ADAP initialization use the same glmnet specification", {
  set.seed(211)
  x <- sweep(matrix(rnorm(600), nrow = 200), 2, c(0.2, 1, 5), "*")
  y <- rbinom(200, 1, stats::plogis(-1.2 + 5 * x[, 1] - x[, 2]))
  for (standardize in c(FALSE, TRUE)) {
    reference <- glmnet::cv.glmnet(x, y, family = "binomial", intercept = TRUE,
      standardize = standardize, foldid = .adapLocalInitFoldId(length(y), 212L))
    for (dense in c(TRUE, FALSE)) {
      fit <- .fitPdaStyleLocalLasso(Matrix::Matrix(x, sparse = TRUE),
        Matrix::Matrix(cbind(1, x), sparse = TRUE), y,
        list(standardize = standardize, maxDenseInitCells = if (dense) Inf else 0, cvSeed = 212L))
      expect_equal(fit$lambda, reference$lambda.min, tolerance = 1e-8)
      expect_equal(fit$beta, as.numeric(stats::coef(reference, s = "lambda.min")), tolerance = 1e-6)
      expect_length(fit$beta, ncol(x) + 1L)
      probability <- stats::plogis(as.numeric(cbind(1, x) %*% fit$beta))
      expect_lt(abs(mean(probability - y)), 1e-6)
    }
  }
})

test_that("all ADAP variants share initialization without duplicating the intercept", {
  set.seed(311)
  x <- Matrix::Matrix(matrix(rnorm(240), nrow = 80), sparse = TRUE)
  y <- rep(c(0, 0, 0, 1), 20)
  set.seed(312)
  expected <- .fitPdaStyleLocalLasso(x, .addInterceptColumn(x), y, list())
  for (intercept in c(FALSE, TRUE)) {
    client <- list(xMatrix = if (intercept) .addInterceptColumn(x) else x, yLabels = y, n = length(y))
    for (method in c("ADAP", "ADAP1", "ADAP2", "ADAPDiag", "ADAP_PDA", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")) {
      set.seed(312)
      report <- .getAlgorithm(method)$clientUpdate(client, list(phase = 0L), list(intercept = intercept))
      expect_length(report$bhat, ncol(x) + 1L)
      expect_equal(report$bhat, expected$beta, tolerance = 1e-8)
      expect_equal(report$n, length(y))
    }
  }
})

test_that("a single predictor still has an unpenalized ADAP initialization intercept", {
  set.seed(411)
  x <- Matrix::Matrix(matrix(rnorm(160), ncol = 1), sparse = TRUE)
  y <- rbinom(160, 1, stats::plogis(-1 + x[, 1]))
  for (dense in c(TRUE, FALSE)) {
    set.seed(412)
    fit <- .fitPdaStyleLocalLasso(x, .addInterceptColumn(x), y,
      list(maxDenseInitCells = if (dense) Inf else 0))
    expect_length(fit$beta, 2L)
    expect_true(all(is.finite(fit$beta)))
    expect_lt(abs(mean(stats::plogis(as.numeric(.addInterceptColumn(x) %*% fit$beta)) - y)), 1e-6)
  }
})

test_that("ADAP initialization is reproducible and leaves the caller's RNG untouched", {
  set.seed(511)
  x <- matrix(rnorm(900), nrow = 300)
  y <- rbinom(300, 1, stats::plogis(-1 + x[, 1] - 0.5 * x[, 2]))
  xRaw <- Matrix::Matrix(x, sparse = TRUE)
  xDesign <- .addInterceptColumn(xRaw)
  first <- .fitPdaStyleLocalLasso(xRaw, xDesign, y, list(cvSeed = 7L))
  oldKind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(oldKind)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(99)
  before <- .Random.seed
  second <- .fitPdaStyleLocalLasso(xRaw, xDesign, y, list(cvSeed = 7L))
  expect_identical(second, first)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  expect_identical(.adapLocalInitFoldId(300, 7L), .adapLocalInitFoldId(300, 7L))
  expect_false(identical(.adapLocalInitFoldId(300, 7L), .adapLocalInitFoldId(300, 8L)))
  expect_identical(sort(unique(.adapLocalInitFoldId(300, 7L))), 1:10)
  seeded <- .fitPdaStyleLocalLasso(xRaw, xDesign, y, list(cvSeed = 7L, adapInitSeed = 8L))
  expect_equal(seeded$beta, .fitPdaStyleLocalLasso(xRaw, xDesign, y, list(cvSeed = 8L))$beta)
})
