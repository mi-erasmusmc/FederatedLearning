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

test_that("ADAP initializer folds are reproducible, stratified and usable", {
  y <- c(rep(0L, 97), rep(1L, 8))
  config <- list(adapInitFolds = 3L, adapInitStratify = TRUE, adapInitSeed = 21L)
  set.seed(420)
  before <- .Random.seed
  folds <- .adapLocalInitFolds(y, config)
  expect_identical(.Random.seed, before)
  expect_identical(folds, .adapLocalInitFolds(y, config))
  expect_identical(sort(unique(folds)), 1:3)
  for (value in 0:1) {
    counts <- tabulate(folds[y == value], 3L)
    expect_lte(max(counts) - min(counts), 1L)
  }
  expect_error(.adapLocalInitFolds(y, list(adapInitFolds = 2L)), "fold count")
  expect_error(.adapLocalInitFolds(rep(0L, 10), config), "both classes")
  expect_error(.adapLocalInitFolds(c(rep(0L, 9), 1L), config), "each training fold")
  expect_error(.adapLocalInitFolds(y, list(adapInitStratify = NA)), "TRUE or FALSE")
  expect_identical(.adapLocalInitFolds(y, list(adapInitSeed = 21L)),
    .adapLocalInitFoldId(length(y), 21L))
})

test_that("sparse glmnet receives sparse input even when the client matrix is dense", {
  skip_if_not_installed("glmnet")
  original <- glmnet::cv.glmnet
  input <- NULL
  observedFolds <- NULL
  local_mocked_bindings(cv.glmnet = function(x, ...) {
    input <<- x
    observedFolds <<- list(...)$foldid
    original(x = x, ...)
  }, .package = "glmnet")
  set.seed(310)
  x <- matrix(rnorm(300), ncol = 3)
  y <- rep(c(0L, 1L), 50)
  config <- list(adapInitBackend = "glmnet_sparse", adapInitFolds = 3L, adapInitStratify = TRUE)
  .fitPdaStyleLocalLasso(x, cbind(1, x), y, config)
  expect_s4_class(input, "sparseMatrix")
  expect_identical(observedFolds, .adapLocalInitFolds(y, config))
})

test_that("sparse and dense glmnet give the same starting estimate", {
  skip_if_not_installed("glmnet")
  set.seed(1221)
  x <- Matrix::Matrix(matrix(rnorm(300), ncol = 3), sparse = TRUE)
  design <- .addInterceptColumn(x)
  y <- rbinom(100, 1, plogis(-1 + x[, 1]))
  default <- .fitPdaStyleLocalLasso(x, design, y, list())
  expect_identical(default, .fitPdaStyleLocalLasso(x, design, y, list(adapInitBackend = "glmnet")))
  expect_true(default$usedDensePdaPath)
  sparse <- .fitPdaStyleLocalLasso(x, design, y, list(adapInitBackend = "glmnet_sparse"))
  expect_false(sparse$usedDensePdaPath)
  expect_identical(sparse, .fitPdaStyleLocalLasso(x, design, y, list(maxDenseInitCells = 0)))
  expect_equal(sparse$beta, default$beta, tolerance = 1e-6)
  expect_equal(.fitPdaStyleLocalLasso(as.matrix(x), as.matrix(design), y,
    list(adapInitBackend = "glmnet_sparse")), sparse, tolerance = 1e-12)
  expect_identical(createMethodSettings("ADAP2", adapInitBackend = "glmnet_sparse")$options$adapInitBackend,
    "glmnet_sparse")
  expect_identical(methodConfig("ADAP2", "all", list(`adap-init-folds` = "3"))$adapInitFolds, 3L)
  expect_error(methodConfig("ADAP2", "all", list(`adap-init-backend` = "cyclops")), "should be one of")
})

test_that("resume rejects a different or unverifiable ADAP initializer", {
  directory <- tempfile("adap-resume-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "task", fold = 1L, featureSet = "all", method = "ADAP1", error = NA_character_)
  model <- list(coefficients = data.frame(coefficient = 0, covariateId = NA_character_, isIntercept = TRUE))
  artifact <- list(task = "task", fold = 1L, featureSet = "all", method = "ADAP1",
    models = list(model), config = list(adapInitBackend = "glmnet"))
  saved <- saveModelArtifact(artifact, rows, file.path(directory, "models"))
  check <- function(rows, args) guardAdapInitializerResume(rows, "task", 1L, "all", "ADAP1", args, directory)
  expect_null(check(saved, list()))
  expect_error(check(saved, list(`adap-init-backend` = "glmnet_sparse")), "different initializer")
  expect_error(check(saved, list(`adap-init-folds` = "3", `adap-init-stratify` = "true")),
    "different initializer folds or seed")
  artifact$config <- list(adapInitBackend = "glmnet_sparse", adapInitFolds = 3L,
    adapInitStratify = TRUE, cvSeed = 42L)
  savedSparse <- saveModelArtifact(artifact, rows, file.path(directory, "models"))
  sparseArgs <- list(`adap-init-backend` = "glmnet_sparse", `adap-init-folds` = "3", `adap-init-stratify` = "true")
  expect_null(check(savedSparse, sparseArgs))
  expect_error(check(savedSparse, list(`adap-init-backend` = "glmnet_sparse")), "different initializer folds or seed")
  expect_error(check(rows, list(`adap-init-folds` = "3", `adap-init-stratify` = "true")), "Cannot verify")
  expect_null(check(rows, list()))
})
