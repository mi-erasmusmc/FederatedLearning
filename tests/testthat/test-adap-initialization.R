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
