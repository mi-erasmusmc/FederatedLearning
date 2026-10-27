test_that("Cyclops and glmnet solve the same fixed-penalty logistic lasso", {
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("glmnet")
  set.seed(1021)
  n <- 600L
  x <- cbind(rnorm(n), rbinom(n, 1, 0.2), rnorm(n), 0)
  x[, 3] <- 0.8 * x[, 1] + 0.6 * x[, 3]
  y <- rbinom(n, 1, plogis(-2.8 + 0.8 * x[, 1] - 0.9 * x[, 2]))
  for (sparse in c(FALSE, TRUE)) {
    predictors <- if (sparse) Matrix::Matrix(x, sparse = TRUE) else x
    design <- cbind(1, predictors)
    for (lambda in c(0.002, 0.02, 0.2)) {
      expected <- suppressWarnings(glmnet::glmnet(predictors, y, family = "binomial", lambda = lambda,
        intercept = TRUE, standardize = FALSE, thresh = 1e-13, maxit = 1e6))
      a <- as.numeric(stats::coef(expected))
      variance <- (sqrt(2) / (n * lambda))^2
      fit <- fitCyclopsWeights(list(list(xMatrix = .asDgCMatrix(design), yLabels = y, n = n)),
        list(`cyclops-tolerance` = 1e-10, `cyclops-max-iterations` = 10000),
        seed = 42L, intercept = TRUE, variance = variance)
      b <- fit$w
      loss <- function(w) logisticNegLogLik(w, design, y, meanLoss = TRUE) + lambda * sum(abs(w[-1]))
      kkt <- function(w) max(.lassoKktResidual(w, gradLogistic(w, design, y), lambda))
      expect_lt(kkt(a), 1e-6)
      expect_lt(kkt(b), 1e-6)
      expect_lt(abs(loss(a) - loss(b)), 1e-8)
      expect_lt(max(abs(as.numeric(design %*% (a - b)))), 1e-3)
      expect_identical(fit$fittingSettings$returnFlag, "SUCCESS")
    }
  }
})

test_that("Cyclops initialization uses the original folds and scores their held-out rows", {
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("glmnet")
  set.seed(1121)
  n <- 301L
  x <- Matrix::Matrix(matrix(rnorm(n * 3), ncol = 3), sparse = TRUE)
  design <- .addInterceptColumn(x)
  y <- rbinom(n, 1, plogis(-1 + x[, 1] - x[, 2]))
  config <- list(adapInitBackend = "cyclops", adapInitSeed = 11L, adapInitMaxEvals = 2L)
  oldKind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(oldKind)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(99)
  before <- .Random.seed
  fit <- suppressWarnings(.fitPdaStyleLocalLasso(x, design, y, config))
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  folds <- .adapLocalInitFoldId(n, 11L)
  diag <- fit$diagnostics
  expect_identical(diag$foldSizes, tabulate(folds, 10L))
  expect_identical(diag$foldOutcomes, tabulate(folds[y == 1], 10L))
  expect_equal(fit$lambda, .laplaceVarianceToLambda(diag$variance, n))
  expect_equal(diag$tolerance, 1e-12)
  expect_lte(diag$kktMax, diag$kktTolerance)
  expect_true(all(diag$trace$kktMax <= diag$kktTolerance))
  expect_equal(diag$trace$score[which.max(diag$trace$score)], max(diag$trace$score))
  expect_equal(diag$variance, diag$trace$variance[which.max(diag$trace$score)])
  args <- list(`cyclops-tolerance` = 1e-12, `cyclops-max-iterations` = 3000L,
    `cyclops-noise-level` = "silent", `cyclops-threads` = 1L)
  data <- list(xMatrix = design, yLabels = y, n = n)
  actualLoss <- sum(vapply(seq_len(10L), function(fold) {
    train <- which(folds != fold)
    held <- which(folds == fold)
    variance <- (sqrt(2) / (length(train) * fit$lambda))^2
    w <- fitCyclopsWeights(list(clientRows(data, train)), args, 11L, variance = variance)$w
    binaryLogLoss(as.numeric(design[held, , drop = FALSE] %*% w), y[held], meanLoss = FALSE)
  }, numeric(1)))
  expect_equal(max(diag$trace$score), -actualLoss / n, tolerance = 1e-11)
  glmnetLoss <- sum(vapply(seq_len(10L), function(fold) {
    train <- which(folds != fold)
    held <- which(folds == fold)
    model <- suppressWarnings(glmnet::glmnet(x[train, , drop = FALSE], y[train],
      family = "binomial", lambda = fit$lambda, intercept = TRUE, standardize = FALSE,
      thresh = 1e-13, maxit = 1e6))
    eta <- as.numeric(design[held, , drop = FALSE] %*% as.numeric(stats::coef(model)))
    binaryLogLoss(eta, y[held], meanLoss = FALSE)
  }, numeric(1)))
  expect_equal(actualLoss / n, glmnetLoss / n, tolerance = 2e-6)
  refit <- fitCyclopsWeights(list(data), args, 11L, variance = diag$variance)
  expect_equal(fit$beta, refit$w, tolerance = 1e-10)
  repeated <- suppressWarnings(.fitPdaStyleLocalLasso(as.matrix(x), as.matrix(design), y, config))
  expect_equal(fit$beta, repeated$beta, tolerance = 1e-10)
  expect_identical(fit$diagnostics$trace, repeated$diagnostics$trace)
  expect_error(.fitCyclopsLocalLasso(design, y, list(standardize = TRUE)), "standardize = FALSE")
  expect_error(.fitCyclopsLocalLasso(design, rep(0L, n), list()), "both outcome classes")
})

test_that("a successful Cyclops return cannot bypass the KKT check", {
  skip_if_not_installed("Cyclops")
  local_mocked_bindings(fitCyclopsWeights = function(clientDataList, ...) {
    list(w = rep(0, ncol(clientDataList[[1]]$xMatrix)), fittingSettings = list(returnFlag = "SUCCESS"))
  }, .package = "FederatedLearning")
  x <- Matrix::Matrix(matrix(seq_len(100) / 100, ncol = 1), sparse = TRUE)
  y <- c(rep(0L, 80), rep(1L, 20))
  expect_error(.fitCyclopsLocalLasso(.addInterceptColumn(x), y, list(adapInitMaxEvals = 1L)),
    "KKT residual.*above")
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

test_that("Cyclops uses the same configured stratified initialization folds", {
  skip_if_not_installed("Cyclops")
  set.seed(1121)
  x <- Matrix::Matrix(matrix(rnorm(903), ncol = 3), sparse = TRUE)
  y <- rbinom(301, 1, plogis(-1 + x[, 1] - x[, 2]))
  config <- list(adapInitBackend = "cyclops", adapInitFolds = 3L,
    adapInitStratify = TRUE, adapInitSeed = 19L, adapInitMaxEvals = 1L)
  fit <- suppressWarnings(.fitPdaStyleLocalLasso(x, cbind(1, x), y, config))
  folds <- .adapLocalInitFolds(y, config)
  expect_identical(fit$diagnostics$foldSizes, tabulate(folds, 3L))
  expect_identical(fit$diagnostics$foldOutcomes, tabulate(folds[y == 1], 3L))
  expect_true(all(fit$diagnostics$trace$kktMax <= fit$diagnostics$kktTolerance))
})

test_that("ADAP methods select the initializer and retain its metadata", {
  skip_if_not_installed("Cyclops")
  set.seed(1221)
  x <- Matrix::Matrix(matrix(rnorm(300), ncol = 3), sparse = TRUE)
  design <- .addInterceptColumn(x)
  y <- rbinom(100, 1, plogis(-1 + x[, 1]))
  config <- list(intercept = TRUE, adapInitBackend = "cyclops", adapInitMaxEvals = 1L)
  data <- list(xMatrix = design, yLabels = y, n = length(y))
  for (method in c("ADAP1", "ADAP2")) {
    algo <- .getAlgorithm(method)
    report <- suppressWarnings(algo$clientUpdate(data, list(phase = 0L), config))
    expect_identical(report$initDiagnostics$backend, "cyclops")
    expect_equal(report$lambdaLocal, report$initDiagnostics$lambda)
    state <- algo$serverRound(list(phase = 0L), list(report), config)$state
    expect_equal(state$adapInitDiagnostics[[1]], report$initDiagnostics)
  }
  default <- .fitPdaStyleLocalLasso(x, design, y, list())
  explicit <- .fitPdaStyleLocalLasso(x, design, y, list(adapInitBackend = "glmnet"))
  expect_identical(default, explicit)
  sparse <- .fitPdaStyleLocalLasso(x, design, y, list(adapInitBackend = "glmnet_sparse"))
  referenceSparse <- .fitPdaStyleLocalLasso(x, design, y, list(maxDenseInitCells = 0))
  expect_identical(sparse, referenceSparse)
  denseInputSparse <- .fitPdaStyleLocalLasso(as.matrix(x), as.matrix(design), y,
    list(adapInitBackend = "glmnet_sparse"))
  expect_equal(denseInputSparse, sparse, tolerance = 1e-12)
  expect_identical(createMethodSettings("ADAP2", adapInitBackend = "cyclops")$options$adapInitBackend, "cyclops")
  expect_identical(methodConfig("ADAP2", "all", list(`adap-init-backend` = "cyclops"))$adapInitBackend, "cyclops")
  expect_equal(methodConfig("ADAP2", "all", list(`adap-init-kkt-tolerance` = "1e-5"))$adapInitKktTolerance, 1e-5)
  expect_error(methodConfig("ADAP2", "all", list(`adap-init-backend` = "bad")), "should be one of")
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
  expect_error(check(saved, list(`adap-init-backend` = "cyclops")), "different initializer")
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
  artifact$config <- list(adapInitBackend = "cyclops", cvSeed = 42L)
  savedCyclops <- saveModelArtifact(artifact, rows, file.path(directory, "models"))
  expect_null(check(savedCyclops, list(`adap-init-backend` = "cyclops")))
  expect_error(check(savedCyclops, list()), "different initializer")
  expect_error(check(savedCyclops, list(`adap-init-backend` = "cyclops", `adap-init-tolerance` = "1e-6")),
    "different initializer settings")
  expect_error(check(rows, list(`adap-init-backend` = "cyclops")), "Cannot verify")
  expect_null(check(rows, list()))
})

test_that("a comparison saves the actual Cyclops initialization diagnostics", {
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("PatientLevelPrediction")
  paths <- stats::setNames(test_path("fixtures", "runner-golden", "data", "goldenTask", paste0("site", 1:3)),
    paste0("site", 1:3))
  if (!all(dir.exists(paths))) skip("Golden runner fixture is unavailable")
  output <- tempfile("adap-cyclops-comparison-")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  population <- PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = FALSE,
    minTimeAtRisk = 1, riskWindowEnd = 365)
  results <- suppressWarnings(runComparison(
    createTaskSettings("goldenTask", 1, 3, population), paths,
    list(ageSex = createFeatureSettings(covariateIds = c(1002, 8532001), minFraction = NULL)),
    createMethodSettings("ADAP1", adapInitBackend = "cyclops", adapInitMaxEvals = 1L,
      adapInitFolds = 3L, adapInitStratify = TRUE, lambda = 0.1),
    output, holdouts = "site3", runSettings = createRunSettings(hosts = "localhost", mirai = FALSE, verbose = FALSE)))
  expect_true(is.na(results$error[[1]]) || !nzchar(results$error[[1]]), info = results$error[[1]])
  artifact <- readModelArtifact(results, output, "goldenTask", 3L, "ageSex", "ADAP1")
  expect_false(is.null(artifact))
  expect_identical(artifact$config$adapInitBackend, "cyclops")
  expect_identical(artifact$config$adapInitFolds, 3L)
  expect_true(artifact$config$adapInitStratify)
  expect_length(artifact$adapInitDiagnostics, 2L)
  expect_true(all(vapply(artifact$adapInitDiagnostics, function(d) identical(d$backend, "cyclops"), logical(1))))
  expect_true(all(vapply(artifact$adapInitDiagnostics, function(d) length(d$foldSizes) == 3L, logical(1))))
  expect_equal(artifact$roundsCompleted, 3L)
})

test_that("stratified initializers run on installed socket and mirai workers", {
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parallelly")
  runWorker <- function(mirai, backend) {
    cl <- clusterInit("localhost", "unused", mirai = mirai)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(backend) {
      set.seed(1321)
      x <- Matrix::Matrix(matrix(rnorm(300), ncol = 3), sparse = TRUE)
      y <- rbinom(100, 1, stats::plogis(-1 + x[, 1]))
      data <- list(xMatrix = cbind(1, x), yLabels = y, n = length(y))
      algo <- getFromNamespace(".getAlgorithm", "FederatedLearning")("ADAP1")
      suppressWarnings(algo$clientUpdate(data, list(phase = 0L),
        list(intercept = TRUE, adapInitBackend = backend, adapInitMaxEvals = 1L,
          adapInitFolds = 3L, adapInitStratify = TRUE)))
    }, backend)[[1]]
  }
  for (mirai in c(FALSE, TRUE)) {
    if (mirai && !requireNamespace("mirai", quietly = TRUE)) next
    for (backend in c("glmnet_sparse", "cyclops")) {
      result <- runWorker(mirai, backend)
      if (backend == "cyclops") {
        expect_identical(result$initDiagnostics$backend, "cyclops")
        expect_length(result$initDiagnostics$foldSizes, 3L)
      }
      expect_length(result$bhat, 4L)
      expect_true(all(is.finite(result$bhat)))
    }
  }
})
