preprocessingRunner <- function() {
  path <- testthat::test_path("..", "..", "extras", "runComparisonMatrix.R")
  if (!file.exists(path)) skip("Extras scripts unavailable")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

preprocessingFixture <- function() {
  x <- cbind(1, age = seq(0.1, 1, length.out = 10), zero = 0, constant = 1,
    rare = c(1, rep(0, 9)), boundary = c(1, 1, rep(0, 8)),
    siteConstant = c(rep(0, 3), rep(1, 7)))
  y <- rep(0:1, 5)
  list(x = x, y = y, mapping = data.frame(covariateId = c(1002, 10, 20, 30, 40, 50), columnId = 1:6),
    clients = lapply(list(1:3, 4:10), function(i) list(
      xMatrix = Matrix::Matrix(x[i, , drop = FALSE], sparse = TRUE), yLabels = y[i], n = length(i))))
}

test_that("distributed filtering matches baseline filtering on unequal sites", {
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  args <- list("baseline-preprocess-min-fraction" = "0.2")
  summaries <- lapply(f$clients, trainingFeatureSummary, intercept = TRUE)
  p <- preprocessorFromSummaries(summaries, f$mapping, env$baselinePreprocessSettings(args))
  baseline <- env$fitBaselinePreprocessor(f$clients, TRUE, args)
  expect_identical(p$audit$retained, as.logical(baseline$keep))
  expect_identical(p$audit$retained, c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(p$audit$nonzeroRows, unname(colSums(f$x[, -1] != 0)))
  expect_equal(p$audit$variance, unname(apply(f$x[, -1], 2, var) * 9 / 10))
  expect_identical(p$mapping$columnId, 1:3)
  expect_identical(p$mapping$covariateId, c(1002, 40, 50))
  expect_true(p$audit$removedRare[4])
  expect_false(p$audit$removedRare[5])
  expect_true(p$audit$removedNearConstant[3])
  expect_false(p$audit$removedNearConstant[6])
  expect_identical(p$scope, "outer-training")
  expect_identical(p$fingerprint, preprocessingFingerprint(c(1002, 40, 50)))
  dense <- f$clients
  dense[[1]]$xMatrix <- as.matrix(dense[[1]]$xMatrix)
  dense[[2]]$xMatrix <- as.matrix(dense[[2]]$xMatrix)
  expect_equal(lapply(dense, trainingFeatureSummary, intercept = TRUE), summaries)
  # Retaining a common map is equivalent to explicit matrix subsetting, including age scale.
  for (client in f$clients) {
    actual <- env$applyBaselinePreprocessor(client, baseline, TRUE)$xMatrix
    expected <- client$xMatrix[, c(TRUE, p$audit$retained), drop = FALSE]
    expect_equal(unname(as.matrix(actual)), unname(as.matrix(expected)))
    expect_equal(as.numeric(actual %*% c(-1, 0.4, 0, -0.1)),
      as.numeric(expected %*% c(-1, 0.4, 0, -0.1)))
  }
})

test_that("preprocessing handles shapes, threshold rounding and invalid summaries", {
  env <- preprocessingRunner()
  settings <- env$baselinePreprocessSettings(list())
  result <- env$baselineFeatureSelection(999, c(0, 1), c(0, 1 / 999), c(0, 1 / 999), settings)
  expect_equal(result$minCount, 0)
  expect_identical(result$rare, c(FALSE, FALSE))
  expect_identical(result$keep, c(FALSE, TRUE))
  result <- env$baselineFeatureSelection(4, c(4, 4), c(1, 1e8), c(1 - 1e-16, 1e16), settings)
  expect_equal(result$variance, c(0, 0))
  expect_false(any(result$keep))
  x <- list(xMatrix = Matrix::Matrix(matrix(c(0.1, 0.2, 0.3), ncol = 1), sparse = TRUE),
    yLabels = c(0, 1, 0), n = 3)
  summary <- trainingFeatureSummary(x, FALSE)
  expect_equal(summary$n, 3)
  expect_length(summary$nnz, 1)
  single <- x
  single$xMatrix <- x$xMatrix[1, , drop = FALSE]
  single$yLabels <- 0
  single$n <- 1
  expect_equal(trainingFeatureSummary(single, FALSE)$sum, 0.1)
  x$n <- 999
  expect_error(trainingFeatureSummary(x, FALSE), "row count")
  expect_error(env$baselineFeatureSelection(0, 0, 0, 0, settings), "Invalid")
  expect_error(env$baselineFeatureSelection(4, 5, 0, 0, settings), "Invalid")
  expect_error(env$baselineFeatureSelection(4, 1, NA, 0, settings), "Invalid")
  expect_error(preprocessorFromSummaries(list(summary),
    data.frame(covariateId = 10, columnId = 2), settings), "feature map")
})

test_that("opt-in settings and resume guard reject incompatible filtering", {
  env <- preprocessingRunner()
  expect_identical(env$federatedPreprocessMode(list()), "none")
  args <- list("federated-preprocess" = "baseline")
  expect_identical(env$federatedPreprocessMode(args), "baseline")
  expect_error(env$federatedPreprocessMode(list("federated-preprocess" = "typo")), "must be")
  expect_error(env$federatedPreprocessMode(c(args, list("baseline-preprocess-normalize" = "true"))), "normalization")
  expect_error(env$federatedPreprocessMode(c(args, list("baseline-preprocess" = "false"))), "enabled")
  expect_error(env$federatedPreprocessMode(c(args, list("preprocess-min-fraction" = "NaN"))), "min-fraction")
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  env$guardPreprocessingSettings(args, directory)
  expect_silent(env$guardPreprocessingSettings(args, directory))
  expect_error(env$guardPreprocessingSettings(list(), directory), "differ")
  expect_error(env$guardPreprocessingSettings(c(args, list("preprocess-min-fraction" = "0.01")), directory), "differ")
  expect_error(env$guardPreprocessingSettings(c(args, list("map-type" = "union")), directory), "differ")
  unlink(file.path(directory, "preprocessing_settings.rds"))
  write.csv(data.frame(auc = 0.7), file.path(directory, "comparison_results.csv"))
  expect_error(env$guardPreprocessingSettings(args, directory), "unverified")
  expect_silent(env$guardPreprocessingSettings(list(), directory))
})

test_that("worker summary exchange cannot reuse a previous feature map", {
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  cl <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(cl, f$clients, function(data) {
    library(Matrix)
    assign("clientData", data, envir = .GlobalEnv)
    NULL
  })
  local_mocked_bindings(
    clusterCollectCovRefs = function(...) f$mapping,
    clusterCreateMatrices = function(...) NULL,
    .package = "FederatedLearning")
  cfg <- list(intercept = TRUE, mapType = "union", featureSet = "all")
  args <- list("baseline-preprocess-min-fraction" = "0.2")
  result <- env$fitFederatedPreprocessor(cl, cfg, args)
  expect_identical(result$mapping$covariateId, c(1002, 40, 50))
  expect_equal(result$communication$messages, 2)
  expect_equal(result$communication$numbers, 2 * (1 + 3 * 6))
  expect_gte(result$elapsedSeconds, 0)
  again <- env$fitFederatedPreprocessor(cl, cfg, list("baseline-preprocess-min-fraction" = "0"))
  expect_identical(again$mapping$covariateId, c(1002, 30, 40, 50))
  expect_false(identical(again$fingerprint, result$fingerprint))
})

test_that("saved pooled masks and population provenance are checked before fitting", {
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  args <- list("baseline-preprocess-min-fraction" = "0.2")
  p <- preprocessorFromSummaries(lapply(f$clients, trainingFeatureSummary, intercept = TRUE),
    f$mapping, env$baselinePreprocessSettings(args))
  cfg <- list(mapType = "union", intercept = TRUE)
  population <- list(riskWindowEnd = 365L)
  artifact <- list(task = "task", fold = 3L, featureSet = "all", method = "PooledLasso",
    originalMapping = f$mapping, preprocessing = env$fitBaselinePreprocessor(f$clients, TRUE, args),
    populationSettings = population, config = cfg,
    trainClientIds = c("a", "b"), testClientIds = "c", trainingSampleSizes = c(a = 3, b = 7),
    models = list(list(coefficients = env$coefficientTable(c(-1, 0.2, 0, 0.1), p$mapping, TRUE))))
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "task", fold = 3, featureSet = "all", method = "PooledLasso", auc = 0.7)
  rows <- env$saveModelArtifact(artifact, rows, file.path(directory, "models"))
  write.csv(rows, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  verify <- function(p, population = list(riskWindowEnd = 365L)) {
    env$checkPreprocessingReference(p, directory, "task", 3, "all", c("a", "b"), "c", population, cfg)
  }
  expect_identical(verify(p), "matched")
  bad <- p
  bad$audit$retained[1] <- FALSE
  expect_error(verify(bad), "feature mask")
  expect_error(verify(p, list(riskWindowEnd = 30L)), "population")
  bad <- p
  bad$audit$trainingRows <- 11
  expect_error(verify(bad), "row count")
  badSizes <- p
  badSizes$trainingSampleSizes <- c(4, 6)
  expect_error(verify(badSizes), "row count")
  p$referenceStatus <- "matched"
  expect_message(env$savePreprocessingAudit(p, directory, "task", 3, "all", c("a", "b"), "c", population), "retained 3/6")
  expect_true(file.exists(file.path(directory, "task_fold3_all.csv")))
  expect_message(env$savePreprocessingAudit(p, directory, "task", 3, "all", c("a", "b"), "c", population), "retained")
  summary <- read.csv(file.path(directory, "summary.csv"))
  expect_equal(nrow(summary), 1L)
  expect_true(summary$requiresRefit)
  expect_identical(summary$referenceStatus, "matched")
  expect_error(env$savePreprocessingAudit(bad, directory, "task", 3, "all", c("a", "b"), "c", population), "audit changed")
})

test_that("reference population checks accept numeric storage differences but reject value changes", {
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  args <- list("baseline-preprocess-min-fraction" = "0.2")
  p <- preprocessorFromSummaries(lapply(f$clients, trainingFeatureSummary, intercept = TRUE),
    f$mapping, env$baselinePreprocessSettings(args))
  cfg <- list(mapType = "union", intercept = TRUE)
  population <- structure(list(riskWindowStart = 1L, riskWindowEnd = 30,
    removeSubjectsWithPriorOutcome = TRUE), class = "populationSettings")
  artifact <- list(task = "task", fold = 3L, featureSet = "all", method = "PooledLasso",
    originalMapping = f$mapping, preprocessing = env$fitBaselinePreprocessor(f$clients, TRUE, args),
    populationSettings = population, config = cfg,
    trainClientIds = c("a", "b"), testClientIds = "c", trainingSampleSizes = c(a = 3, b = 7),
    models = list(list(coefficients = env$coefficientTable(c(-1, 0.2, 0, 0.1), p$mapping, TRUE))))
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "task", fold = 3, featureSet = "all", method = "PooledLasso", auc = 0.7)
  rows <- env$saveModelArtifact(artifact, rows, file.path(directory, "models"))
  write.csv(rows, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  verify <- function(pop) env$checkPreprocessingReference(p, directory, "task", 3,
    "all", c("a", "b"), "c", pop, cfg)

  reconstructed <- population
  reconstructed$riskWindowStart <- 1
  reconstructed$riskWindowEnd <- 30L
  expect_identical(verify(reconstructed), "matched")
  for (end in c(365, 30 + 1e-10, NA_real_)) {
    changed <- reconstructed
    changed$riskWindowEnd <- end
    expect_error(verify(changed), "population")
  }
  changed <- reconstructed
  changed$removeSubjectsWithPriorOutcome <- FALSE
  expect_error(verify(changed), "population")
  expect_error(verify(unclass(reconstructed)), "population")
  changed <- reconstructed
  changed$riskWindowStart <- NULL
  expect_error(verify(changed), "population")
})

test_that("audit-only runner never loads held-out data or invokes fitters", {
  skip_if_not_installed("PatientLevelPrediction")
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (id in c("a", "b", "test")) dir.create(file.path(directory, "data", "task", id), recursive = TRUE)
  args <- list("data-root" = file.path(directory, "data"), tasks = "task", folds = "3",
    "client-ids" = "a,b,test", "feature-sets" = "all", "methods" = "DualAvg",
    "result-directory" = file.path(directory, "results"), "federated-preprocess" = "baseline",
    "preprocessing-audit-only" = "true")
  loaded <- character()
  local_mocked_bindings(clusterInit = function(...) list(),
    clusterLoadData = function(cl, clientPaths, popSettings) {
      loaded <<- c(loaded, basename(clientPaths))
      c(3, 7)
    }, .package = "FederatedLearning")
  env$safeStopCluster <- function(cl) NULL
  env$fitFederatedPreprocessor <- function(cl, config, args) {
    preprocessorFromSummaries(lapply(f$clients, trainingFeatureSummary, intercept = TRUE),
      f$mapping, env$baselinePreprocessSettings(args))
  }
  env$fitFederatedFold <- env$fitBaselineFold <- env$selectMethodConfigForFold <- function(...) stop("FIT CALLED")
  expect_message(env$runComparison(args), "Preprocessing task")
  expect_identical(loaded, c("a", "b"))
  expect_false(file.exists(file.path(args[["result-directory"]], "comparison_results.csv")))
  expect_true(file.exists(file.path(args[["result-directory"]], "preprocessing", "task_fold3_all.rds")))
  args[["dualavg-map-type"]] <- "union"
  expect_error(env$runComparison(args), "same matrix settings")
})

test_that("no-op masks leave baseline transforms and DualAvg updates unchanged", {
  env <- preprocessingRunner()
  x <- Matrix::Matrix(cbind(1, c(0.2, 0.4, 0.8, 1), c(0, 1, 0, 1)), sparse = TRUE)
  data <- list(xMatrix = x, yLabels = c(0, 1, 0, 1), n = 4L)
  mapping <- data.frame(covariateId = c(1002, 8532001), columnId = 1:2)
  settings <- env$baselinePreprocessSettings(list())
  p <- preprocessorFromSummaries(list(trainingFeatureSummary(data, TRUE)), mapping, settings)
  expect_true(all(p$audit$retained))
  expect_equal(p$mapping, mapping)
  pp <- env$fitBaselinePreprocessor(list(data), TRUE, list())
  transformed <- env$applyBaselinePreprocessor(data, pp, TRUE)
  expect_equal(as.matrix(transformed$xMatrix), as.matrix(data$xMatrix))
  cfg <- list(intercept = TRUE, lambda = 0.03, etaClient = 0.2, etaServer = 1, k = 2L)
  state <- list(z = c(-1, 0.2, 0), r = 0L)
  expect_equal(clientUpdateDualAveragingCpp(data, state, cfg),
    clientUpdateDualAveragingCpp(transformed, state, cfg), tolerance = 1e-14)
  disabled <- env$preprocessBaselineData(list(data), list(data), list(intercept = TRUE),
    list("baseline-preprocess" = "false"))
  expect_identical(disabled$trainData, list(data))
  expect_identical(disabled$testData, list(data))
})

test_that("filtered maps survive lambda tuning and artifact saving without double filtering", {
  env <- preprocessingRunner()
  f <- preprocessingFixture()
  args <- list("baseline-preprocess-min-fraction" = "0.2")
  filtering <- preprocessorFromSummaries(lapply(f$clients, trainingFeatureSummary, intercept = TRUE),
    f$mapping, env$baselinePreprocessSettings(args))
  filtering$communication <- list(messages = 2L, numbers = 38L)
  filtering$elapsedSeconds <- 0.5
  cfg <- env$methodConfig("DualAvg", "all", list("dualavg-rounds" = "3"))
  cfg$mapping <- filtering$mapping
  cfg$p <- nrow(cfg$mapping)
  cfg$featureFiltering <- filtering
  cfg$trainClientPaths <- c("a", "b")
  local_mocked_bindings(
    tuneLambda = function(cl, algorithm, configBase, ..., globalMap) {
      expect_identical(configBase$mapping, filtering$mapping)
      expect_identical(globalMap, filtering$mapping)
      expect_equal(configBase$p, 3)
      expect_identical(configBase$featureFiltering, filtering)
      list(bestLambda = 0.01, bestSearchValue = 0.1, perf = 0.7)
    },
    fitFederated = function(cl, algorithm, config, verbose) {
      expect_identical(config$mapping, filtering$mapping)
      list(w = c(-1, 0.3, 0, 1e-15), config = config, roundsCompleted = 3L)
    },
    clusterCreateMatrices = function(cl, config) expect_identical(config$mapping, filtering$mapping),
    clusterEvaluateModel = function(cl, w) data.frame(client = 1L, auc = 0.7, n = 10, outcomes = 2),
    .package = "FederatedLearning")
  tuned <- env$tuneDualAvgForFold("DualAvg", NULL, cfg, c(3, 7), list(), FALSE)
  expect_identical(tuned$mapping, filtering$mapping)
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  result <- env$fitFederatedFold("DualAvg", NULL, NULL, tuned, directory, "task", "all", 3L,
    "test", FALSE, modelDirectory = file.path(directory, "models"))
  saved <- readRDS(file.path(directory, result$modelFile))
  expect_identical(saved$models[[1]]$coefficients$covariateId, c(NA_character_, "1002", "40", "50"))
  expect_identical(saved$featureFiltering$audit$retained, c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(saved$models[[1]]$coefficients$coefficient, c(-1, 0.3, 0, 1e-15))
  expect_equal(result$preprocessMessages, 2)
  expect_equal(result$preprocessNumbers, 38)
  expect_identical(result$preprocessFingerprint, filtering$fingerprint)
})
