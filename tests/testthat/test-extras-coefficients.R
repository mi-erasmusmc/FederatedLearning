coefficientExtrasPath <- function(file) {
  path <- testthat::test_path("..", "..", "extras", file)
  if (!file.exists(path)) skip("Extras scripts are unavailable in this installation")
  normalizePath(path)
}

coefficientRunner <- function() {
  env <- new.env(parent = globalenv())
  sys.source(coefficientExtrasPath("runComparisonMatrix.R"), env)
  env
}

test_that("exact support retains tiny coefficients and excludes the intercept", {
  env <- coefficientRunner()
  mapping <- data.frame(covariateId = c(10, 20, 30), columnId = 1:3)
  w <- c(-4, 0, 1e-20, -1e-12)
  tab <- env$coefficientTable(w, mapping, TRUE)
  expect_identical(tab$selected, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(env$exactNonzero(w, TRUE), 2L)
  expect_equal(env$exactNonzero(w, FALSE), 3L)
  expect_equal(env$exactNonzero(3, TRUE), 0L)
  expect_true(is.na(env$exactNonzero(c(1, Inf), TRUE)))
  expect_equal(tab$coefficient, w, tolerance = 0)
  expect_error(env$coefficientTable(c(1, 2), mapping, TRUE), "dimensions")
  expect_error(env$coefficientTable(c(1, NA, 0, 2), mapping, TRUE), "non-finite")
  mapping$columnId <- c(1, 1, 3)
  expect_error(env$coefficientTable(w, mapping, TRUE), "mapping")
})

test_that("saved coefficients align with retained columns and normalization", {
  env <- coefficientRunner()
  mapping <- data.frame(covariateId = c(30, 10, 20), columnId = c(3L, 1L, 2L))
  preprocessor <- list(enabled = TRUE, keep = c(TRUE, FALSE, TRUE), normFactors = c(2, 1, 4))
  tab <- env$coefficientTable(c(-2, 0.3, 1e-15), mapping, TRUE, preprocessor)
  expect_identical(tab$covariateId, c(NA_character_, "10", "30"))
  expect_equal(tab$coefficientSharedScale, c(-2, 0.15, 1e-15 / 4), tolerance = 0)
  expect_identical(tab$selected, c(TRUE, TRUE, TRUE))
  x <- cbind(1, c(2, 4, 1), c(3, 8, 2))
  scaledX <- sweep(x, 2, tab$normalizationFactor, "/")
  expect_equal(as.numeric(scaledX %*% tab$coefficient), as.numeric(x %*% tab$coefficientSharedScale))
  expect_equal(nrow(env$coefficientTable(numeric(), mapping[FALSE, ], FALSE)), 0L)
  expect_equal(env$coefficientTable(2, mapping[FALSE, ], TRUE)$coefficient, 2)
  preprocessor$keep <- c(TRUE, FALSE)
  expect_error(env$coefficientTable(c(-2, 0.3, 1), mapping, TRUE, preprocessor), "mask")
})

test_that("resume checks a matching readable model and preserves failed combinations", {
  env <- coefficientRunner()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "taskA", fold = 1L, featureSet = "ageSex", method = "PooledLasso",
    error = NA_character_, auc = 0.7)
  check <- function(x, ...) env$isCompletedCombination(x, "taskA", 1L, "ageSex", "PooledLasso",
    rerunMissingModels = TRUE, resultDirectory = directory, ...)
  expect_false(check(rows))
  expect_true(env$isCompletedCombination(rows, "taskA", 1L, "ageSex", "PooledLasso"))
  failed <- rows
  failed$error <- "Did not converge"
  expect_true(check(failed))
  expect_false(check(failed, rerunErrors = TRUE))
  artifact <- list(task = "taskA", fold = 1L, featureSet = "ageSex", method = "PooledLasso",
    models = list(list(coefficients = env$coefficientTable(c(-1, 1e-15),
      data.frame(covariateId = 1002, columnId = 1L), TRUE))))
  savedRows <- env$saveModelArtifact(artifact, rows, file.path(directory, "models"))
  expect_true(check(savedRows))
  csv <- file.path(directory, "comparison_results.csv")
  write.csv(savedRows, csv, row.names = FALSE)
  expect_true(check(read.csv(csv)))
  model <- readRDS(file.path(directory, savedRows$modelFile))
  expect_identical(model$models[[1]]$coefficients$coefficient, c(-1, 1e-15))
  savedRows$modelId <- "stale-reference"
  expect_false(check(savedRows))
  savedRows$modelId <- model$modelId
  model$task <- "taskB"
  saveRDS(model, file.path(directory, savedRows$modelFile))
  expect_false(check(savedRows))
  saveRDS(list(), file.path(directory, savedRows$modelFile))
  expect_false(check(savedRows))
  unlink(file.path(directory, savedRows$modelFile))
  expect_false(check(savedRows))
})

test_that("baseline artifacts save pooled coefficients and actual local averaging components", {
  env <- coefficientRunner()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  makeData <- function(n) list(n = n,
    yLabels = rep(c(0, 1), length.out = n),
    xMatrix = cbind(1, seq_len(n), rep(c(0, 1), length.out = n), 0))
  clients <- list(a = makeData(4), b = makeData(6), c = makeData(8), test = makeData(4))
  local_mocked_bindings(
    loadClientData = function(path, popSettings) clients[[path]],
    getClientFeatures = function(plpData) data.frame(covariateId = c(10, 20, 30)),
    createClientMatrix = function(plpData, config) plpData,
    .package = "FederatedLearning"
  )
  env$assertCyclopsMethod <- function(method) invisible(NULL)
  env$fitBaselineWeights <- function(clientDataList, args, seed, intercept = TRUE) {
    n <- sum(vapply(clientDataList, `[[`, numeric(1), "n"))
    if (n == 6) stop("synthetic local failure")
    list(w = if (n == 4) c(-1, 2, 1e-12) else if (n == 8) c(-2, -1, 0) else c(-0.5, 0, 2),
      selectedLambda = 0.1, elapsedSeconds = 0,
      fittingSettings = list(seed = seed, selectedVariance = 0.1))
  }
  run <- function(method) env$fitBaselineFold(method,
    trainPaths = c("a", "b", "c"), testPaths = "test", popSettings = list(),
    config = list(mapType = "union", intercept = TRUE, pooledDiagnostics = FALSE),
    args = list(), task = "taskA", featureSet = "all", fold = 4L,
    trainClientIds = c("a", "b", "c"), testClientIds = "test", testClientIndexes = 4L,
    modelDirectory = file.path(directory, "models"))
  pooled <- run("PooledLasso")
  saved <- readRDS(file.path(directory, pooled$modelFile))
  expect_equal(pooled$nonzeroPredictors, 1L)
  expect_identical(saved$models[[1]]$coefficients$covariateId, c(NA_character_, "10", "20"))
  expect_equal(saved$models[[1]]$coefficients$coefficient, c(-0.5, 0, 2))
  expect_identical(saved$preprocessing$keep, c(TRUE, TRUE, FALSE))
  expect_equal(length(saved$components), 0L)
  averaged <- run("LocalAvgLasso")
  saved <- readRDS(file.path(directory, averaged$modelFile))
  expect_equal(averaged$nonzeroPredictors, 1L)
  expect_equal(saved$models[[1]]$coefficients$coefficient, c(-5 / 3, 0, 1e-12 / 3))
  expect_equal(vapply(saved$components, `[[`, numeric(1), "weight"), c(1 / 3, 2 / 3))
  expect_equal(vapply(saved$components, `[[`, character(1), "sourceClientId"), c("a", "c"))
  expect_equal(saved$localFailures[[1]]$trainClientId, "b")
  expect_equal(saved$localFailures[[1]]$error, "synthetic local failure")
  local <- run("LocalSiteLasso")
  expect_equal(nrow(local), 3L)
  expect_equal(sum(!is.na(local$error)), 1L)
  expect_true(env$isCompletedCombination(local, "taskA", 4L, "all", "LocalSiteLasso",
    rerunMissingModels = TRUE, resultDirectory = directory))
  ensemble <- run("LocalEnsembleLasso")
  saved <- readRDS(file.path(directory, ensemble$modelFile))
  expect_true(is.na(ensemble$nonzeroPredictors))
  expect_equal(length(saved$models), 2L)
  expect_match(saved$aggregation, "predictionAverage")
})

test_that("coefficient export reads model files and marks unavailable models explicitly", {
  env <- coefficientRunner()
  script <- new.env(parent = globalenv())
  sys.source(coefficientExtrasPath("summarizeCoefficients.R"), script)
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "taskA", fold = 1L, featureSet = "ageSex", method = "PooledLasso",
    error = NA_character_, auc = 0.7, selectedLambda = 0.01)
  artifact <- list(task = "taskA", fold = 1L, featureSet = "ageSex", method = "PooledLasso",
    models = list(list(coefficients = env$coefficientTable(c(-2, 0, 1e-20),
      data.frame(covariateId = c(10, 20), columnId = 1:2), TRUE))))
  rows <- env$saveModelArtifact(artifact, rows, file.path(directory, "models"))
  absent <- rows
  absent$method <- "LocalAvgLasso"
  absent$modelFile <- NA_character_
  absent$modelId <- NA_character_
  write.csv(rbind(rows, absent), file.path(directory, "comparison_results.csv"), row.names = FALSE)
  reads <- 0L
  reader <- FederatedLearning:::readModelArtifact
  readerEnv <- new.env(parent = environment(reader))
  readerEnv$readRDS <- function(path) {
    reads <<- reads + 1L
    base::readRDS(path)
  }
  environment(reader) <- readerEnv
  local_mocked_bindings(readModelArtifact = reader, .package = "FederatedLearning")
  result <- script$summarizeCoefficients(directory)
  expect_equal(reads, 1L)
  expect_equal(result$summary$status, c("saved_model", "missing_model"))
  expect_equal(result$summary$nonzeroPredictors, c(1L, NA_integer_))
  expect_identical(result$coefficients$coefficient, c(-2, 0, 1e-20))
  expect_true(file.exists(file.path(directory, "coefficient_summary", "coefficients.csv")))
})

test_that("federated artifacts contain exact support and task-specific lambda paths", {
  env <- coefficientRunner()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  config <- list(intercept = TRUE, mapping = data.frame(covariateId = c(10, 20), columnId = 1:2),
    lambda = 0.01, rounds = 10L, trainClientPaths = c("a", "b"))
  local_mocked_bindings(
    fitFederated = function(cl, algorithm, config, verbose) list(w = c(-2, 0, 1e-20), config = config,
      roundsCompleted = 3L, lambdaSeq = c(0.01, 0.1), cvScores = c(0.7, 0.6)),
    clusterCreateMatrices = function(cl, config) NULL,
    clusterEvaluateModel = function(cl, w) data.frame(client = 1L, auc = 0.7, n = 10, outcomes = 2),
    .package = "FederatedLearning"
  )
  run <- function(task) env$fitFederatedFold("DualAvg", NULL, NULL, config, directory,
    task, "all", 1L, "test", FALSE, modelDirectory = file.path(directory, "models"),
    provenance = list(populationSettings = list(riskWindowEnd = 365L)))
  first <- run("taskA")
  second <- run("taskB")
  expect_false(identical(first$lambdaPathFile, second$lambdaPathFile))
  expect_true(all(file.exists(c(first$lambdaPathFile, second$lambdaPathFile))))
  saved <- readRDS(file.path(directory, first$modelFile))
  expect_equal(first$nonzeroPredictors, 1L)
  expect_identical(saved$models[[1]]$coefficients$coefficient, c(-2, 0, 1e-20))
  expect_equal(saved$roundsCompleted, 3L)
  expect_equal(saved$provenance$populationSettings$riskWindowEnd, 365L)
  expect_equal(saved$lambdaSeq, c(0.01, 0.1))
  blocked <- file.path(directory, "blocked")
  file.create(blocked)
  failedSave <- env$fitFederatedFold("DualAvg", NULL, NULL, config, directory,
    "taskA", "all", 1L, "test", FALSE, modelDirectory = file.path(blocked, "models"))
  expect_equal(failedSave$auc, 0.7)
  expect_equal(failedSave$nonzeroPredictors, 1L)
  expect_true(is.na(failedSave$error))
  expect_match(failedSave$modelSaveError, "Could not create model directory")
})

test_that("runner reruns only successful combinations with missing artifacts", {
  env <- coefficientRunner()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (id in c("a", "b", "c")) dir.create(file.path(directory, "data", "taskA", id), recursive = TRUE)
  resultDir <- file.path(directory, "results")
  dir.create(resultDir)
  args <- list("data-root" = file.path(directory, "data"), "result-directory" = resultDir,
    tasks = "taskA", "feature-sets" = "all", methods = "PooledLasso,LocalAvgLasso",
    "client-ids" = "a,b,c", folds = "1", "rerun-missing-models" = "true")
  rows <- data.frame(task = "taskA", fold = 1L, featureSet = "all",
    method = c("PooledLasso", "LocalAvgLasso"), error = NA_character_, auc = 0.7,
    logLoss = 0.4, calibrationIntercept = 0, calibrationSlope = 1, elapsedSeconds = 0,
    messages = 0, numbers = 0)
  write.csv(rows, file.path(resultDir, "comparison_results.csv"), row.names = FALSE)
  calls <- character()
  mapping <- data.frame(covariateId = 10, columnId = 1L)
  env$fitBaselineFold <- function(method, trainPaths, testPaths, popSettings, config, args,
      task, featureSet, fold, trainClientIds, testClientIds, testClientIndexes, modelDirectory) {
    calls <<- c(calls, method)
    env$saveModelArtifact(list(task = task, fold = fold, featureSet = featureSet, method = method,
      models = list(list(coefficients = env$coefficientTable(c(-1, 0.3), mapping, TRUE)))),
      rows[rows$method == method, ], modelDirectory)
  }
  env$safeStopCluster <- function(cl) NULL
  local_mocked_bindings(
    clusterInit = function(clientHosts, clientPaths, mirai) list(),
    clusterLoadData = function(cl, clientPaths, popSettings) rep(10, length(clientPaths)),
    clusterCollectCovRefs = function(...) mapping,
    clusterCreateMatrices = function(...) NULL,
    clusterDiagnostics = function(...) data.frame(client = 1L, n = 10),
    .package = "FederatedLearning"
  )
  skip_if_not_installed("PatientLevelPrediction")
  # A failed artifact write must not turn the completed fit into an NA result.
  file.create(file.path(resultDir, "models"))
  env$runComparison(args)
  expect_equal(calls, c("PooledLasso", "LocalAvgLasso"))
  failedSave <- read.csv(file.path(resultDir, "comparison_results.csv"))
  expect_equal(failedSave$auc, c(0.7, 0.7))
  expect_true(all(is.na(failedSave$error)))
  expect_true(all(!is.na(failedSave$modelSaveError)))
  expect_true(all(is.na(failedSave$modelFile)))
  unlink(file.path(resultDir, "models"))
  env$runComparison(args)
  expect_equal(calls, rep(c("PooledLasso", "LocalAvgLasso"), 2))
  env$runComparison(args)
  expect_equal(length(calls), 4L)
  current <- read.csv(file.path(resultDir, "comparison_results.csv"))
  expect_true(all(is.na(current$modelSaveError)))
  unlink(file.path(resultDir, current$modelFile[current$method == "LocalAvgLasso"]))
  env$runComparison(args)
  expect_equal(calls, c(rep(c("PooledLasso", "LocalAvgLasso"), 2), "LocalAvgLasso"))
  args[["save-models"]] <- "false"
  expect_error(env$runComparison(args), "requires --save-models")
})

test_that("actual Cyclops coefficients round-trip with no support threshold", {
  skip_if_not_installed("Cyclops")
  env <- coefficientRunner()
  set.seed(813)
  x <- Matrix::Matrix(cbind(1, matrix(rnorm(400), 100, 4)), sparse = TRUE)
  fit <- env$fitCyclopsWeights(list(list(xMatrix = x, yLabels = as.integer(x[, 2] > 0.5))),
    args = list("cyclops-cv" = "false", "cyclops-variance" = "0.1"), seed = 42L)
  expect_true(all(is.finite(fit$w)))
  expect_true(any(fit$w[-1L] == 0))
  tab <- env$coefficientTable(fit$w, data.frame(covariateId = 101:104, columnId = 1:4), TRUE)
  expect_identical(tab$coefficient, as.numeric(fit$w))
  expect_identical(tab$selected, as.numeric(fit$w) != 0)
  expect_equal(fit$fittingSettings$selectedVariance, 0.1)
  expect_equal(fit$fittingSettings$returnFlag, "SUCCESS")
  expect_false(fit$fittingSettings$useCrossValidation)
})

test_that("legacy debug exports reject stale evaluations", {
  env <- coefficientRunner()
  script <- new.env(parent = globalenv())
  sys.source(coefficientExtrasPath("summarizeCoefficients.R"), script)
  directory <- tempfile()
  dir.create(file.path(directory, "debug"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  row <- data.frame(task = "taskA", fold = 1L, featureSet = "ageSex", method = "DualAvg",
    auc = 0.7, n = 10L, outcomes = 2L, clientId = "test", p = 2L, selectedLambda = 1e-12)
  legacy <- c(as.list(row[, c("task", "fold", "featureSet", "method")]), list(
    selectedLambda = 1e-12,
    evaluation = row[, c("auc", "n", "outcomes", "clientId")],
    config = list(mapping = data.frame(covariateId = 10, columnId = 1L), intercept = TRUE,
      lambdaSearchBestTrain = 1e-7), coefficients = c(-1, 1e-15)))
  path <- env$debugPath(file.path(directory, "debug"), "taskA", 1L, "ageSex", "DualAvg", "fit")
  saveRDS(legacy, path)
  write.csv(row, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  run <- function() script$summarizeCoefficients(directory)
  result <- run()
  expect_equal(result$summary$status, "saved_debug")
  expect_equal(result$summary$nonzeroPredictors, 1L)
  expect_true(is.na(result$summary$selectedVariance))
  expect_false(env$isCompletedCombination(row, "taskA", 1L, "ageSex", "DualAvg",
    rerunMissingModels = TRUE, resultDirectory = directory))
  legacy$config$lambdaSearchTrace <- data.frame(searchValue = 0.01, fitLambda = 1e-7)
  saveRDS(legacy, path)
  result <- run()
  expect_true(is.na(result$cvFolds$selected))
  expect_equal(result$cvFolds$finalLambda, row$selectedLambda)
  legacy$evaluation$auc <- 0.6
  saveRDS(legacy, path)
  result <- run()
  expect_equal(result$summary$status, "stale_or_invalid_debug")
  expect_true(is.na(result$summary$nonzeroPredictors))
  expect_equal(nrow(result$coefficients), 0L)
  expect_false(env$isCompletedCombination(row, "taskA", 1L, "ageSex", "DualAvg",
    rerunMissingModels = TRUE, resultDirectory = directory))
})

test_that("coefficient export retains the selected variance and unequal-fold lambda trace", {
  script <- new.env(parent = globalenv())
  path <- coefficientExtrasPath("summarizeCoefficients.R")
  sys.source(path, script)
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- data.frame(task = "taskA", fold = 4L, featureSet = "ageSex", method = "DualAvg",
    error = NA_character_, auc = 0.7, selectedLambda = sqrt(200) / 100)
  trace <- data.frame(iteration = rep(0:1, each = 3), validationClient = rep(1:3, 2),
    searchScale = "priorVariance", searchValue = rep(c(0.01, 0.02), each = 3),
    trainingRows = rep(c(90, 80, 30), 2), auc = rep(c(0.7, 0.6), each = 3))
  trace$fitLambda <- sqrt(2 / trace$searchValue) / trace$trainingRows
  artifact <- c(as.list(rows[c("task", "fold", "featureSet", "method")]), list(
    config = list(lambda = rows$selectedLambda, selectedVariance = 0.01, lambdaSearchTrace = trace),
    models = list(list(coefficients = FederatedLearning:::coefficientTable(c(-1, 0.1),
      data.frame(covariateId = 1002, columnId = 1L), TRUE)))))
  rows <- FederatedLearning:::saveModelArtifact(artifact, rows, file.path(directory, "models"))
  write.csv(rows, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  result <- script$summarizeCoefficients(directory)
  expect_equal(result$summary$selectedVariance, 0.01)
  expect_equal(result$summary$selectedLambda, sqrt(200) / 100)
  expect_false(any(c("innerCvFitLambda", "finalToInnerCvLambdaRatio") %in% names(result$summary)))
  expect_equal(result$cvFolds$selected, rep(c(TRUE, FALSE), each = 3))
  chosen <- result$cvFolds[result$cvFolds$selected, ]
  expect_equal(chosen$fitLambda, sqrt(200) / c(90, 80, 30))
  expect_equal(chosen$finalLambda / chosen$fitLambda, c(0.9, 0.8, 0.3))
  expect_equal(nrow(read.csv(file.path(directory, "coefficient_summary", "lambda_search_folds.csv"))), 6L)

  # Running the standalone CLI from outside the repository must not source the runner.
  old <- setwd(directory)
  on.exit(setwd(old), add = TRUE)
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(path), shQuote(paste0("--result-directory=", directory)),
      "--methods=DualAvg", shQuote(paste0("--output-directory=", file.path(directory, "cli=output")))),
    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(file.exists(file.path(directory, "cli=output", "lambda_search_folds.csv")))
})
