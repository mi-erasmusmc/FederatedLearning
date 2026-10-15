loadPredictionAudit <- function() {
  path <- test_path("..", "..", "extras", "auditSavedSurrogatePredictions.R")
  skip_if_not(file.exists(path), "Audit script is not available in this test installation")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

predictionAuditFixture <- function(directory) {
  mapping <- data.frame(covariateId = 1002, columnId = 1L)
  rows <- lapply(1:2, function(i) {
    task <- paste0("task", i)
    path <- file.path(directory, "data", task, "test")
    dir.create(path, recursive = TRUE)
    config <- list(mapping = mapping, intercept = TRUE, p = 1L,
      diagnosticControlsPerCase = Inf, diagnosticDownsampleSeed = 47L)
    artifact <- list(task = task, fold = 1L, featureSet = "ageSexPhenotypes", method = "ODAL2",
      models = list(list(coefficients = coefficientTable(c(40, 1), mapping, TRUE))),
      config = config, testClientIds = "test",
      provenance = list(trainClientIds = "train", testClientIds = "test",
        trainPaths = "training-path-must-not-be-loaded", testPaths = path,
        populationSettings = list(riskWindowStart = 1L, riskWindowEnd = c(30L, 365L)[i])))
    row <- data.frame(task = task, fold = 1L, featureSet = "ageSexPhenotypes", method = "ODAL2",
      clientId = "test", n = 4L, outcomes = 2L, auc = 0.5)
    saveModelArtifact(artifact, row, file.path(directory, "models"))
  })
  rows <- do.call(rbind, rows)
  utils::write.csv(rows, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  rows
}

test_that("scoring audit distinguishes saturation, ties and reversed rankings", {
  env <- loadPredictionAudit()
  tab <- coefficientTable(c(40, 1), data.frame(covariateId = 1002, columnId = 1L), TRUE)
  data <- list(xMatrix = cbind(1, 0:3), yLabels = c(0, 0, 1, 1), n = 4L)
  result <- env$.surrogatePredictionSummary(data, tab)
  expect_equal(result$etaAuc, 1)
  expect_equal(result$probabilityAuc, 0.5)
  expect_equal(result$etaDistinct, 4L)
  expect_equal(result$probabilityDistinct, 1L)
  expect_equal(result$probabilityOneFraction, 1)
  expect_equal(result$meanLogLoss, mean(c(40, 41, 0, 0)), tolerance = 1e-14)
  expect_identical(result$coefficientMaxId, "(Intercept)")
  data$xMatrix <- Matrix::Matrix(data$xMatrix, sparse = TRUE)
  expect_equal(env$.surrogatePredictionSummary(data, tab), result)

  tab$coefficient <- c(0, -1)
  inverted <- env$.surrogatePredictionSummary(data, tab)
  expect_equal(inverted$etaAuc, 0)
  expect_equal(inverted$probabilityAuc, 0)
  expect_identical(inverted$coefficientMaxId, "1002")
  data$yLabels <- rep(0, 4)
  expect_true(is.na(env$.surrogatePredictionSummary(data, tab)$etaAuc))
  data$n <- 5L
  expect_error(env$.surrogatePredictionSummary(data, tab), "sample-count")
  data$n <- 4L
  data$xMatrix[1, 2] <- Inf
  overflow <- env$.surrogatePredictionSummary(data, tab)
  expect_equal(overflow$etaNonFinite, 1L)
  expect_true(is.na(overflow$probabilityAuc))
  expect_true(is.na(overflow$etaAuc))
  data$yLabels[1] <- NA
  expect_error(env$.surrogatePredictionSummary(data, tab), "label")
})

test_that("scoring audit checks artifacts before reconstructing only held-out matrices", {
  env <- loadPredictionAudit()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- predictionAuditFixture(directory)
  files <- file.path(directory, c("comparison_results.csv", rows$modelFile))
  before <- tools::md5sum(files)
  loaded <- list()
  env$.surrogatePredictionSite <- function(inputs) {
    loaded[[length(loaded) + 1L]] <<- inputs
    env$.surrogatePredictionSummary(
      list(xMatrix = cbind(1, 0:3), yLabels = c(0, 0, 1, 1), n = 4L), inputs$coefficients)
  }
  local_mocked_bindings(fitFederated = function(...) stop("Must not fit"),
    .package = "FederatedLearning")
  result <- env$auditSavedSurrogatePredictions(directory)
  expect_equal(result$status, rep("rescored", 2))
  expect_true(all(result$countsMatch))
  expect_equal(result$etaMinusProbabilityAuc, rep(0.5, 2))
  expect_equal(result$probabilityAucDifference, rep(0, 2))
  expect_equal(result$riskWindowEnd, c(30L, 365L))
  expect_true(all(vapply(loaded, function(x) basename(x$path) == "test", logical(1))))
  expect_true(all(vapply(loaded, function(x) is.infinite(x$config$diagnosticControlsPerCase) &&
    x$config$diagnosticDownsampleSeed == 47L && x$config$p == 1L, logical(1))))
  expect_true(all(result$historicalDataIdentity == "unverified_no_saved_data_hash"))
  expect_true(all(result$frozenSurrogate == "unavailable_not_saved"))
  expect_identical(before, tools::md5sum(files))
  output <- file.path(directory, "surrogate-prediction-audit")
  expect_setequal(list.files(output), c("summary.csv", "code-fingerprints.csv", "session-info.txt", "README.txt"))
  saved <- utils::read.csv(file.path(output, "summary.csv"))
  expect_equal(saved$etaAuc, c(1, 1))
  expect_false(any(c("eta", "predictions", "yLabels", "xMatrix", "rowId", "subjectId") %in% names(saved)))
  expect_error(env$auditSavedSurrogatePredictions(directory), "new output")
  expect_error(env$auditSavedSurrogatePredictions(directory, outputDirectory = directory), "new output")
  expect_error(env$auditSavedSurrogatePredictions(directory, tempfile(), methods = "LocalAvgLasso"), "supported")
  expect_error(env$auditSavedSurrogatePredictions(directory, tempfile(), tasks = "missing"), "No matching")

  one <- env$auditSavedSurrogatePredictions(directory, file.path(directory, "one"), tasks = "task2", folds = 1)
  expect_identical(one$task, "task2")
})

test_that("scoring audit refuses mismatched metadata and supports relocated test data", {
  env <- loadPredictionAudit()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- predictionAuditFixture(directory)
  model <- readRDS(file.path(directory, rows$modelFile[1]))
  original <- model
  model$provenance$testPaths <- "old-path"
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "dataRoot")
  moved <- env$.surrogatePredictionInputs(model, rows[1, ], file.path(directory, "data"))
  expect_identical(moved$path, original$provenance$testPaths)

  model <- original
  model$models[[1]]$coefficients$covariateId[2] <- "wrong-feature"
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "feature order")
  model <- original
  model$models[[1]]$coefficients$normalizationFactor[2] <- 2
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "scale")
  model <- original
  model$provenance$trainClientIds <- "test"
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "identities")
  model <- original
  model$provenance$populationSettings <- NULL
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "population settings")
  model <- original
  model$results$n <- 20L
  expect_error(env$.surrogatePredictionInputs(model, rows[1, ], NULL), "disagree")
  model <- original
  model$results$auc <- 0.5000000000000001
  expect_silent(env$.surrogatePredictionInputs(model, rows[1, ], NULL))
  large <- rows[1, ]
  large$n <- model$results$n <- 1e9
  large$auc <- 0.500001
  expect_error(env$.surrogatePredictionInputs(model, large, NULL), "disagree")
})

test_that("scoring audit preserves partial results and reports changed counts or missing artifacts", {
  env <- loadPredictionAudit()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- predictionAuditFixture(directory)
  unlink(file.path(directory, rows$modelFile[1]))
  env$.surrogatePredictionSite <- function(inputs) {
    env$.surrogatePredictionSummary(
      list(xMatrix = cbind(1, 0:4), yLabels = c(0, 0, 1, 1, 1), n = 5L), inputs$coefficients)
  }
  result <- env$auditSavedSurrogatePredictions(directory)
  expect_identical(result$status, c("error", "count_mismatch"))
  expect_match(result$error[1], "missing")
  expect_false(result$countsMatch[2])
  expect_true(is.na(result$etaAuc[1]))
  expect_true(file.exists(file.path(directory, "surrogate-prediction-audit", "summary.csv")))
})

test_that("scoring audit shuts down the worker after matrix construction fails", {
  env <- loadPredictionAudit()
  stopped <- FALSE
  local_mocked_bindings(
    clusterInit = function(clientHosts, clientPaths, mirai) {
      expect_identical(clientHosts, "localhost")
      expect_false(mirai)
      list()
    },
    clusterLoadData = function(cl, clientPaths, popSettings) NULL,
    clusterCreateMatrices = function(cl, config) stop("matrix construction failed"),
    .package = "FederatedLearning")
  local_mocked_bindings(stopCluster = function(cl) stopped <<- TRUE, .package = "parallel")
  expect_error(env$.surrogatePredictionSite(list(path = "test")), "matrix construction failed")
  expect_true(stopped)
})

test_that("scoring summary executes in an isolated PSOCK worker and returns aggregates only", {
  env <- loadPredictionAudit()
  local_mocked_bindings(
    clusterLoadData = function(cl, clientPaths, popSettings) NULL,
    clusterCreateMatrices = function(cl, config) {
      parallel::clusterCall(cl, function() {
        data <- list(xMatrix = Matrix::Matrix(cbind(1, 0:3), sparse = TRUE),
          yLabels = c(0, 0, 1, 1), n = 4L)
        assign("clientData", data, envir = .GlobalEnv)
        NULL
      })
    }, .package = "FederatedLearning")
  tab <- coefficientTable(c(40, 1), data.frame(covariateId = 1002, columnId = 1L), TRUE)
  scores <- env$.surrogatePredictionSite(list(path = "synthetic", coefficients = tab))
  expect_s3_class(scores, "data.frame")
  expect_equal(nrow(scores), 1L)
  expect_equal(scores$etaAuc, 1)
  expect_equal(scores$probabilityAuc, 0.5)
  expect_false(any(vapply(scores, is.list, logical(1))))
})
