savedAuditFixture <- function(directory) {
  mapping <- data.frame(covariateId = c(1002, 1152049), columnId = 1:2)
  settings <- list(enabled = TRUE, minFraction = 0.25, normalize = FALSE, removeRedundancy = TRUE)
  rows <- lapply(seq_along(c(30L, 365L)), function(i) {
    task <- paste0("task", i)
    train <- file.path(directory, "data", task, "train")
    dir.create(train, recursive = TRUE)
    artifact <- list(task = task, fold = 1L, featureSet = "ageSexPhenotypes", method = "PooledLasso",
      config = list(mapType = "union", intercept = TRUE, diagnosticControlsPerCase = 2,
        diagnosticDownsampleSeed = 47L),
      populationSettings = list(riskWindowStart = 1L, riskWindowEnd = c(30L, 365L)[i]),
      trainPaths = train, testPaths = "held-out-path-must-not-be-opened",
      trainClientIds = "train", testClientIds = "test", trainingSampleSizes = c(train = 4L),
      originalMapping = mapping, preprocessing = list(settings = settings, keep = c(TRUE, FALSE)),
      models = list(list(coefficients = coefficientTable(c(-1, 0.3), mapping[1, ], TRUE))))
    row <- data.frame(task = task, fold = 1L, featureSet = "ageSexPhenotypes", method = "PooledLasso")
    saveModelArtifact(artifact, row, file.path(directory, "models"))
  })
  rows <- do.call(rbind, rows)
  utils::write.csv(rows, file.path(directory, "comparison_results.csv"), row.names = FALSE)
  list(rows = rows, mapping = mapping, settings = settings)
}

test_that("one-command audit restores saved horizons and uses training sites only", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- savedAuditFixture(directory)
  collected <- loaded <- list()
  stopped <- 0L
  local_mocked_bindings(
    clusterInit = function(clientHosts, clientPaths, mirai) {
      expect_identical(clientHosts, "localhost")
      expect_false(mirai)
      expect_identical(basename(clientPaths), "train")
      list()
    },
    clusterLoadData = function(cl, clientPaths, popSettings) {
      loaded[[length(loaded) + 1L]] <<- popSettings
    },
    collectTrainingFeatureFilter = function(cl, config, settings) {
      collected[[length(collected) + 1L]] <<- config
      expect_identical(settings, f$settings)
      preprocessorFromSummaries(list(list(n = 4L, nnz = c(4, 0), sum = c(1, 0),
        sumSquares = c(0.3, 0))), f$mapping, settings)
    },
    stopCluster = function(cl) stopped <<- stopped + 1L,
    fitFederated = function(...) stop("Must not fit"),
    .package = "FederatedLearning")
  before <- tools::md5sum(file.path(directory, c("comparison_results.csv", f$rows$modelFile)))
  result <- auditMatchedPreprocessing(directory)
  expect_equal(result$referenceStatus, rep("matched", 2))
  expect_equal(result$riskWindowEnd, c(30L, 365L))
  expect_equal(vapply(loaded, `[[`, integer(1), "riskWindowEnd"), c(30L, 365L))
  expect_true(all(vapply(collected, function(x) x$diagnosticControlsPerCase == 2 &&
    x$diagnosticDownsampleSeed == 47L && x$featureSet == "ageSexPhenotypes", logical(1))))
  expect_true(all(result$requiresRefit))
  expect_equal(stopped, 2L)
  expect_identical(before, tools::md5sum(names(before)))
  expect_equal(nrow(utils::read.csv(file.path(directory, "preprocessing-audit", "summary.csv"))), 2L)
  expect_equal(nrow(utils::read.csv(file.path(directory, "preprocessing-audit", "features.csv"))), 4L)
  one <- auditMatchedPreprocessing(directory, tasks = "task2", folds = 1)
  expect_identical(one$task, "task2")
  expect_error(auditMatchedPreprocessing(directory, tasks = "missing"), "no pooled results")
  expect_error(auditMatchedPreprocessing(directory, outputDirectory = directory), "separate")
})

test_that("audit records missing models, mismatches and local errors without fitting", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- savedAuditFixture(directory)
  unlink(file.path(directory, f$rows$modelFile[1]))
  local_mocked_bindings(auditSavedPreprocessing = function(model, row, dataRoot) {
    preprocessorFromSummaries(list(list(n = 4L, nnz = c(4, 2), sum = c(1, 2),
      sumSquares = c(0.3, 2))), f$mapping, f$settings)
  }, .package = "FederatedLearning")
  result <- auditMatchedPreprocessing(directory)
  expect_identical(result$referenceStatus, c("error", "mismatch"))
  expect_match(result$error[1], "missing")
  expect_match(result$error[2], "mask")
  expect_true(file.exists(file.path(directory, "preprocessing-audit", "summary.csv")))
  expect_true(is.na(result$requiresRefit[1]))
})

test_that("audit supports moved data and closes workers after collection errors", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- savedAuditFixture(directory)
  model <- readRDS(file.path(directory, f$rows$modelFile[1]))
  model$trainPaths <- "old-missing-data-location"
  expect_error(auditSavedPreprocessing(model, f$rows[1, ], NULL), "dataRoot")
  stopped <- FALSE
  local_mocked_bindings(
    clusterInit = function(clientHosts, clientPaths, mirai) {
      expect_identical(clientPaths, file.path(directory, "data", "task1", "train"))
      list()
    },
    clusterLoadData = function(...) NULL,
    collectTrainingFeatureFilter = function(...) stop("collection failed"),
    stopCluster = function(cl) stopped <<- TRUE,
    .package = "FederatedLearning")
  expect_error(auditSavedPreprocessing(model, f$rows[1, ], file.path(directory, "data")), "collection failed")
  expect_true(stopped)
  model$preprocessing$settings$normalize <- TRUE
  expect_error(auditSavedPreprocessing(model, f$rows[1, ], file.path(directory, "data")), "normalization")
})
