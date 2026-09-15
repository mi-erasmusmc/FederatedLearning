modelArtifactFixture <- function() {
  rows <- data.frame(task = "taskA", fold = 1L, featureSet = "all", method = "PooledLasso",
    auc = 0.7, logLoss = 0.3, elapsedSeconds = 10, error = NA_character_)
  artifact <- c(as.list(rows[c("task", "fold", "featureSet", "method")]), list(
    models = list(list(coefficients = coefficientTable(c(-1, 0.1, 0),
      data.frame(covariateId = c(1002, 8507), columnId = 1:2), TRUE)))))
  list(rows = rows, artifact = artifact)
}

test_that("coefficient IDs survive integer64 conversion, RDS and CSV round trips", {
  skip_if_not_installed("bit64")
  ids <- c("1002", "9007199254740993", "9223372036854775806")
  mapping <- data.frame(covariateId = bit64::as.integer64(ids), columnId = 1:3)
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  fixture <- modelArtifactFixture()
  for (intercept in c(TRUE, FALSE)) {
    tab <- coefficientTable(c(if (intercept) -1, 0.1, 0, -0.2), mapping, intercept)
    expect_identical(tab$covariateId, c(if (intercept) NA_character_, ids))
    fixture$artifact$models[[1]]$coefficients <- tab
    rows <- saveModelArtifact(fixture$artifact, fixture$rows, file.path(directory, "models"))
    saved <- readModelArtifact(rows, directory, "taskA", 1L, "all", "PooledLasso")
    expect_identical(saved$models[[1]]$coefficients$covariateId, tab$covariateId)
    path <- file.path(directory, "coefficients.csv")
    write.csv(tab, path, row.names = FALSE)
    # CSV readers must treat identifiers as strings, not floating-point values.
    expect_identical(read.csv(path, colClasses = c(covariateId = "character"))$covariateId, tab$covariateId)
  }
  trimmed <- coefficientTable(c(-1, 0.1, -0.2), mapping, TRUE,
    list(enabled = TRUE, keep = c(TRUE, FALSE, TRUE), normFactors = c(1, 1, 1)))
  expect_identical(trimmed$covariateId, c(NA_character_, ids[c(1, 3)]))
})

test_that("write and rename errors preserve fitted metrics and remove temporary artifacts", {
  fixture <- modelArtifactFixture()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (operation in c("saveRDS", "file.rename")) {
    writer <- saveModelArtifact
    env <- new.env(parent = environment(writer))
    env[[operation]] <- if (operation == "saveRDS") function(...) stop("write failed") else function(...) FALSE
    environment(writer) <- env
    rows <- writer(fixture$artifact, fixture$rows, directory)
    expect_identical(rows[names(fixture$rows)], fixture$rows)
    expect_true(is.na(rows$modelFile))
    expect_true(is.na(rows$modelId))
    expect_match(rows$modelSaveError, if (operation == "saveRDS") "write failed" else "Could not save")
    expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
  }
  expect_identical(saveModelArtifact(stop("must not be evaluated"), fixture$rows, NULL), fixture$rows)
})

test_that("successful writes return the same validated artifact from a custom subdirectory", {
  fixture <- modelArtifactFixture()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  rows <- saveModelArtifact(fixture$artifact, fixture$rows, file.path(directory, "fits"))
  expect_true(is.na(rows$modelSaveError))
  expect_match(rows$modelFile, "^fits")
  saved <- readModelArtifact(rows, directory, "taskA", 1L, "all", "PooledLasso")
  expect_identical(saved$results$auc, fixture$rows$auc)
  expect_identical(saved$modelId, rows$modelId)
  expect_null(readModelArtifact(rows, directory, "otherTask", 1L, "all", "PooledLasso"))
})
