manifestSettings <- function(lambda = 0.1) list(
  taskSettings = list(name = "taskA"), sites = c(a = "pathA", b = "pathB"),
  featureSettings = list(ageSex = list()), methodSettings = list(list(method = "PooledLasso", options = list(lambda = lambda)),
    list(method = "DualAvg", options = list())), holdouts = "a", resume = TRUE, runSettings = list(mirai = TRUE))

test_that("each run adds a manifest row with versions, settings hash and finish time", {
  directory <- tempfile("manifest-")
  id <- .startRunManifest(directory, manifestSettings())
  rows <- .readRunManifest(file.path(directory, "run_manifest.csv"))
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$manifestId, id)
  expect_identical(rows$task, "taskA")
  expect_identical(rows$methods, "PooledLasso,DualAvg")
  expect_identical(rows$featureSets, "ageSex")
  expect_true(startsWith(rows$framework, as.character(utils::packageVersion("FederatedLearning"))))
  expect_identical(rows$settingsHash, .settingsHash(manifestSettings()))
  expect_true(is.na(rows$finishedAt))
  saved <- readRDS(file.path(directory, "manifest", paste0(id, ".rds")))
  expect_identical(saved$settings, manifestSettings())
  .finishRunManifest(directory, id)
  expect_false(is.na(.readRunManifest(file.path(directory, "run_manifest.csv"))$finishedAt))
})

test_that("resuming with other settings warns, the same settings do not", {
  directory <- tempfile("manifest-")
  .startRunManifest(directory, manifestSettings())
  expect_no_warning(.startRunManifest(directory, manifestSettings()))
  expect_warning(.startRunManifest(directory, manifestSettings(lambda = 0.2)), "different settings")
  expect_identical(nrow(.readRunManifest(file.path(directory, "run_manifest.csv"))), 3L)
})

test_that("a different framework version is flagged", {
  previous <- data.frame(task = "taskA", framework = "0.3.0@aaaa", study = NA_character_, settingsHash = "x",
    startedAt = "2026-10-27 10:00:00 UTC", stringsAsFactors = FALSE)
  row <- previous
  row$framework <- "0.3.0@bbbb"
  expect_warning(.warnManifestChanges(previous, row), "different framework")
  expect_no_warning(.warnManifestChanges(previous, previous))
})
