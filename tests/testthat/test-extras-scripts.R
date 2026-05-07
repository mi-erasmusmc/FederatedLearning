extrasPath <- function(file) {
  candidates <- file.path(
    c(
      getwd(),
      dirname(testthat::test_path()),
      file.path(dirname(testthat::test_path()), ".."),
      file.path(dirname(testthat::test_path()), "..", "..")
    ),
    "extras",
    file
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    testthat::skip(paste0("extras/", file, " is not available in this test installation"))
  }
  normalizePath(hit[[1]], mustWork = TRUE)
}

test_that("task manifest template encodes generic external task structure", {
  fetchEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("fetchTaskData.R"), fetchEnv)

  manifest <- fetchEnv$readManifest(extrasPath("task_manifest_template.csv"))

  expect_equal(nrow(manifest), 4L)
  expect_equal(sort(unique(manifest$clientId)), c("databaseA", "databaseB"))
  expect_true(all(manifest$removeSubjectsWithPriorOutcome == "true"))
  expect_true(all(manifest$priorOutcomeLookback == 99999L))

  taskRows <- manifest[!duplicated(manifest$task), ]
  expect_equal(taskRows$task, c("taskA", "taskB"))
  expect_equal(taskRows$riskWindowEnd, c(365L, 30L))
  expect_true(all(taskRows$targetId == "<TARGET_COHORT_ID>"))
  expect_true(all(taskRows$outcomeId == "<OUTCOME_COHORT_ID>"))
})

test_that("fetch task helpers expand cohort ranges and keep one row per cohort", {
  fetchEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("fetchTaskData.R"), fetchEnv)

  expect_equal(fetchEnv$csvValues("100:102,200"), c("100", "101", "102", "200"))

  row <- data.frame(
    targetId = 300L,
    outcomeId = 200L,
    covariateCohortIds = "100:102",
    stringsAsFactors = FALSE
  )
  expect_equal(fetchEnv$cohortIdsForRow(row), c(100L, 101L, 102L, 200L, 300L))

  cohortRow <- data.frame(
    cohortId = 1L,
    cohortName = "cohort_1",
    sql = paste(c("select 1", "select 2"), collapse = "\n"),
    json = paste(c("{", "}"), collapse = "\n"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(cohortRow), 1L)
  expect_equal(cohortRow$sql, "select 1\nselect 2")
})

test_that("comparison runner helpers parse external comparison settings", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  args <- runnerEnv$parseArgs(c(
    "--tasks=taskA,taskB",
    "--client-ids=databaseA,databaseB,databaseC,databaseD,databaseE",
    "--folds=2:4",
    "--remove-prior-outcomes=true",
    "--prior-outcome-lookback=99999",
    "--pda-rounds=3",
    "--adapdiag-style=pda"
  ))

  expect_equal(runnerEnv$csvArg(args[["client-ids"]]), c("databaseA", "databaseB", "databaseC", "databaseD", "databaseE"))
  expect_equal(runnerEnv$foldArg(args[["folds"]], 5L), 2:4)
  expect_true(runnerEnv$logicalArg(args[["remove-prior-outcomes"]], FALSE))
  expect_equal(runnerEnv$intArg(args[["prior-outcome-lookback"]], 1L), 99999L)

  cfg <- runnerEnv$methodConfig("ADAPDiag", "ageSexPhenotypes", args)
  expect_equal(cfg$rounds, 3L)
  expect_equal(cfg$adapDiagStyle, "pda")
  expect_equal(runnerEnv$taskRiskWindow("taskA"), 30L)
})
