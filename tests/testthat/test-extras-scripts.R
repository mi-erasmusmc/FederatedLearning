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

loadFetchEnv <- function() {
  fetchEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("fetchTaskData.R"), fetchEnv)
  fetchEnv
}

test_that("split fetch templates encode generic external task structure", {
  skip_if_not_installed("yaml")
  fetchEnv <- loadFetchEnv()

  config <- fetchEnv$readFetchConfig(
    studyPath = extrasPath("fetch_study_template.yml"),
    dataSourcesPath = extrasPath("fetch_data_sources_template.yml"),
    executionPath = extrasPath("fetch_execution_template.yml")
  )
  execution <- fetchEnv$normalizeExecutionSettings(config$execution)
  rows <- fetchEnv$expandFetchRows(config$study, config$dataSources)

  expect_equal(length(rows), 4L)
  expect_equal(sort(unique(vapply(rows, `[[`, character(1), "clientId"))), c("databaseA", "databaseB"))
  expect_true(all(vapply(rows, `[[`, logical(1), "removeSubjectsWithPriorOutcome")))
  expect_true(all(vapply(rows, `[[`, integer(1), "priorOutcomeLookback") == 99999L))
  expect_equal(execution$outputRoot, "data")
  expect_true(execution$generateCohorts)

  taskRows <- rows[!duplicated(vapply(rows, `[[`, character(1), "task"))]
  expect_equal(vapply(taskRows, `[[`, character(1), "task"), c("taskA", "taskB"))
  expect_equal(vapply(taskRows, `[[`, integer(1), "riskWindowEnd"), c(365L, 30L))
  expect_equal(vapply(taskRows, `[[`, integer(1), "targetId"), c(100001L, 100002L))
  expect_equal(vapply(taskRows, `[[`, integer(1), "outcomeId"), c(200001L, 200002L))
  expect_equal(fetchEnv$cohortIdsForRow(rows[[1]]), c(100001L, 200001L, 300001L, 300002L))
})

test_that("fetch task helpers expand cohort ranges and keep one row per cohort", {
  fetchEnv <- loadFetchEnv()

  expect_equal(fetchEnv$csvValues("100:102,200"), c("100", "101", "102", "200"))

  row <- list(targetId = 300L, outcomeId = 200L, covariateCohortIds = "100:102")
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

test_that("connection profiles support env vars and connection string templates", {
  fetchEnv <- loadFetchEnv()

  oldUser <- Sys.getenv("FL_TEST_USER", unset = NA)
  oldPassword <- Sys.getenv("FL_TEST_PASSWORD", unset = NA)
  oldDriver <- Sys.getenv("FL_TEST_DRIVER", unset = NA)
  on.exit({
    if (is.na(oldUser)) Sys.unsetenv("FL_TEST_USER") else Sys.setenv(FL_TEST_USER = oldUser)
    if (is.na(oldPassword)) Sys.unsetenv("FL_TEST_PASSWORD") else Sys.setenv(FL_TEST_PASSWORD = oldPassword)
    if (is.na(oldDriver)) Sys.unsetenv("FL_TEST_DRIVER") else Sys.setenv(FL_TEST_DRIVER = oldDriver)
  })
  Sys.setenv(
    FL_TEST_USER = "user_a",
    FL_TEST_PASSWORD = "secret",
    FL_TEST_DRIVER = tempdir()
  )

  details <- fetchEnv$makeConnectionDetails(
    dataSource = list(
      connectionProfile = "custom",
      database = "db_a",
      databaseHost = "host_a"
    ),
    connectionProfiles = list(
      custom = list(
        dbms = "spark",
        userEnv = "FL_TEST_USER",
        passwordEnv = "FL_TEST_PASSWORD",
        pathToDriverEnv = "FL_TEST_DRIVER",
        connectionStringTemplate = "jdbc:spark://{databaseHost}/{database};ssl=1"
      )
    )
  )

  expect_equal(details$dbms, "spark")
  expect_equal(details$user(), "user_a")
  expect_equal(details$password(), "secret")
  expect_equal(details$pathToDriver, tempdir())
  expect_equal(details$connectionString(), "jdbc:spark://host_a/db_a;ssl=1")
})

test_that("split fetch config fails clearly for invalid references", {
  fetchEnv <- loadFetchEnv()

  study <- list(
    tasks = list(taskA = list(targetAtlasId = 1L, outcomeAtlasId = 2L, covariateProfile = "missing")),
    covariateProfiles = list(ageSex = list(demographicsAge = TRUE))
  )
  dataSources <- list(
    dataSources = list(siteA = list(
      cdmDatabaseSchema = "cdm",
      cohortDatabaseSchema = "scratch",
      cohortTable = "cohort"
    ))
  )
  expect_error(fetchEnv$expandFetchRows(study, dataSources), "unknown covariateProfile")

  dataSources$dataSources$siteA$connectionProfile <- "missing"
  expect_error(
    fetchEnv$makeConnectionDetails(dataSources$dataSources$siteA, connectionProfiles = list()),
    "Unknown connectionProfile"
  )

  dataSources$dataSources$siteA$connectionProfile <- NULL
  dataSources$dataSources$siteA$cdmDatabaseSchema <- NULL
  expect_error(fetchEnv$expandFetchRows(study = list(
    tasks = list(taskA = list(targetAtlasId = 1L, outcomeAtlasId = 2L, covariateProfile = "ageSex")),
    covariateProfiles = list(ageSex = list(demographicsAge = TRUE))
  ), dataSources = dataSources), "cdmDatabaseSchema")
})

test_that("execution settings support environment values and cli overrides", {
  fetchEnv <- loadFetchEnv()

  oldAtlas <- Sys.getenv("FL_TEST_ATLAS_URL", unset = NA)
  on.exit({
    if (is.na(oldAtlas)) Sys.unsetenv("FL_TEST_ATLAS_URL") else Sys.setenv(FL_TEST_ATLAS_URL = oldAtlas)
  })
  Sys.setenv(FL_TEST_ATLAS_URL = "https://atlas.example.org/WebAPI")

  execution <- fetchEnv$normalizeExecutionSettings(list(
    outputRoot = "data_a",
    atlasBaseUrlEnv = "FL_TEST_ATLAS_URL",
    generateCohorts = FALSE,
    overwrite = FALSE
  ))
  args <- fetchEnv$parseArgs(c(
    "--output-root=data_b",
    "--atlas-base-url=https://override.example.org/WebAPI",
    "--generate-cohorts=true",
    "--overwrite=true"
  ))
  if (!is.null(args[["output-root"]])) execution$outputRoot <- args[["output-root"]]
  if (!is.null(args[["atlas-base-url"]])) execution$atlasBaseUrl <- args[["atlas-base-url"]]
  if (!is.null(args[["generate-cohorts"]])) {
    execution$generateCohorts <- fetchEnv$logicalArg(args[["generate-cohorts"]], execution$generateCohorts)
  }
  if (!is.null(args[["overwrite"]])) {
    execution$overwrite <- fetchEnv$logicalArg(args[["overwrite"]], execution$overwrite)
  }

  expect_equal(execution$outputRoot, "data_b")
  expect_equal(execution$atlasBaseUrl, "https://override.example.org/WebAPI")
  expect_true(execution$generateCohorts)
  expect_true(execution$overwrite)
})

test_that("cohort preparation passes expected schemas and cohort ids to OHDSI helpers", {
  fetchEnv <- loadFetchEnv()

  generated <- list()
  fetchEnv$fetchCohortDefinitionSet <- function(cohortIds, row, atlasBaseUrl, jsonDirectory = NULL,
                                                cohortDatabaseSchema = row$cohortDatabaseSchema,
                                                cohortTable = row$cohortTable,
                                                generateStats = FALSE) {
    data.frame(
      cohortId = as.integer(cohortIds),
      cohortName = paste0("cohort_", cohortIds),
      sql = "select 1",
      json = "{}",
      stringsAsFactors = FALSE
    )
  }
  fetchEnv$generateCohortTable <- function(connectionDetails, row, cohortDefinitionSet,
                                           cohortDatabaseSchema, cohortTable,
                                           createTables = TRUE,
                                           incremental = TRUE,
                                           incrementalFolder = NULL) {
    generated[[length(generated) + 1L]] <<- list(
      cohortIds = cohortDefinitionSet$cohortId,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      createTables = createTables,
      incremental = incremental,
      incrementalFolder = incrementalFolder
    )
    invisible(NULL)
  }

  row <- list(
    task = "taskA",
    clientId = "siteA",
    targetId = 10L,
    outcomeId = 20L,
    covariateCohortIds = c(30L, 31L),
    cdmDatabaseSchema = "cdm",
    cohortDatabaseSchema = "scratch",
    cohortTable = "cohort",
    covariateCohortDatabaseSchema = "scratch_cov",
    covariateCohortTable = "covariate_cohort"
  )
  fetchEnv$prepareCohorts(
    row = row,
    connectionDetails = list(),
    execution = list(
      generateCohorts = TRUE,
      atlasBaseUrl = "https://atlas.example.org/WebAPI",
      jsonDirectory = tempdir(),
      generateStats = FALSE,
      incremental = TRUE,
      createCohortTables = TRUE,
      incrementalFolder = "incremental"
    )
  )

  expect_equal(length(generated), 2L)
  expect_equal(generated[[1]]$cohortIds, c(10L, 20L))
  expect_equal(generated[[1]]$cohortDatabaseSchema, "scratch")
  expect_equal(generated[[1]]$cohortTable, "cohort")
  expect_equal(generated[[2]]$cohortIds, c(30L, 31L))
  expect_equal(generated[[2]]$cohortDatabaseSchema, "scratch_cov")
  expect_equal(generated[[2]]$cohortTable, "covariate_cohort")
})

test_that("fetch rows preserve PLP database and population settings", {
  fetchEnv <- loadFetchEnv()

  study <- list(
    tasks = list(taskA = list(
      targetAtlasId = 10L,
      outcomeAtlasId = 20L,
      riskWindowStart = 1L,
      riskWindowEnd = 365L,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999L,
      requireTimeAtRisk = FALSE,
      minTimeAtRisk = 1L,
      covariateProfile = "ageSexPhenotypes"
    )),
    covariateProfiles = list(ageSexPhenotypes = list(
      demographicsAge = TRUE,
      demographicsGender = TRUE,
      cohortCovariates = list(analysisId = 49L, atlasIds = c(30L, 31L))
    ))
  )
  dataSources <- list(dataSources = list(siteA = list(
    cdmDatabaseSchema = "cdm",
    cdmDatabaseName = "CDM A",
    cohortDatabaseSchema = "scratch",
    cohortTable = "cohort",
    outcomeDatabaseSchema = "outcome_scratch",
    outcomeTable = "outcome_cohort",
    covariateCohortDatabaseSchema = "cov_scratch",
    covariateCohortTable = "covariate_cohort"
  )))

  rows <- fetchEnv$expandFetchRows(study, dataSources)
  expect_equal(length(rows), 1L)
  row <- rows[[1]]
  expect_equal(row$cdmDatabaseSchema, "cdm")
  expect_equal(row$cdmDatabaseName, "CDM A")
  expect_equal(row$cohortDatabaseSchema, "scratch")
  expect_equal(row$outcomeDatabaseSchema, "outcome_scratch")
  expect_equal(row$riskWindowEnd, 365L)
  expect_true(row$removeSubjectsWithPriorOutcome)
  expect_equal(row$covariateCohortIds, c(30L, 31L))
  expect_equal(row$covariateAnalysisId, 49L)
  expect_equal(row$covariateCohortDatabaseSchema, "cov_scratch")
})

test_that("fetchOne passes expected database and population settings to PLP", {
  fetchEnv <- loadFetchEnv()

  captured <- new.env(parent = emptyenv())
  captured$databaseDetails <- NULL
  captured$populationSettings <- NULL
  captured$savePath <- NULL

  fetchEnv$makeConnectionDetails <- function(dataSource, connectionProfiles = list()) {
    list(connection = "details")
  }
  fetchEnv$prepareCohorts <- function(row, connectionDetails, execution) {
    invisible(NULL)
  }
  fetchEnv$getCovariateSettings <- function(row) {
    list(covariates = "settings")
  }

  testthat::local_mocked_bindings(
    createDatabaseDetails = function(...) {
      captured$databaseDetails <- list(...)
      captured$databaseDetails
    },
    getPlpData = function(databaseDetails, covariateSettings, restrictPlpDataSettings = NULL) {
      list(
        databaseDetails = databaseDetails,
        covariateSettings = covariateSettings,
        restrictPlpDataSettings = restrictPlpDataSettings
      )
    },
    createRestrictPlpDataSettings = function(...) list(restrict = list(...)),
    createStudyPopulationSettings = function(...) {
      captured$populationSettings <- list(...)
      captured$populationSettings
    },
    createStudyPopulation = function(populationSettings, plpData) {
      data.frame(rowId = 1L, outcomeCount = 0L)
    },
    savePlpData = function(plpData, file) {
      captured$savePath <- file
      invisible(NULL)
    },
    .package = "PatientLevelPrediction"
  )

  outputRoot <- tempfile("fl-fetch-output")
  row <- list(
    task = "taskA",
    clientId = "siteA",
    targetId = 10L,
    outcomeId = 20L,
    cdmDatabaseSchema = "cdm",
    cdmDatabaseName = "CDM A",
    cohortDatabaseSchema = "scratch",
    cohortTable = "cohort",
    outcomeDatabaseSchema = "outcome_scratch",
    outcomeTable = "outcome_cohort",
    riskWindowStart = 1L,
    riskWindowEnd = 365L,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999L,
    requireTimeAtRisk = FALSE,
    minTimeAtRisk = 1L
  )

  out <- fetchEnv$fetchOne(
    row = row,
    dataSources = list(dataSources = list(siteA = list())),
    execution = list(outputRoot = outputRoot, overwrite = TRUE)
  )

  expect_equal(out, file.path(outputRoot, "taskA", "siteA"))
  expect_equal(captured$databaseDetails$cdmDatabaseSchema, "cdm")
  expect_equal(captured$databaseDetails$cdmDatabaseName, "CDM A")
  expect_equal(captured$databaseDetails$cohortDatabaseSchema, "scratch")
  expect_equal(captured$databaseDetails$cohortTable, "cohort")
  expect_equal(captured$databaseDetails$outcomeDatabaseSchema, "outcome_scratch")
  expect_equal(captured$databaseDetails$outcomeTable, "outcome_cohort")
  expect_equal(captured$databaseDetails$targetId, 10L)
  expect_equal(captured$databaseDetails$outcomeIds, 20L)
  expect_equal(captured$populationSettings$riskWindowStart, 1L)
  expect_equal(captured$populationSettings$riskWindowEnd, 365L)
  expect_true(captured$populationSettings$removeSubjectsWithPriorOutcome)
  expect_equal(captured$populationSettings$priorOutcomeLookback, 99999L)
  expect_equal(captured$savePath, file.path(outputRoot, "taskA", "siteA"))
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
