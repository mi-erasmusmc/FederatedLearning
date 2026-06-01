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
  expect_equal(execution$tempEmulationSchema, "scratch")

  taskRows <- rows[!duplicated(vapply(rows, `[[`, character(1), "task"))]
  expect_equal(vapply(taskRows, `[[`, character(1), "task"), c("taskA", "taskB"))
  expect_equal(vapply(taskRows, `[[`, integer(1), "riskWindowEnd"), c(365L, 30L))
  expect_equal(vapply(taskRows, `[[`, integer(1), "targetId"), c(100001L, 100002L))
  expect_equal(vapply(taskRows, `[[`, integer(1), "outcomeId"), c(200001L, 200002L))
  expect_equal(fetchEnv$cohortIdsForRow(rows[[1]]), c(1152:1215, 100001L, 200001L))
})

test_that("fetch task helpers expand cohort ranges and keep one row per cohort", {
  fetchEnv <- loadFetchEnv()

  expect_equal(fetchEnv$csvValues("100:102,200"), c("100", "101", "102", "200"))
  expect_equal(fetchEnv$defaultPhenotypeLibraryIds(), 1152:1215)
  json <- fetchEnv$cohortJson(list(expression = list(ConceptSets = list())))
  expect_type(json, "character")
  expect_false(inherits(json, "json"))

  json <- fetchEnv$cohortJson(list(expression = list(
    PrimaryCriteria = list(ObservationWindow = c(PriorDays = 0, PostDays = 0))
  )))
  expect_match(json, '"ObservationWindow"\\s*:\\s*\\{')
  expect_match(json, '"PriorDays"\\s*:\\s*0')
  expect_match(json, '"PostDays"\\s*:\\s*0')
  expect_false(grepl('"ObservationWindow"\\s*:\\s*\\[', json))
  expect_equal(fetchEnv$qualifiedTableName("scratch.username", "cohort"), "scratch.username.cohort")
  expect_equal(fetchEnv$qualifiedTableName("scratch.username", "other.cohort"), "other.cohort")

  profiles <- list(
    a = list(dbms = "postgresql"),
    b = list(dbms = "spark")
  )
  expect_identical(fetchEnv$`%||%`(profiles, list()), profiles)
  expect_identical(fetchEnv$`%||%`(NULL, list(default = TRUE)), list(default = TRUE))
  expect_identical(fetchEnv$`%||%`(NA_character_, "fallback"), "fallback")

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

test_that("Circe cohort SQL qualifies target cohort table", {
  skip_if_not_installed("CirceR")
  skip_if_not_installed("CohortGenerator")
  fetchEnv <- loadFetchEnv()

  jsonPath <- system.file("testdata/id/cohorts/1.json", package = "CohortGenerator")
  skip_if(!nzchar(jsonPath), "CohortGenerator test cohort JSON is unavailable")

  sql <- fetchEnv$buildSqlFromJson(
    json = paste(readLines(jsonPath, warn = FALSE), collapse = "\n"),
    cohortId = 1L,
    cdmDatabaseSchema = "cdm_schema",
    cohortDatabaseSchema = "scratch.username",
    cohortTable = "federated_learning",
    generateStats = FALSE
  )

  expect_match(sql, "DELETE FROM scratch\\.username\\.federated_learning")
  expect_match(sql, "INSERT INTO scratch\\.username\\.federated_learning")
  expect_false(grepl("DELETE FROM federated_learning", sql, fixed = TRUE))
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
    FL_TEST_DRIVER = tempdir(),
    FL_TEST_SERVER = "server.example.org",
    FL_TEST_HTTP_PATH = "/sql/1.0/warehouses/abc"
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
        serverEnv = "FL_TEST_SERVER",
        httpPathEnv = "FL_TEST_HTTP_PATH",
        connectionStringTemplate = "jdbc:databricks://{server};httpPath={httpPath};database={database};ssl=1"
      )
    )
  )

  expect_equal(details$dbms, "spark")
  expect_equal(details$user(), "user_a")
  expect_equal(details$password(), "secret")
  expect_equal(details$pathToDriver, tempdir())
  expect_equal(details$connectionString(), "jdbc:databricks://server.example.org;httpPath=/sql/1.0/warehouses/abc;database=db_a;ssl=1")
})

test_that("connection fields support compact keyring resolvers", {
  skip_if_not_installed("keyring")
  fetchEnv <- loadFetchEnv()

  calls <- list()
  testthat::local_mocked_bindings(
    key_get = function(service, username) {
      calls[[length(calls) + 1L]] <<- list(service = service, username = username)
      paste(service, username, sep = "::")
    },
    .package = "keyring"
  )

  expect_equal(fetchEnv$resolveConfigScalar("literal"), "literal")
  expect_equal(fetchEnv$resolveConfigScalar("keyring:database/server"), "database::server")
  expect_equal(calls[[1]]$service, "database")
  expect_equal(calls[[1]]$username, "server")
  expect_error(
    fetchEnv$resolveConfigScalar("keyring:database"),
    "Invalid keyring resolver"
  )
})

test_that("keyring resolvers can be used inside connection profiles", {
  skip_if_not_installed("keyring")
  fetchEnv <- loadFetchEnv()

  testthat::local_mocked_bindings(
    key_get = function(service, username) {
      values <- list(
        "database::server" = "server_a",
        "database::password" = "secret_a"
      )
      values[[paste(service, username, sep = "::")]]
    },
    .package = "keyring"
  )
  testthat::local_mocked_bindings(
    createConnectionDetails = function(...) {
      args <- list(...)
      server <- args$server
      user <- args$user
      password <- args$password
      args$server <- function() server
      args$user <- function() user
      args$password <- function() password
      args
    },
    .package = "DatabaseConnector"
  )

  details <- fetchEnv$makeConnectionDetails(
    dataSource = list(connectionProfile = "custom"),
    connectionProfiles = list(custom = list(
      dbms = "postgresql",
      server = "keyring:database/server",
      user = "token",
      password = "keyring:database/password"
    ))
  )

  expect_equal(details$dbms, "postgresql")
  expect_equal(details$server(), "server_a")
  expect_equal(details$user(), "token")
  expect_equal(details$password(), "secret_a")
})

test_that("WebAPI auth supports db credentials resolved from keyring", {
  skip_if_not_installed("keyring")
  fetchEnv <- loadFetchEnv()

  captured <- NULL
  testthat::local_mocked_bindings(
    key_get = function(service, username) {
      values <- list(
        "webapi::username" = "atlas_user",
        "webapi::password" = "atlas_secret"
      )
      values[[paste(service, username, sep = "::")]]
    },
    .package = "keyring"
  )
  testthat::local_mocked_bindings(
    authorizeWebApi = function(baseUrl, authMethod, webApiUsername = NULL, webApiPassword = NULL) {
      captured <<- list(
        baseUrl = baseUrl,
        authMethod = authMethod,
        webApiUsername = webApiUsername,
        webApiPassword = webApiPassword
      )
      invisible(NULL)
    },
    .package = "ROhdsiWebApi"
  )

  execution <- fetchEnv$normalizeExecutionSettings(list(
    atlasBaseUrl = "https://atlas.example.org/WebAPI",
    webApiAuth = list(
      method = "db",
      username = "keyring:webapi/username",
      password = "keyring:webapi/password"
    )
  ))
  fetchEnv$authorizeWebApiIfNeeded(execution)

  expect_equal(captured$baseUrl, "https://atlas.example.org/WebAPI")
  expect_equal(captured$authMethod, "db")
  expect_equal(captured$webApiUsername, "atlas_user")
  expect_equal(captured$webApiPassword, "atlas_secret")
})

test_that("WebAPI auth supports bearer token headers", {
  fetchEnv <- loadFetchEnv()

  captured <- NULL
  testthat::local_mocked_bindings(
    setAuthHeader = function(baseUrl, authHeader) {
      captured <<- list(baseUrl = baseUrl, authHeader = authHeader)
      invisible(NULL)
    },
    .package = "ROhdsiWebApi"
  )

  execution <- fetchEnv$normalizeExecutionSettings(list(
    atlasBaseUrl = "https://atlas.example.org/WebAPI",
    webApiAuth = list(bearerToken = "abc123")
  ))
  fetchEnv$authorizeWebApiIfNeeded(execution)

  expect_equal(captured$baseUrl, "https://atlas.example.org/WebAPI")
  expect_equal(captured$authHeader, "Bearer abc123")
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
  fetched <- list()
  fetchEnv$fetchCohortDefinitionSet <- function(cohortIds, row, atlasBaseUrl, jsonDirectory = NULL,
                                                cohortDatabaseSchema = row$cohortDatabaseSchema,
                                                cohortTable = row$cohortTable,
                                                generateStats = FALSE,
                                                cohortRole = "cohort") {
    fetched[[length(fetched) + 1L]] <<- list(
      cohortIds = as.integer(cohortIds),
      cohortRole = cohortRole
    )
    data.frame(
      cohortId = as.integer(cohortIds),
      cohortName = paste0("cohort_", cohortIds),
      sql = "select 1",
      json = "{}",
      stringsAsFactors = FALSE
    )
  }
  fetchEnv$phenotypeLibraryDefinitionSet <- function(cohortIds) {
    data.frame(
      cohortId = as.integer(cohortIds),
      cohortName = paste0("phenotype_", cohortIds),
      sql = "select 1",
      json = "{}",
      stringsAsFactors = FALSE
    )
  }
  fetchEnv$generateCohortTable <- function(connectionDetails, row, cohortDefinitionSet,
                                           cohortDatabaseSchema, cohortTable,
                                           createTables = TRUE,
                                           incremental = TRUE,
                                           incrementalFolder = NULL,
                                           tempEmulationSchema = NULL) {
    generated[[length(generated) + 1L]] <<- list(
      cohortIds = cohortDefinitionSet$cohortId,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      createTables = createTables,
      incremental = incremental,
      incrementalFolder = incrementalFolder,
      tempEmulationSchema = tempEmulationSchema
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
    tempEmulationSchema = "scratch_temp",
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
      incrementalFolder = "incremental",
      tempEmulationSchema = "execution_temp"
    )
  )

  expect_equal(length(generated), 2L)
  expect_equal(length(fetched), 1L)
  expect_equal(fetched[[1]]$cohortIds, c(10L, 20L))
  expect_equal(fetched[[1]]$cohortRole, "target/outcome")
  expect_equal(generated[[1]]$cohortIds, c(10L, 20L))
  expect_equal(generated[[1]]$cohortDatabaseSchema, "scratch")
  expect_equal(generated[[1]]$cohortTable, "cohort")
  expect_equal(generated[[1]]$tempEmulationSchema, "scratch_temp")
  expect_equal(generated[[2]]$cohortIds, c(30L, 31L))
  expect_equal(generated[[2]]$cohortDatabaseSchema, "scratch_cov")
  expect_equal(generated[[2]]$cohortTable, "covariate_cohort")
  expect_equal(generated[[2]]$tempEmulationSchema, "scratch_temp")
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
      cohortCovariates = list(analysisId = 49L, phenotypeLibraryIds = c(30L, 31L))
    ))
  )
  dataSources <- list(dataSources = list(siteA = list(
    cdmDatabaseSchema = "cdm",
    cdmDatabaseName = "CDM A",
    cohortDatabaseSchema = "scratch",
    cohortTable = "cohort",
    tempEmulationSchema = "scratch_temp",
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
  expect_equal(row$tempEmulationSchema, "scratch_temp")
  expect_equal(row$outcomeDatabaseSchema, "outcome_scratch")
  expect_equal(row$riskWindowEnd, 365L)
  expect_true(row$removeSubjectsWithPriorOutcome)
  expect_equal(row$covariateCohortIds, c(30L, 31L))
  expect_equal(row$covariateAnalysisId, 49L)
  expect_equal(row$covariateCohortDatabaseSchema, "cov_scratch")
})

test_that("covariate profiles support OHDSI default covariates", {
  skip_if_not_installed("FeatureExtraction")
  fetchEnv <- loadFetchEnv()

  defaultSettings <- fetchEnv$getCovariateSettings(list(
    covariateProfileDef = list(defaultCovariates = TRUE)
  ))
  expect_s3_class(defaultSettings, "covariateSettings")
  expect_true(isTRUE(defaultSettings$DemographicsGender))
  expect_true(isTRUE(defaultSettings$ConditionGroupEraLongTerm))
  expect_equal(attr(defaultSettings, "fun"), "getDbDefaultCovariateData")

  combinedSettings <- fetchEnv$getCovariateSettings(list(
    covariateProfileDef = list(
      defaultCovariates = TRUE,
      demographicsAge = TRUE,
      demographicsGender = TRUE,
      cohortCovariates = list(analysisId = 49L)
    ),
    covariateCohortIds = c(30L, 31L),
    covariateAnalysisId = 49L,
    covariateCohortDatabaseSchema = "scratch",
    covariateCohortTable = "covariate_cohort",
    cohortDatabaseSchema = "scratch",
    cohortTable = "cohort"
  ))
  expect_type(combinedSettings, "list")
  expect_length(combinedSettings, 2L)
  expect_s3_class(combinedSettings[[1]], "covariateSettings")
  expect_s3_class(combinedSettings[[2]], "covariateSettings")
  expect_equal(attr(combinedSettings[[1]], "fun"), "getDbDefaultCovariateData")
  expect_equal(attr(combinedSettings[[2]], "fun"), "getDbCohortBasedCovariatesData")
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
    tempEmulationSchema = "scratch_temp",
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
  expect_equal(captured$databaseDetails$tempEmulationSchema, "scratch_temp")
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
    "--adapdiag-style=pda",
    "--lambda-search=optimize",
    "--lambda-search-tol=0.2",
    "--lambda-search-max-evals=12",
    "--diagnostic-controls-per-case=2",
    "--odal-init=ridgeFallback",
    "--lead-index=2,3",
    "--dualavg-convergence-objective=cyclopsGradient"
  ))

  expect_equal(runnerEnv$csvArg(args[["client-ids"]]), c("databaseA", "databaseB", "databaseC", "databaseD", "databaseE"))
  expect_equal(runnerEnv$foldArg(args[["folds"]], 5L), 2:4)
  expect_true(runnerEnv$logicalArg(args[["remove-prior-outcomes"]], FALSE))
  expect_equal(runnerEnv$intArg(args[["prior-outcome-lookback"]], 1L), 99999L)

  cfg <- runnerEnv$methodConfig("ADAPDiag", "ageSexPhenotypes", args)
  expect_equal(cfg$rounds, 3L)
  expect_equal(cfg$adapDiagStyle, "pda")
  expect_equal(cfg$lambdaSearch, "optimize")
  expect_equal(cfg$lambdaSearchTol, 0.2)
  expect_equal(cfg$lambdaSearchMaxEvals, 12L)
  expect_equal(cfg$diagnosticControlsPerCase, 2)
  expect_equal(runnerEnv$taskRiskWindow("dementia"), 5 * 365)
  expect_equal(runnerEnv$taskRiskWindow("dementiaPhenotypes"), 5 * 365)
  expect_equal(runnerEnv$taskRiskWindow("taskA"), 30L)

  dualAvgCfg <- runnerEnv$methodConfig("DualAvg", "ageSex", args)
  expect_equal(dualAvgCfg$convergenceObjective, "cyclopsGradient")
  expect_true(runnerEnv$shouldTuneDualAvg(args))
  expect_equal(runnerEnv$dualAvgStartingVariance(args), 0.01)

  odalCfg <- runnerEnv$methodConfig("ODAL", "ageSexPhenotypes", args)
  expect_equal(odalCfg$odalInit, "ridgeFallback")
  expect_equal(odalCfg$odalRidgeLambda, 1e-8)
  expect_equal(odalCfg$odalVariant, "second")
  expect_equal(odalCfg$leadIndex, 2L)
  expect_equal(runnerEnv$methodConfig("ODAL1", "ageSexPhenotypes", args)$odalVariant, "first")
  expect_equal(length(runnerEnv$methodConfigGrid("ODAL", "ageSex", args)), 2L)

  gridArgs <- runnerEnv$parseArgs(c(
    "--eta-client=0.5,1",
    "--eta-server=0.25,1",
    "--k=2,4"
  ))
  grid <- runnerEnv$methodConfigGrid("DualAvg", "ageSex", gridArgs)
  expect_equal(length(grid), 8L)
  expect_equal(
    sort(unique(vapply(grid, `[[`, numeric(1), "etaClient"))),
    c(0.5, 1)
  )
  expect_equal(sort(unique(vapply(grid, `[[`, integer(1), "k"))), c(2L, 4L))
  expect_match(grid[[1]]$configLabel, "etaClient=")
  expect_true(all(c(
    "PooledLasso",
    "BiggestSiteLasso",
    "LocalAvgLasso",
    "LocalEnsembleLasso",
    "LocalBestLasso",
    "LocalSiteLasso"
  ) %in% runnerEnv$baselineMethods))
  expect_equal(
    runnerEnv$localModelWeights(c(10, 30), runnerEnv$parseArgs(character())),
    c(0.25, 0.75)
  )
  expect_equal(
    runnerEnv$localModelWeights(
      c(10, 30),
      runnerEnv$parseArgs(c("--local-ensemble-weighting=equalClient"))
    ),
    c(0.5, 0.5)
  )

  adapGridArgs <- runnerEnv$parseArgs(c(
    "--methods=ADAP",
    "--max-outer=50,100",
    "--max-inner=25,50",
    "--lambda-search=grid,optimize"
  ))
  adapGrid <- runnerEnv$methodConfigGrid("ADAP", "ageSex", adapGridArgs)
  expect_equal(length(adapGrid), 8L)
  expect_equal(sort(unique(vapply(adapGrid, `[[`, integer(1), "maxOuter"))), c(50L, 100L))
  expect_equal(sort(unique(vapply(adapGrid, `[[`, character(1), "lambdaSearch"))), c("grid", "optimize"))

  fixedGridArgs <- runnerEnv$parseArgs(c(
    "--dualavg-lambda=1e-05",
    "--eta-client=0.5,1"
  ))
  fixedGrid <- runnerEnv$methodConfigGrid("DualAvg", "ageSex", fixedGridArgs)
  runnerEnv$tuneDualAvgForFold <- function(method, clTrain, config, trainPopSizes, args, verbose) {
    config
  }
  runnerEnv$scoreFederatedConfigInnerCv <- function(method, clTrain, config, trainIds, verbose) {
    config$innerCvScore <- if (identical(config$etaClient, 1)) 0.8 else 0.7
    config
  }
  selectedFixed <- runnerEnv$selectMethodConfigForFold(
    method = "DualAvg",
    clTrain = list(1, 2, 3),
    configs = fixedGrid,
    trainPopSizes = c(10, 10, 10),
    args = fixedGridArgs,
    verbose = FALSE
  )
  expect_equal(selectedFixed$etaClient, 1)
  expect_equal(
    runnerEnv$communicationMessages(
      fit = list(roundsCompleted = 17L),
      config = list(rounds = 100L, trainClientPaths = paste0("site", 1:4))
    ),
    68L
  )
  expect_equal(
    runnerEnv$communicationMessages(
      fit = list(),
      config = list(rounds = 100L, trainClientPaths = paste0("site", 1:4))
    ),
    400L
  )

  defaultArgs <- runnerEnv$parseArgs(character())
  expect_equal(runnerEnv$methodConfig("ADAP_PDA", "ageSex", defaultArgs)$lambdaSearch, "optimize")
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$lambdaSearch, "optimize")
  expect_equal(runnerEnv$methodConfig("ADAP1", "ageSex", defaultArgs)$lambdaSearch, "optimize")
  expect_equal(runnerEnv$methodConfig("ADAPDiag", "ageSex", defaultArgs)$lambdaSearch, "optimize")
  expect_equal(runnerEnv$methodConfig("MaxConv-ADAP", "ageSex", defaultArgs)$lambdaSearch, "optimize")
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$lambdaSelectionMetric, "deviance")
  expect_equal(runnerEnv$methodConfig("ADAP1", "ageSex", defaultArgs)$lambdaSelectionMetric, "deviance")
  expect_equal(runnerEnv$methodConfig("ADAPDiag", "ageSex", defaultArgs)$lambdaSelectionMetric, "deviance")
  expect_equal(runnerEnv$methodConfig("ADAP_PDA", "ageSex", defaultArgs)$lambdaSelectionMetric, "deviance")
  expect_equal(runnerEnv$methodConfig("ODAL", "ageSex", defaultArgs)$odalInit, "pda")
  expect_equal(runnerEnv$methodConfig("ODAL", "ageSex", defaultArgs)$odalCurvatureAction, "report")
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$lambdaCvMaxRows, Inf)
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$maxOuter, 500L)
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$adapFinalMaxOuter, 1000L)
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$lambdaCvGlobalAdjustment, "leaveValOut")
  expect_equal(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$diagnosticControlsPerCase, Inf)
  expect_true(runnerEnv$methodConfig("ADAP", "ageSex", defaultArgs)$pooledDiagnostics)

  gapRows <- data.frame(
    task = "taskA",
    fold = 1L,
    featureSet = "ageSex",
    method = c("PooledLasso", "ADAP1"),
    pooledMeanLogLoss = c(0.20, 0.25)
  )
  expect_equal(runnerEnv$addPooledObjectiveGap(gapRows)$pooledObjectiveGap, c(0, 0.05))

  fixedDualAvgArgs <- runnerEnv$parseArgs(c("--dualavg-lambda=1e-05"))
  expect_false(runnerEnv$shouldTuneDualAvg(fixedDualAvgArgs))

  forcedTuneArgs <- runnerEnv$parseArgs(c(
    "--dualavg-lambda=1e-05",
    "--dualavg-tune-lambda=true",
    "--dualavg-starting-variance=0.02"
  ))
  expect_true(runnerEnv$shouldTuneDualAvg(forcedTuneArgs))
  expect_equal(runnerEnv$dualAvgStartingVariance(forcedTuneArgs), 0.02)
})

test_that("comparison runner evaluates prediction ensembles", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  clientData <- list(
    xMatrix = cbind(1, c(-2, -1, 1, 2)),
    yLabels = c(0, 0, 1, 1)
  )
  localFits <- list(
    list(w = c(0, 1)),
    list(w = c(0.5, 0.5))
  )
  modelWeights <- c(0.25, 0.75)

  ev <- runnerEnv$evaluateLocalEnsemble(
    clientData = clientData,
    localFits = localFits,
    modelWeights = modelWeights,
    clientId = "siteA",
    clientIndex = 1L
  )

  expectedPreds <- as.numeric(cbind(
    stats::plogis(clientData$xMatrix %*% localFits[[1]]$w),
    stats::plogis(clientData$xMatrix %*% localFits[[2]]$w)
  ) %*% modelWeights)
  expect_equal(ev$auc, runnerEnv$binaryAuc(clientData$yLabels, expectedPreds))
  expect_equal(ev$logLoss, FederatedLearning:::logLoss(clientData$yLabels, expectedPreds))
  expect_equal(ev$density, 0.875)
  expect_equal(ev$clientId, "siteA")
  expect_equal(ev$client, 1L)
})

test_that("comparison runner records local baseline fit failures", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  runnerEnv$fitBaselineWeights <- function(clientDataList, args, seed) {
    if (identical(seed, 2L)) {
      stop("local fit did not converge")
    }
    list(w = c(0, 1), selectedLambda = 0.01, elapsedSeconds = 0.2)
  }

  trainData <- list(
    list(xMatrix = cbind(1, c(0, 1)), yLabels = c(0, 1), n = 2L),
    list(xMatrix = cbind(1, c(0, 1)), yLabels = c(0, 1), n = 2L)
  )

  ok <- runnerEnv$fitLocalBaselineSafely(
    trainData = trainData,
    localIndex = 1L,
    trainClientId = "site1",
    args = list(),
    seed = 1L
  )
  failed <- runnerEnv$fitLocalBaselineSafely(
    trainData = trainData,
    localIndex = 2L,
    trainClientId = "site2",
    args = list(),
    seed = 2L
  )

  expect_true(ok$ok)
  expect_equal(ok$localIndex, 1L)
  expect_false(failed$ok)
  expect_equal(failed$trainClientId, "site2")
  expect_match(failed$error, "did not converge")
  expect_true(is.finite(failed$elapsedSeconds))
})

test_that("comparison runner preprocesses Cyclops baseline matrices", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  trainData <- list(
    list(
      xMatrix = Matrix::Matrix(cbind(1, c(0.2, 0.4, 0.6, 0.8), c(1, 0, 0, 0), 1), sparse = TRUE),
      yLabels = c(0, 1, 0, 1),
      n = 4L
    )
  )
  testData <- list(
    list(
      xMatrix = Matrix::Matrix(cbind(1, c(0.1, 0.8), c(1, 0), 1), sparse = TRUE),
      yLabels = c(0, 1),
      n = 2L
    )
  )

  out <- runnerEnv$preprocessBaselineData(
    trainData = trainData,
    testData = testData,
    config = list(intercept = TRUE),
    args = runnerEnv$parseArgs(character())
  )

  expect_equal(ncol(out$trainData[[1]]$xMatrix), 3L)
  expect_equal(as.numeric(out$trainData[[1]]$xMatrix[, 2]), c(0.2, 0.4, 0.6, 0.8))
  expect_equal(as.numeric(out$testData[[1]]$xMatrix[, 2]), c(0.1, 0.8))
  expect_equal(as.numeric(out$trainData[[1]]$xMatrix[, 3]), c(1, 0, 0, 0))
  expect_equal(out$preprocessor$removed, 1L)

  normalized <- runnerEnv$preprocessBaselineData(
    trainData = trainData,
    testData = testData,
    config = list(intercept = TRUE),
    args = runnerEnv$parseArgs("--baseline-preprocess-normalize=true")
  )

  expect_equal(as.numeric(normalized$trainData[[1]]$xMatrix[, 2]), c(0.25, 0.5, 0.75, 1))
  expect_equal(as.numeric(normalized$testData[[1]]$xMatrix[, 2]), c(0.125, 1))
})

test_that("comparison runner AUC handles large held-out sites", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  y <- c(rep(0L, 50000L), rep(1L, 50000L))
  preds <- seq_along(y)

  expect_equal(runnerEnv$binaryAuc(y, preds), 1)
})

test_that("comparison runner does not partial-match lambda-grid-len as fixed lambda", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  args <- runnerEnv$parseArgs(c(
    "--lambda-grid-len=100",
    "--methods=ADAP,ADAP1,ADAPDiag"
  ))

  expect_null(runnerEnv$argValue(args, "lambda"))
  expect_equal(runnerEnv$argValue(args, "lambda-grid-len"), "100")

  for (method in c("ADAP", "ADAP1", "ADAPDiag")) {
    cfg <- runnerEnv$methodConfig(method, "ageSex", args)
    expect_null(cfg[["lambda", exact = TRUE]])
    expect_equal(cfg$lambdaGridLen, 100L)
  }
})

test_that("comparison runner resumes successful combinations and reruns errors", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  rows <- data.frame(
    task = c("taskA", "taskA", "taskA"),
    fold = c(1L, 1L, 2L),
    featureSet = c("ageSex", "ageSex", "ageSex"),
    method = c("DualAvg", "ADAP", "DualAvg"),
    auc = c(0.7, NA, 0.6),
    error = c(NA_character_, "failed fit", ""),
    stringsAsFactors = FALSE
  )
  expect_true(runnerEnv$isCompletedCombination(rows, "taskA", 1L, "ageSex", "DualAvg"))
  expect_true(runnerEnv$isCompletedCombination(rows, "taskA", 1L, "ageSex", "ADAP"))
  expect_false(runnerEnv$isCompletedCombination(
    rows,
    "taskA",
    1L,
    "ageSex",
    "ADAP",
    rerunErrors = TRUE
  ))
  expect_true(runnerEnv$isCompletedCombination(
    rows,
    "taskA",
    1L,
    "ageSex",
    "ADAP",
    rerunErrors = FALSE
  ))
  expect_false(runnerEnv$isCompletedCombination(rows, "taskA", 1L, "ageSexPhenotypes", "DualAvg"))

  replacement <- data.frame(
    task = "taskA",
    fold = 1L,
    featureSet = "ageSex",
    method = "ADAP",
    auc = 0.65,
    error = NA_character_,
    stringsAsFactors = FALSE
  )
  combined <- runnerEnv$appendCombinationRows(rows, replacement, "taskA", 1L, "ageSex", "ADAP")

  expect_equal(nrow(combined), 3L)
  expect_true(runnerEnv$isCompletedCombination(combined, "taskA", 1L, "ageSex", "ADAP"))
  expect_equal(
    combined$auc[combined$task == "taskA" & combined$fold == 1L &
      combined$featureSet == "ageSex" & combined$method == "ADAP"],
    0.65
  )
  expect_false(any(combined$error == "failed fit", na.rm = TRUE))
})

test_that("comparison runner elapsed time covers outer method work", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  rows <- data.frame(
    method = c("DualAvg", "DualAvg"),
    elapsedSeconds = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )

  stamped <- runnerEnv$stampMethodElapsed(rows, Sys.time() - 10)

  expect_true(all(stamped$elapsedSeconds >= 9))
  expect_equal(stamped$fitElapsedSeconds, c(0.5, 0.5))
})

test_that("comparison runner reads existing result and diagnostic files", {
  runnerEnv <- new.env(parent = globalenv())
  sys.source(extrasPath("runComparisonMatrix.R"), runnerEnv)

  tmp <- tempfile("comparison-resume-")
  dir.create(tmp)
  resultFile <- file.path(tmp, "comparison_results.csv")
  diagnosticFile <- file.path(tmp, "diagnostics.csv")
  rows <- data.frame(
    task = "taskA",
    fold = 1L,
    featureSet = "ageSex",
    method = "DualAvg",
    error = NA_character_,
    stringsAsFactors = FALSE
  )
  diagnostics <- data.frame(
    task = "taskA",
    fold = 1L,
    featureSet = "ageSex",
    n = 10L,
    stringsAsFactors = FALSE
  )
  utils::write.csv(rows, resultFile, row.names = FALSE)
  utils::write.csv(diagnostics, diagnosticFile, row.names = FALSE)

  expect_null(runnerEnv$readCsvIfExists(file.path(tmp, "missing.csv")))
  loadedRows <- runnerEnv$readCsvIfExists(resultFile)
  loadedDiagnostics <- runnerEnv$readCsvIfExists(diagnosticFile)

  expect_true(runnerEnv$isCompletedCombination(loadedRows, "taskA", 1L, "ageSex", "DualAvg"))
  expect_true(runnerEnv$isCompletedDiagnostic(loadedDiagnostics, "taskA", 1L, "ageSex"))
  expect_false(runnerEnv$isCompletedDiagnostic(loadedDiagnostics, "taskA", 2L, "ageSex"))
})
