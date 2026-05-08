# Fetch PLP task/client data into the directory layout expected by runComparisonMatrix.R.
#
# This helper follows the OHDSI pattern of separating:
# - study settings: cohorts, risk windows, and covariate profiles;
# - execution settings: output folders, ATLAS URL, and generation switches;
# - data-source settings: local schemas and DatabaseConnector connection inputs.
#
# Example:
# Rscript extras/fetchTaskData.R \
#   --study=extras/fetch_study_template.yml \
#   --data-sources=extras/fetch_data_sources_template.yml \
#   --execution=extras/fetch_execution_template.yml \
#   --overwrite=false

parseArgs <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }
    kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    out[[kv[[1]]]] <- if (length(kv) > 1L) paste(kv[-1], collapse = "=") else "true"
  }
  out
}

logicalArg <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(as.character(x))) {
    return(default)
  }
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

firstNonEmpty <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0L && !is.na(val[[1]]) && nzchar(as.character(val[[1]]))) {
      return(as.character(val[[1]]))
    }
  }
  NULL
}

field <- function(row, name, default = NULL) {
  if (!name %in% names(row)) {
    return(default)
  }
  firstNonEmpty(row[[name]]) %||% default
}

logicalField <- function(row, name, default = FALSE) {
  value <- field(row, name)
  logicalArg(value, default = default)
}

integerField <- function(row, name, default) {
  value <- field(row, name)
  if (is.null(value)) {
    return(default)
  }
  as.integer(value)
}

asNamedList <- function(x, label) {
  if (is.null(x) || !is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(label, " must be a named YAML mapping")
  }
  x
}

readYamlConfig <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml is required to read split fetch configuration")
  }
  yaml::read_yaml(path)
}

readFetchConfig <- function(studyPath, dataSourcesPath, executionPath) {
  list(
    study = readYamlConfig(studyPath),
    dataSources = readYamlConfig(dataSourcesPath),
    execution = readYamlConfig(executionPath)
  )
}

envField <- function(x, name) {
  envName <- firstNonEmpty(x[[paste0(name, "Env")]])
  if (!is.null(envName)) {
    value <- Sys.getenv(envName)
    if (nzchar(value)) {
      return(resolveConfigScalar(value))
    }
  }
  resolveConfigScalar(firstNonEmpty(x[[name]]))
}

resolveConfigScalar <- function(value) {
  value <- firstNonEmpty(value)
  if (is.null(value)) {
    return(NULL)
  }
  if (!startsWith(value, "keyring:")) {
    return(value)
  }
  spec <- sub("^keyring:", "", value)
  parts <- strsplit(spec, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L || any(!nzchar(parts))) {
    stop("Invalid keyring resolver '", value, "'. Use keyring:<service>/<username>.")
  }
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("keyring is required to resolve '", value, "'")
  }
  keyring::key_get(service = parts[[1]], username = parts[[2]])
}

renderTemplate <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    value <- values[[nm]]
    if (is.null(value) || length(value) != 1L || is.na(value)) {
      next
    }
    out <- gsub(paste0("\\{", nm, "\\}"), as.character(value), out)
  }
  out
}

scalarOrNull <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  x[[1]]
}

mergeLists <- function(...) {
  parts <- list(...)
  out <- list()
  for (part in parts) {
    if (is.null(part)) {
      next
    }
    for (nm in names(part)) {
      out[[nm]] <- part[[nm]]
    }
  }
  out
}

makeConnectionDetails <- function(dataSource, connectionProfiles = list()) {
  profileName <- scalarOrNull(dataSource$connectionProfile)
  profile <- if (!is.null(profileName)) {
    connectionProfiles[[profileName]] %||%
      stop("Unknown connectionProfile '", profileName, "' for data source")
  } else {
    list()
  }
  connection <- mergeLists(profile, dataSource$connection)
  values <- mergeLists(dataSource, connection)
  connectionString <- envField(connection, "connectionString")
  template <- firstNonEmpty(connection$connectionStringTemplate)
  if (is.null(connectionString) && !is.null(template)) {
    connectionString <- renderTemplate(template, values)
  }
  args <- list(
    dbms = envField(connection, "dbms"),
    server = envField(connection, "server"),
    user = envField(connection, "user"),
    password = envField(connection, "password"),
    port = suppressWarnings(as.integer(envField(connection, "port"))),
    extraSettings = envField(connection, "extraSettings"),
    oracleDriver = envField(connection, "oracleDriver"),
    connectionString = connectionString,
    pathToDriver = envField(connection, "pathToDriver")
  )
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(DatabaseConnector::createConnectionDetails, args)
}

csvValues <- function(x) {
  x <- firstNonEmpty(x)
  if (is.null(x)) {
    return(character())
  }
  vals <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  unlist(lapply(vals, function(val) {
    if (grepl("^[0-9]+:[0-9]+$", val)) {
      rng <- as.integer(strsplit(val, ":", fixed = TRUE)[[1]])
      return(as.character(seq.int(rng[[1]], rng[[2]])))
    }
    val
  }), use.names = FALSE)
}

cohortVector <- function(x) {
  if (is.null(x)) {
    return(integer())
  }
  if (length(x) == 1L && is.character(x)) {
    return(as.integer(csvValues(x)))
  }
  as.integer(unlist(x, use.names = FALSE))
}

getCohortCovariateSettings <- function(row) {
  ids <- cohortVector(row$covariateCohortIds)
  if (length(ids) == 0L || any(is.na(ids))) {
    stop("Cohort covariate profiles require numeric covariateCohortIds")
  }
  cohortNames <- paste0("cohort_", ids)
  covariateCohorts <- data.frame(
    cohortId = ids,
    cohortName = cohortNames,
    stringsAsFactors = FALSE
  )
  FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = as.integer(row$covariateAnalysisId %||% 49L),
    covariateCohortDatabaseSchema = row$covariateCohortDatabaseSchema %||%
      row$cohortDatabaseSchema,
    covariateCohortTable = row$covariateCohortTable %||% row$cohortTable,
    covariateCohorts = covariateCohorts
  )
}

cohortIdsForRow <- function(row) {
  ids <- c(as.integer(row$targetId), as.integer(row$outcomeId))
  ids <- c(ids, cohortVector(row$covariateCohortIds))
  sort(unique(ids[is.finite(ids)]))
}

taskCohortIdsForRow <- function(row) {
  sort(unique(as.integer(c(row$targetId, row$outcomeId))))
}

cohortJson <- function(definition) {
  expr <- definition$expression %||% definition$json %||% definition
  jsonlite::toJSON(expr, auto_unbox = TRUE, null = "null", pretty = TRUE)
}

buildSqlFromJson <- function(json, cohortId, cdmDatabaseSchema, cohortDatabaseSchema,
                             cohortTable, generateStats = FALSE) {
  expression <- CirceR::cohortExpressionFromJson(json)
  options <- CirceR::createGenerateOptions(
    cohortIdFieldName = "cohort_definition_id",
    cohortId = as.integer(cohortId),
    cdmSchema = cdmDatabaseSchema,
    targetTable = cohortTable,
    resultSchema = cohortDatabaseSchema,
    vocabularySchema = cdmDatabaseSchema,
    generateStats = generateStats
  )
  CirceR::buildCohortQuery(expression, options)
}

fetchCohortDefinitionSet <- function(cohortIds, row, atlasBaseUrl, jsonDirectory = NULL,
                                     cohortDatabaseSchema = row$cohortDatabaseSchema,
                                     cohortTable = row$cohortTable,
                                     generateStats = FALSE) {
  if (is.null(atlasBaseUrl) || !nzchar(atlasBaseUrl)) {
    stop("atlasBaseUrl is required when generateCohorts=true")
  }
  cohortRows <- lapply(cohortIds, function(cohortId) {
    definition <- ROhdsiWebApi::getCohortDefinition(
      cohortId = as.integer(cohortId),
      baseUrl = atlasBaseUrl
    )
    json <- cohortJson(definition)
    if (!is.null(jsonDirectory)) {
      dir.create(jsonDirectory, recursive = TRUE, showWarnings = FALSE)
      writeLines(json, file.path(jsonDirectory, paste0("cohort_", cohortId, ".json")))
    }
    sql <- buildSqlFromJson(
      json = json,
      cohortId = cohortId,
      cdmDatabaseSchema = row$cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      generateStats = generateStats
    )
    data.frame(
      cohortId = as.integer(cohortId),
      cohortName = definition$name %||% paste0("cohort_", cohortId),
      sql = paste(sql, collapse = "\n"),
      json = paste(json, collapse = "\n"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, cohortRows)
}

generateCohortTable <- function(connectionDetails, row, cohortDefinitionSet,
                                cohortDatabaseSchema, cohortTable,
                                createTables = TRUE,
                                incremental = TRUE,
                                incrementalFolder = NULL) {
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  if (createTables) {
    CohortGenerator::createCohortTables(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = cohortTableNames,
      incremental = incremental
    )
  }
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = row$cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet,
    stopOnError = TRUE,
    incremental = incremental,
    incrementalFolder = incrementalFolder
  )
}

prepareCohorts <- function(row, connectionDetails, execution) {
  if (!isTRUE(execution$generateCohorts)) {
    return(invisible(NULL))
  }
  atlasBaseUrl <- execution$atlasBaseUrl
  jsonRoot <- execution$jsonDirectory %||% file.path("extras", "atlas_json")
  generateStats <- isTRUE(execution$generateStats)
  incremental <- isTRUE(execution$incremental)
  createTables <- isTRUE(execution$createCohortTables)
  incrementalFolder <- row$incrementalFolder %||%
    execution$incrementalFolder %||%
    file.path(tempdir(), "FederatedLearningCohortGenerator")
  taskIds <- taskCohortIdsForRow(row)

  message("Fetching ATLAS target/outcome definitions for ", row$task, "/", row$clientId)
  taskSet <- fetchCohortDefinitionSet(
    cohortIds = taskIds,
    row = row,
    atlasBaseUrl = atlasBaseUrl,
    jsonDirectory = file.path(jsonRoot, row$task),
    cohortDatabaseSchema = row$cohortDatabaseSchema,
    cohortTable = row$cohortTable,
    generateStats = generateStats
  )
  generateCohortTable(
    connectionDetails = connectionDetails,
    row = row,
    cohortDefinitionSet = taskSet,
    cohortDatabaseSchema = row$cohortDatabaseSchema,
    cohortTable = row$cohortTable,
    createTables = createTables,
    incremental = incremental,
    incrementalFolder = incrementalFolder
  )

  covariateIds <- cohortVector(row$covariateCohortIds)
  if (length(covariateIds) > 0L) {
    covariateSchema <- row$covariateCohortDatabaseSchema %||% row$cohortDatabaseSchema
    covariateTable <- row$covariateCohortTable %||% row$cohortTable
    message("Fetching ATLAS covariate cohort definitions for ", row$task, "/", row$clientId)
    covSet <- fetchCohortDefinitionSet(
      cohortIds = covariateIds,
      row = row,
      atlasBaseUrl = atlasBaseUrl,
      jsonDirectory = file.path(jsonRoot, row$task),
      cohortDatabaseSchema = covariateSchema,
      cohortTable = covariateTable,
      generateStats = generateStats
    )
    generateCohortTable(
      connectionDetails = connectionDetails,
      row = row,
      cohortDefinitionSet = covSet,
      cohortDatabaseSchema = covariateSchema,
      cohortTable = covariateTable,
      createTables = createTables && (!identical(covariateSchema, row$cohortDatabaseSchema) ||
        !identical(covariateTable, row$cohortTable)),
      incremental = incremental,
      incrementalFolder = incrementalFolder
    )
  }
  invisible(NULL)
}

getCovariateSettings <- function(row) {
  profile <- row$covariateProfileDef %||% list(demographicsAge = TRUE, demographicsGender = TRUE)
  baseSettings <- NULL
  if (isTRUE(profile$demographicsAge) || isTRUE(profile$demographicsGender) ||
      isTRUE(profile$conditionsLongTerm)) {
    baseSettings <- FeatureExtraction::createCovariateSettings(
      useDemographicsGender = isTRUE(profile$demographicsGender),
      useDemographicsAge = isTRUE(profile$demographicsAge),
      useConditionOccurrenceLongTerm = isTRUE(profile$conditionsLongTerm)
    )
  }
  hasCohorts <- length(cohortVector(row$covariateCohortIds)) > 0L
  if (!hasCohorts) {
    return(baseSettings)
  }
  cohortSettings <- getCohortCovariateSettings(row)
  if (is.null(baseSettings)) {
    cohortSettings
  } else {
    list(baseSettings, cohortSettings)
  }
}

normalizeExecutionSettings <- function(execution) {
  execution <- execution %||% list()
  atlasBaseUrl <- resolveConfigScalar(execution$atlasBaseUrl)
  atlasBaseUrlEnv <- firstNonEmpty(execution$atlasBaseUrlEnv)
  if (is.null(atlasBaseUrl) && !is.null(atlasBaseUrlEnv)) {
    atlasBaseUrl <- resolveConfigScalar(Sys.getenv(atlasBaseUrlEnv))
    if (!nzchar(atlasBaseUrl)) {
      atlasBaseUrl <- NULL
    }
  }
  list(
    outputRoot = firstNonEmpty(execution$outputRoot) %||% "data",
    atlasBaseUrl = atlasBaseUrl,
    generateCohorts = logicalArg(execution$generateCohorts, FALSE),
    createCohortTables = logicalArg(execution$createCohortTables, TRUE),
    generateStats = logicalArg(execution$generateStats, FALSE),
    incremental = logicalArg(execution$incremental, TRUE),
    jsonDirectory = firstNonEmpty(execution$jsonDirectory) %||% file.path("extras", "atlas_json"),
    incrementalFolder = firstNonEmpty(execution$incrementalFolder),
    overwrite = logicalArg(execution$overwrite, FALSE),
    webApiAuth = execution$webApiAuth
  )
}

authorizeWebApiIfNeeded <- function(execution) {
  auth <- execution$webApiAuth
  if (is.null(auth)) {
    return(invisible(FALSE))
  }
  if (is.null(execution$atlasBaseUrl) || !nzchar(execution$atlasBaseUrl)) {
    stop("atlasBaseUrl is required when webApiAuth is configured")
  }
  bearerToken <- resolveConfigScalar(auth$bearerToken)
  authHeader <- resolveConfigScalar(auth$authHeader)
  if (!is.null(bearerToken) || !is.null(authHeader)) {
    token <- authHeader %||% bearerToken
    if (!grepl("^Bearer\\s+", token)) {
      token <- paste("Bearer", token)
    }
    ROhdsiWebApi::setAuthHeader(
      baseUrl = execution$atlasBaseUrl,
      authHeader = token
    )
    return(invisible(TRUE))
  }
  method <- resolveConfigScalar(auth$method)
  if (is.null(method)) {
    stop("webApiAuth must define method, bearerToken, or authHeader")
  }
  ROhdsiWebApi::authorizeWebApi(
    baseUrl = execution$atlasBaseUrl,
    authMethod = method,
    webApiUsername = resolveConfigScalar(auth$username),
    webApiPassword = resolveConfigScalar(auth$password)
  )
  invisible(TRUE)
}

requireScalar <- function(x, name) {
  value <- scalarOrNull(x)
  if (is.null(value)) {
    stop("Missing required setting: ", name)
  }
  value
}

rowForTaskSource <- function(taskName, task, profile, sourceName, dataSource) {
  targetId <- task$targetId %||% task$targetAtlasId
  outcomeId <- task$outcomeId %||% task$outcomeAtlasId
  if (is.null(targetId) || is.null(outcomeId)) {
    stop("Task '", taskName, "' must define targetId/targetAtlasId and outcomeId/outcomeAtlasId")
  }
  cohortCovariates <- profile$cohortCovariates %||% list()
  list(
    task = taskName,
    clientId = sourceName,
    targetId = as.integer(targetId),
    outcomeId = as.integer(outcomeId),
    riskWindowStart = as.integer(task$riskWindowStart %||% 1L),
    riskWindowEnd = as.integer(task$riskWindowEnd %||% 30L),
    removeSubjectsWithPriorOutcome = task$removeSubjectsWithPriorOutcome %||% TRUE,
    priorOutcomeLookback = as.integer(task$priorOutcomeLookback %||% 99999L),
    requireTimeAtRisk = task$requireTimeAtRisk %||% FALSE,
    minTimeAtRisk = as.integer(task$minTimeAtRisk %||% 1L),
    covariateProfile = task$covariateProfile %||% "demographics",
    covariateProfileDef = profile,
    covariateCohortIds = cohortVector(cohortCovariates$atlasIds %||% cohortCovariates$cohortIds),
    covariateAnalysisId = as.integer(cohortCovariates$analysisId %||% 49L),
    covariateCohortDatabaseSchema = dataSource$covariateCohortDatabaseSchema %||%
      dataSource$cohortDatabaseSchema,
    covariateCohortTable = dataSource$covariateCohortTable %||% dataSource$cohortTable,
    cdmDatabaseSchema = requireScalar(dataSource$cdmDatabaseSchema, paste0(sourceName, ".cdmDatabaseSchema")),
    cdmDatabaseName = dataSource$cdmDatabaseName %||% sourceName,
    cohortDatabaseSchema = requireScalar(dataSource$cohortDatabaseSchema, paste0(sourceName, ".cohortDatabaseSchema")),
    cohortTable = requireScalar(dataSource$cohortTable, paste0(sourceName, ".cohortTable")),
    outcomeDatabaseSchema = dataSource$outcomeDatabaseSchema %||% dataSource$cohortDatabaseSchema,
    outcomeTable = dataSource$outcomeTable %||% dataSource$cohortTable,
    connectionProfile = dataSource$connectionProfile,
    connection = dataSource$connection,
    incrementalFolder = dataSource$incrementalFolder
  )
}

expandFetchRows <- function(study, dataSources) {
  tasks <- asNamedList(study$tasks, "study$tasks")
  profiles <- asNamedList(study$covariateProfiles, "study$covariateProfiles")
  sources <- asNamedList(dataSources$dataSources, "dataSources$dataSources")
  rows <- list()
  for (taskName in names(tasks)) {
    task <- tasks[[taskName]]
    profileName <- task$covariateProfile %||% "demographics"
    profile <- profiles[[profileName]] %||%
      stop("Task '", taskName, "' references unknown covariateProfile '", profileName, "'")
    for (sourceName in names(sources)) {
      rows[[length(rows) + 1L]] <- rowForTaskSource(
        taskName = taskName,
        task = task,
        profile = profile,
        sourceName = sourceName,
        dataSource = sources[[sourceName]]
      )
    }
  }
  rows
}

fetchOne <- function(row, dataSources, execution) {
  outputDir <- file.path(execution$outputRoot, row$task, as.character(row$clientId))
  if (dir.exists(outputDir) && !execution$overwrite) {
    message("Skipping existing ", outputDir)
    return(outputDir)
  }
  dir.create(dirname(outputDir), recursive = TRUE, showWarnings = FALSE)

  connectionProfiles <- dataSources$connectionProfiles %||% list()
  connectionDetails <- makeConnectionDetails(
    dataSource = mergeLists(
      dataSources$dataSources[[row$clientId]],
      list(connection = row$connection)
    ),
    connectionProfiles = connectionProfiles
  )
  prepareCohorts(row, connectionDetails, execution)
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = row$cdmDatabaseSchema,
    cdmDatabaseName = row$cdmDatabaseName %||% row$cdmDatabaseSchema,
    cohortDatabaseSchema = row$cohortDatabaseSchema,
    cohortTable = row$cohortTable,
    outcomeDatabaseSchema = row$outcomeDatabaseSchema,
    outcomeTable = row$outcomeTable,
    targetId = as.integer(row$targetId),
    outcomeIds = as.integer(row$outcomeId)
  )
  covariateSettings <- getCovariateSettings(row)
  restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()

  message(sprintf(
    "Fetching task=%s client=%s target=%s outcome=%s",
    row$task, row$clientId, row$targetId, row$outcomeId
  ))
  plpData <- PatientLevelPrediction::getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = covariateSettings,
    restrictPlpDataSettings = restrictPlpDataSettings
  )

  populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = logicalArg(row$requireTimeAtRisk, FALSE),
    minTimeAtRisk = as.integer(row$minTimeAtRisk %||% 1L),
    riskWindowStart = as.integer(row$riskWindowStart %||% 1L),
    riskWindowEnd = as.integer(row$riskWindowEnd %||% 30L),
    removeSubjectsWithPriorOutcome = logicalArg(row$removeSubjectsWithPriorOutcome, TRUE),
    priorOutcomeLookback = as.integer(row$priorOutcomeLookback %||% 99999L)
  )
  plpData$population <- PatientLevelPrediction::createStudyPopulation(
    populationSettings = populationSettings,
    plpData = plpData
  )

  if (dir.exists(outputDir) && execution$overwrite) {
    unlink(outputDir, recursive = TRUE)
  }
  PatientLevelPrediction::savePlpData(plpData, file = outputDir)
  outputDir
}

runFetch <- function(args) {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    stop("DatabaseConnector is required")
  }
  if (!requireNamespace("FeatureExtraction", quietly = TRUE)) {
    stop("FeatureExtraction is required")
  }
  if (!requireNamespace("PatientLevelPrediction", quietly = TRUE)) {
    stop("PatientLevelPrediction is required")
  }

  studyPath <- args[["study"]] %||% "extras/fetch_study.yml"
  dataSourcesPath <- args[["data-sources"]] %||% "extras/fetch_data_sources.yml"
  executionPath <- args[["execution"]] %||% "extras/fetch_execution.yml"
  config <- readFetchConfig(studyPath, dataSourcesPath, executionPath)
  execution <- normalizeExecutionSettings(config$execution)
  if (!is.null(args[["output-root"]])) {
    execution$outputRoot <- args[["output-root"]]
  }
  if (!is.null(args[["overwrite"]])) {
    execution$overwrite <- logicalArg(args[["overwrite"]], execution$overwrite)
  }
  if (!is.null(args[["atlas-base-url"]])) {
    execution$atlasBaseUrl <- args[["atlas-base-url"]]
  }
  if (!is.null(args[["generate-cohorts"]])) {
    execution$generateCohorts <- logicalArg(args[["generate-cohorts"]], execution$generateCohorts)
  }
  if (isTRUE(execution$generateCohorts)) {
    for (pkg in c("ROhdsiWebApi", "CirceR", "CohortGenerator", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(pkg, " is required when generateCohorts=true")
      }
    }
    authorizeWebApiIfNeeded(execution)
  }
  rows <- expandFetchRows(config$study, config$dataSources)
  outputs <- lapply(rows, fetchOne, dataSources = config$dataSources, execution = execution)
  invisible(unlist(outputs))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) y else x
}

if (sys.nframe() == 0L) {
  runFetch(parseArgs())
}
