# Fetch PLP task/client data into the directory layout expected by runComparisonMatrix.R.
#
# Manifest columns:
# task,clientId,dbms,server,user,password,passwordEnvVar,port,
# cdmDatabaseSchema,cdmDatabaseName,cohortDatabaseSchema,cohortTable,
# outcomeDatabaseSchema,outcomeTable,targetId,outcomeId,riskWindowStart,riskWindowEnd,
# removeSubjectsWithPriorOutcome,priorOutcomeLookback,requireTimeAtRisk,minTimeAtRisk,covariatePreset,
# covariateCohortDatabaseSchema,covariateCohortTable,covariateCohortIds,covariateAnalysisId
#
# Example:
# Rscript extras/fetchTaskData.R \
#   --manifest=extras/task_manifest.csv \
#   --output-root=data \
#   --atlas-base-url="$ATLAS_BASE_URL" \
#   --generate-cohorts=true \
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
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  tolower(x) %in% c("true", "t", "1", "yes", "y")
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

readManifest <- function(path) {
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  required <- c(
    "task", "clientId", "dbms", "server", "cdmDatabaseSchema",
    "cohortDatabaseSchema", "cohortTable", "outcomeDatabaseSchema",
    "outcomeTable", "targetId", "outcomeId"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing) > 0L) {
    stop("Manifest is missing required columns: ", paste(missing, collapse = ", "))
  }
  manifest
}

getPassword <- function(row) {
  envVar <- field(row, "passwordEnvVar")
  if (!is.null(envVar)) {
    value <- Sys.getenv(envVar)
    if (nzchar(value)) {
      return(value)
    }
  }
  field(row, "password")
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

getCohortCovariateSettings <- function(row) {
  ids <- as.integer(csvValues(field(row, "covariateCohortIds")))
  if (length(ids) == 0L || any(is.na(ids))) {
    stop("covariatePreset requires numeric covariateCohortIds")
  }
  cohortNames <- paste0("cohort_", ids)
  covariateCohorts <- data.frame(
    cohortId = ids,
    cohortName = cohortNames,
    stringsAsFactors = FALSE
  )
  FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = as.integer(field(row, "covariateAnalysisId") %||% 49L),
    covariateCohortDatabaseSchema = field(row, "covariateCohortDatabaseSchema") %||%
      row$cohortDatabaseSchema,
    covariateCohortTable = field(row, "covariateCohortTable") %||% row$cohortTable,
    covariateCohorts = covariateCohorts
  )
}

cohortIdsForRow <- function(row) {
  ids <- c(as.integer(row$targetId), as.integer(row$outcomeId))
  ids <- c(ids, as.integer(csvValues(field(row, "covariateCohortIds"))))
  sort(unique(ids[is.finite(ids)]))
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
    stop("atlas-base-url is required when generate-cohorts=true")
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

prepareCohorts <- function(row, connectionDetails, args) {
  if (!logicalArg(args[["generate-cohorts"]], FALSE)) {
    return(invisible(NULL))
  }
  atlasBaseUrl <- args[["atlas-base-url"]] %||% Sys.getenv("ATLAS_BASE_URL")
  jsonRoot <- args[["json-directory"]] %||% file.path("extras", "atlas_json")
  generateStats <- logicalArg(args[["generate-stats"]], FALSE)
  incremental <- logicalArg(args[["incremental"]], TRUE)
  createTables <- logicalArg(args[["create-cohort-tables"]], TRUE)
  incrementalFolder <- field(row, "incrementalFolder") %||%
    file.path(tempdir(), "FederatedLearningCohortGenerator")
  taskIds <- unique(as.integer(c(row$targetId, row$outcomeId)))

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

  covariateIds <- as.integer(csvValues(field(row, "covariateCohortIds")))
  if (length(covariateIds) > 0L) {
    covariateSchema <- field(row, "covariateCohortDatabaseSchema") %||% row$cohortDatabaseSchema
    covariateTable <- field(row, "covariateCohortTable") %||% row$cohortTable
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
  preset <- field(row, "covariatePreset") %||% "demographics"
  if (identical(preset, "none")) {
    return(NULL)
  }
  if (identical(preset, "demographics")) {
    return(FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE
    ))
  }
  if (identical(preset, "conditions")) {
    return(FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE,
      useConditionOccurrenceLongTerm = TRUE
    ))
  }
  if (identical(preset, "demographicsAndConditions")) {
    return(FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE,
      useConditionOccurrenceLongTerm = TRUE
    ))
  }
  if (identical(preset, "cohorts") || identical(preset, "phenotypes")) {
    return(getCohortCovariateSettings(row))
  }
  if (identical(preset, "demographicsAndCohorts") ||
      identical(preset, "demographicsAndPhenotypes")) {
    return(list(
      FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE,
        useDemographicsAge = TRUE
      ),
      getCohortCovariateSettings(row)
    ))
  }
  stop("Unsupported covariatePreset: ", preset)
}

makeConnectionDetails <- function(row) {
  args <- list(
    dbms = row$dbms,
    server = row$server,
    user = field(row, "user"),
    password = getPassword(row),
    port = suppressWarnings(as.integer(field(row, "port")))
  )
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(DatabaseConnector::createConnectionDetails, args)
}

fetchOne <- function(row, outputRoot, overwrite = FALSE, fetchArgs = list()) {
  outputDir <- file.path(outputRoot, row$task, as.character(row$clientId))
  if (dir.exists(outputDir) && !overwrite) {
    message("Skipping existing ", outputDir)
    return(outputDir)
  }
  dir.create(dirname(outputDir), recursive = TRUE, showWarnings = FALSE)

  connectionDetails <- makeConnectionDetails(row)
  prepareCohorts(row, connectionDetails, fetchArgs)
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = row$cdmDatabaseSchema,
    cdmDatabaseName = field(row, "cdmDatabaseName") %||% row$cdmDatabaseSchema,
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
    requireTimeAtRisk = logicalField(row, "requireTimeAtRisk", FALSE),
    minTimeAtRisk = integerField(row, "minTimeAtRisk", 1L),
    riskWindowStart = integerField(row, "riskWindowStart", 1L),
    riskWindowEnd = integerField(row, "riskWindowEnd", 30L),
    removeSubjectsWithPriorOutcome = logicalField(row, "removeSubjectsWithPriorOutcome", TRUE),
    priorOutcomeLookback = integerField(row, "priorOutcomeLookback", 99999L)
  )
  plpData$population <- PatientLevelPrediction::createStudyPopulation(
    populationSettings = populationSettings,
    plpData = plpData
  )

  if (dir.exists(outputDir) && overwrite) {
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
  if (logicalArg(args[["generate-cohorts"]], FALSE)) {
    for (pkg in c("ROhdsiWebApi", "CirceR", "CohortGenerator", "jsonlite")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(pkg, " is required when generate-cohorts=true")
      }
    }
  }
  manifestPath <- args[["manifest"]] %||% "extras/task_manifest.csv"
  outputRoot <- args[["output-root"]] %||% "data"
  overwrite <- logicalArg(args[["overwrite"]], FALSE)
  manifest <- readManifest(manifestPath)
  outputs <- lapply(seq_len(nrow(manifest)), function(i) {
    fetchOne(
      manifest[i, , drop = FALSE],
      outputRoot = outputRoot,
      overwrite = overwrite,
      fetchArgs = args
    )
  })
  invisible(unlist(outputs))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) y else x
}

if (sys.nframe() == 0L) {
  runFetch(parseArgs())
}
