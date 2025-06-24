# split one data object into many for testing
split <- function(plpData, 
                  populationSettings, 
                  N, 
                  underSample = FALSE,
                  seed = 42,
                  subjectSplit = FALSE) {
  population <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData,
    populationSettings = populationSettings
  )
  set.seed(seed)
  if (underSample) {
    message("Under-sampling")
    positives <- dplyr::filter(population, .data$outcomeCount > 0)
    negatives <- dplyr::filter(population, .data$outcomeCount == 0)
    negatives <- dplyr::sample_n(negatives, nrow(positives), replace = TRUE)
    population <- dplyr::bind_rows(positives, negatives)
  }
  if (!subjectSplit) {
    message("Splitting population into ", N, " groups (row-wise)")
    population <- dplyr::mutate(population, group = sample(1:N, nrow(population), replace = TRUE))
  } else {
    message("Splitting population into ", N, " groups (subject-wise)")
    subjectLevel <- population |> 
      dplyr::group_by(.data$subjectId) |>
      dplyr::summarise(event = any(.data$outcomeCount > 0), .groups = "drop")
    pos <- subjectLevel |>
      dplyr::filter(.data$event) |>
      dplyr::slice_sample(prop = 1)
    neg <- subjectLevel |>
      dplyr::filter(!.data$event) |>
      dplyr::slice_sample(prop = 1) 
    pos$group <- rep(seq_len(N), length.out = nrow(pos))
    neg$group <- rep(seq_len(N), length.out = nrow(neg))

    subjectGroups <- dplyr::bind_rows(pos, neg) |>
      dplyr::select("subjectId", "group")
    population <- population |>
      dplyr::left_join(subjectGroups, by = "subjectId")
  }
  # split the plpData into N groups
  for (i in 1:N) {
    # get the population for group i
    message("Getting population for group ", i)
    populationClient <- dplyr::filter(population, .data$group == i)
    # get the plpData for group i
    plpDataClient <- plpData

    plpDataClient$population <- populationClient
    message("Getting outcomes for group ", i)
    plpDataClient$outcomes <- dplyr::filter(plpData$outcomes, 
                                            .data$rowId %in% populationClient$rowId)
    message("Getting cohorts for group ", i)
    plpDataClient$cohorts <- dplyr::filter(plpData$cohorts, 
                                           .data$rowId %in% populationClient$rowId)

    message("Getting covariates for group ", i)
    covariateDataClient <- Andromeda::copyAndromeda(plpData$covariateData)
    covariateDataClient$covariates <- dplyr::filter(
      covariateDataClient$covariates,
      .data$rowId %in% populationClient$rowId
    )
    message("Getting covariateRef for group ", i)
    covariateDataClient$covariateRef <- plpData$covariateData$covariateRef |>
      dplyr::filter(.data$covariateId %in% !!covariateDataClient$covariates |>
        dplyr::pull(.data$covariateId) |>
        unique())
    plpDataClient$covariateData <- covariateDataClient
    message("Saving plpData for group ", i)
    PatientLevelPrediction::savePlpData(plpDataClient,
      file = file.path("./data/readmissionPhenotypes/", paste0("client", i))
    )
  }
}


connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "duckdb",
  server = "~/database/duckyS.duckdb"
)

getCovariateSettings <- function(cohorts = FALSE,
                                 connectionDetails = NULL,
                                 cohortTable = NULL) {
  if (cohorts) {
    phenoTypeDefs <- PhenotypeLibrary::getPlCohortDefinitionSet(1152:1215)
    cohortTableNames <-
      CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

    # CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
    #                                     cohortDatabaseSchema = "cohorts",
    #                                     cohortTableNames = cohortTableNames)
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      cohortDatabaseSchema = "cohorts",
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = phenoTypeDefs
    )
    covariateCohorts <- cohortsGenerated |>
      dplyr::select(c("cohortId", "cohortName"))
    covariateSettings <- list(
      FeatureExtraction::createCovariateSettings(
        useDemographicsAge = TRUE,
        useDemographicsGender = TRUE
      ),
      FeatureExtraction::createCohortBasedCovariateSettings(
        analysisId = 49,
        covariateCohortDatabaseSchema = "cohorts",
        covariateCohortTable = cohortTable,
        covariateCohorts = covariateCohorts
      )
    )
  } else {
    covariateSettings <- FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE,
      useConditionOccurrenceLongTerm = TRUE
    )
  }
  return(covariateSettings)
}

covariateSettings <- getCovariateSettings(
  cohorts = TRUE,
  connectionDetails = connectionDetails,
  cohortTable = "temporal_cohorts"
)

plpData <- PatientLevelPrediction::getPlpData(
  databaseDetails = PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cdmDatabaseName = "ducky",
    cohortDatabaseSchema = "cohorts",
    cohortTable = "temporal_cohorts",
    outcomeDatabaseSchema = "cohorts",
    outcomeTable = "temporal_cohorts",
    targetId = 242,
    outcomeIds = 243
  ),
  covariateSettings = covariateSettings,
)
populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE,
  riskWindowEnd = 30,
  removeSubjectsWithPriorOutcome = FALSE
)
split(
  plpData = plpData,
  populationSettings = populationSettings,
  N = 5,
  underSample = FALSE,
  subjectSplit = TRUE
)
