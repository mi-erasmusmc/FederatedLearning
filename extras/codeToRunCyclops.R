resultsDirectory <- "./results/readmissionCyclopsPhenotypes"
if (!dir.exists(resultsDirectory)) {
  dir.create(resultsDirectory, recursive = TRUE)
}
popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  riskWindowEnd = 30,
  requireTimeAtRisk = FALSE,
  removeSubjectsWithPriorOutcome = FALSE
)
dataPath <- "./data/readmission/"
clientPaths <- c(
  file.path(dataPath, "client1"),
  file.path(dataPath, "client2"),
  file.path(dataPath, "client3"),
  file.path(dataPath, "client4"),
  file.path(dataPath, "client5")
)

# load and merge data

clientIds <- seq_len(length(clientPaths))

mergeData <- function(plpDataList, ids) {
  plpData <- list()
  class(plpData) <- "plpData"
  plpData$cohorts <- do.call(rbind, lapply(plpDataList[ids], function(x) x$cohorts))
  plpData$outcomes <- do.call(rbind, lapply(plpDataList[ids], function(x) x$outcomes))
  plpData$metaData$databaseDetails <- plpDataList[[1]]$metaData$databaseDetails

  plpData$covariateData <- Andromeda::copyAndromeda(plpDataList[[ids[[1]]]]$covariateData)
  for (i in ids[[2]]:ids[[length(ids)]]) {
    Andromeda::appendToTable(
      plpData$covariateData$covariates,
      plpDataList[[i]]$covariateData$covariates
    )
    Andromeda::appendToTable(
      plpData$covariateData$covariateRef,
      plpDataList[[i]]$covariateData$covariateRef
    )
    Andromeda::appendToTable(
      plpData$covariateData$analysisRef,
      plpDataList[[i]]$covariateData$analysisRef
    )
  }
  class(plpData$covariateData) <- "CovariateData"
  popList <- lapply(seq_along(ids), function(k) {
    id <- ids[k]
    tmp <- plpDataList[[id]]$cohorts

    data.frame(
      rowId = tmp$rowId,
      group = id,
      stringsAsFactors = FALSE
    )
  })
  plpData$folds <- do.call(rbind, popList)
  return(plpData)
}

plpDataList <- lapply(clientPaths, function(path) {
  PatientLevelPrediction::loadPlpData(path)
})
plpData <- mergeData(plpDataList, ids = clientIds)
splitIds <- plpData$folds |>
  dplyr::select("rowId", "group") |>
  dplyr::mutate(index = .data$group) |>
  dplyr::select("rowId", "index")
folds <- plpData$folds |>
  dplyr::pull(.data$group) |>
  unique()
results <- list()
for (fold in folds) {
  splitIdsLoop <- splitIds
  testFold <- fold
  trainFolds <- folds[folds != testFold]
  
  mappingVector <- c(setNames(-1, testFold),
                     setNames(1:(length(folds) - 1), trainFolds))
  splitIdsLoop$index <- as.integer(mappingVector[as.character(splitIdsLoop$index)])
  splitSettingsLoop <- PatientLevelPrediction::createExistingSplitSettings(splitIds = splitIdsLoop)
  plpResults <-  PatientLevelPrediction::runPlp(
      plpData = plpData,
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(seed = 42),
      analysisId = as.character(fold),
      analysisName = "test",
      populationSettings = popSettings,
      splitSettings = splitSettingsLoop,
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
      logSettings = PatientLevelPrediction::createLogSettings(verbosity = "TRACE"),
      executeSettings = PatientLevelPrediction::createExecuteSettings(
        runSplitData = TRUE,
        runSampleData = FALSE,
        runFeatureEngineering = FALSE,
        runPreprocessData = TRUE,
        runModelDevelopment = TRUE,
        runCovariateSummary = FALSE
      ),
      saveDirectory = resultsDirectory
    )
  auc <- plpResults$performanceEvaluation$evaluationStatistics |> 

    dplyr::filter(.data$evaluation == "Test", .data$metric == "AUROC") |>
    dplyr::pull(.data$value)
  auc <- auc[[1]]

  message("AUC for fold ", fold, " is ", auc)
  variance <- plpResults$model$model$priorVariance
  lambda <- sqrt(2 * variance) / nrow(plpData$cohorts)
  time <- plpResults$model$trainDetails$trainingTime

  results[[fold]] <- list(
    auc = auc, 
    lambda = lambda, 
    epsilon = 1e-6, 
    time = time, 
    log_likelihood = plpResults$model$model$log_likelihood)  
}

allResults <- do.call(rbind, lapply(results, as.data.frame))

print(allResults)
write.csv(allResults, file.path(resultsDirectory, 
                             "nested_cv_results_full.csv"), 
          row.names = FALSE)
