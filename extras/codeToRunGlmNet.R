
# debug script to run the algorithms
# pkgload::load_all()
lambda <- 14.14
epsilon <- 1e-6
# preprocessing
plpData <- FederatedLearning::loadClientData("./data/client1/", popSettings = PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE,
    riskWindowEnd = 5 * 365
  )
)

covRef <- FederatedLearning::getClientFeatures(plpData)
covRefList <- list(covRef)
config <- list()
config$mapType <- "intersection"
globalMap <- FederatedLearning::createGlobalMap(covRefList, type = config$mapType)

config$p <- nrow(globalMap)
config$mapping <- globalMap
config$intercept <- TRUE

clientData <- FederatedLearning::createClientMatrix(
  plpData,
  config = config
)

logisticRegression::run_lasso(
  x_matrix = clientData$xMatrix,
  y = clientData$yLabels,
  prior_var = 0.01,
  algorithm = "glmnet",
  init_intercept = FALSE
)
