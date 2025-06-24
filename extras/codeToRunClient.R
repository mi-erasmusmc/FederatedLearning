# debug script to run the algorithms
# pkgload::load_all()

algorithm <- "ProxNewton" # or "DualAvg", "FastDualAvg", etc.
algo <- .getAlgorithm(algorithm)

config <- list(
  gamma = c(1e-6),
  cdSweeps = 1,
  mu = c(0.001),
  a = c(1),
  etaClient = c(1.0),
  etaServer = c(1.0),
  k = c(10),
  # lambda = c(2.09e-4),
  lambda = 14.14,
  mapType = c("intersection"),
  intercept = c(TRUE),
  profile = c(FALSE),
  rounds = 10000,
  epsilon = 1e-6,
  clientFrac = 1,
  epsThresh = 1e-8
)
config$tildeEta <- config$etaClient * config$etaServer * config$k

# preprocessing
plpData <- FederatedLearning::loadClientData("./data/client1/", popSettings = PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE,
    riskWindowEnd = 5 * 365
  )
)

covRef <- FederatedLearning::getClientFeatures(plpData)
covRefList <- list(covRef)

globalMap <- FederatedLearning::createGlobalMap(covRefList, type = config$mapType)


config$p <- nrow(globalMap)
config$mapping <- globalMap


clientData <- FederatedLearning::createClientMatrix(
  plpData,
  config = config
)

serverState <- algo$serverInit(config)

# 5) optional per‐client init hook
if (!is.null(algo$clientInit)) {
  algo$clientInit(serverState, config)
}

getLocalObjective <- function(w) {
  n <- nrow(clientData$xMatrix)
  linearPred <- as.numeric(clientData$xMatrix %*% w)
  loss <- sum(clientData$yLabels * linearPred - log1p(exp(linearPred)))
  list(
    objective = loss,
    n = n
  )
}

for (r in 0:(config$rounds - 1)) {
  serverState$r <- r

  report <- algo$clientUpdate(
    clientData = clientData,
    serverBroadcast = serverState,
    config = config
  )

  srv <- algo$serverRound(
    serverState   = serverState,
    clientReports = list(report),
    config        = config
  )
  serverState <- srv$state
  serverReport <- srv$report

  wnew <- serverReport$w %||% serverReport$x %||% serverReport$pX
  obj <- getLocalObjective(wnew)
  cat("Local objective:", obj$objective, "\n\n")
}

cat("Done.\n")
