# Code to run federated nested CV
clientHosts <- c("localhost", "localhost", "localhost", "localhost", "localhost")
clientPaths <- c("./data/client1", "./data/client2", "./data/client3", "./data/client4", "./data/client5")
stopifnot(length(clientHosts) == length(clientPaths))

popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE,
  riskWindowEnd = 5 * 365
)

algorithm <- "DualAvg"

paramGrid <- list(
  etaClient = c(0.01, 0.1, 1.0),
  etaServer = c(0.01, 0.1, 1.0),
  k = c(1, 5, 10),
  lambda = c(1e-3, 1e-4, 1e-2),
  mapType = c("union", "intersection"),
  intercept = c(TRUE, FALSE),
  profile = c(FALSE)
)

gridDf <- expand.grid(paramGrid, stringsAsFactors = FALSE)
hyperGrid <- apply(gridDf, 1, as.list)

rounds <- 100
clientFrac <- 1

results <- FederatedLearning::federatedNestedCv(
  clientHosts = clientHosts,
  clientPaths = clientPaths,
  popSettings = popSettings,
  algorithm   = algorithm,
  hyperGrid   = hyperGrid,
  rounds      = rounds,
  clientFrac  = clientFrac,
  epsilon     = 1e-6
)

print(results)
write.csv(results, "nested_cv_results.csv", row.names = FALSE)
