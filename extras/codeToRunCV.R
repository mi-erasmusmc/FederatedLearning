# Code to run federated nested CV
clientHosts <- c("localhost", "localhost", "localhost", "localhost", "localhost")
path <- "./data/readmissionPhenotypes"
resultDirectory <- "./results/readmissionPhenotypes"
if (!dir.exists(resultDirectory)) dir.create(resultDirectory, recursive = TRUE)
clientPaths <- c(file.path(path, "client1"), 
                 file.path(path, "client2"),
                 file.path(path, "client3"),
                 file.path(path, "client4"),
                 file.path(path, "client5"))
stopifnot(length(clientHosts) == length(clientPaths))

popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE,
  riskWindowEnd = 30,
  removeSubjectsWithPriorOutcome = FALSE
)

algorithm <- "DualAvgCpp"

paramGrid <- list(
  etaClient = c(1.0),
  etaServer = c(1.0),
  k = c(10),
  lambda = c(14.14214), # unnormalized
  mapType = c("intersection"),
  intercept = c(TRUE),
  profile = c(FALSE)
)
paramGrid$tildeEta <- paramGrid$etaClient * paramGrid$etaServer * paramGrid$k

gridDf <- expand.grid(paramGrid, stringsAsFactors = FALSE)
hyperGrid <- split(gridDf, seq_len(nrow(gridDf)))

rounds <- 10000
clientFrac <- 1

results <- FederatedLearning::federatedNestedCv(
  clientHosts = clientHosts,
  clientPaths = clientPaths,
  popSettings = popSettings,
  algorithm   = algorithm,
  hyperGrid   = hyperGrid,
  rounds      = rounds,
  clientFrac  = clientFrac,
  resultDirectory = resultDirectory,
  epsilon     = 1e-6
)

print(results)
write.csv(results, file.path(resultDirectory, 
                             "nested_cv_results_full.csv"), 
          row.names = FALSE)

