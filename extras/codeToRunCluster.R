clientHosts <- c("localhost", "localhost", "localhost")
clientPaths <- c("./data/client1", "./data/client2", "./data/client3")

popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE,
  riskWindowEnd = 5 * 365,
)

# config <- list(
#   etaClient = c(0.01, 0.1, 1.0),
#   etaServer = c(0.01, 0.1, 1.0),
#   k = c(1, 5, 10),
#   rounds = 100,
#   lambda = c(1e-3, 1e-4, 1e-2),
#   mapType = c("union", "intersection"),
#   intercept = TRUE,
#   profile = FALSE
# )
# allResults <- list()
# config <- expand.grid(config)
# for (i in seq_len(nrow(config))) {
#   c <- config[i, ]
#   cat("Running with config: ", i, "\n")
#   results <- FederatedLearning::runFederated(
#     clientHosts = clientHosts,
#     clientPaths = clientPaths,
#     popSettings = popSettings,
#     algorithm = "DualAvg",
#     config = list(
#       etaClient = c$etaClient,
#       etaServer = c$etaServer,
#       k = c$k,
#       rounds = c$rounds,
#       lambda = c$lambda,
#       mapType = as.character(c$mapType),
#       intercept = c$intercept,
#       profile = c$profile
#     )
#   )
#   allResults <- append(allResults, list(results))
# }
#
# objectives <- unlist(lapply(allResults, function(x) x$globalObjective))
# id <- which.max(objectives)
# bestConfig <- config[id, ]
bestConfig <- list(
  etaClient = 1,
  etaServer = 1,
  k = 10,
  rounds = 10000,
  lambda = 2.09e-4,
  mapType = "union",
  intercept = TRUE,
  profile = FALSE,
  epsilon = 1e-6
)
cl <- FederatedLearning::clusterInit(clientHosts, clientPaths)
on.exit(FederatedLearning:::stopCluster(cl), add = TRUE)
FederatedLearning::clusterLoadData(cl, clientPaths, popSettings)
fedFit <- FederatedLearning::runFederated(
  cl = cl,
  algorithm = "DualAvg",
  config = bestConfig)
