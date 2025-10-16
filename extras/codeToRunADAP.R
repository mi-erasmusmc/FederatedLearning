library(FederatedLearning)

clientHosts <- c("localhost", "localhost", "localhost")
clientPaths <- c(
  "./data/lungCancerPhenotypes/client1",
  "./data/lungCancerPhenotypes/client2",
  "./data/lungCancerPhenotypes/client3"
)

popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE,
  riskWindowEnd = 3 * 365,
  removeSubjectsWithPriorOutcome = FALSE
)

cl <- clusterInit(clientHosts, clientPaths)
on.exit(FederatedLearning:::stopCluster(cl), add = TRUE)

clusterLoadData(cl, clientPaths, popSettings)

# Usual data prep via Orchestrator
config <- list(
  mapType = "intersection",
  intercept = TRUE,
  rounds = 2, # two rounds only
  epsilon = 1e-6, # stopping; not used here but OK
  k = 1, # not used by ADAP baseline
  etaClient = 1.0, # not used by ADAP baseline
  etaServer = 1.0, # not used by ADAP baseline
  lambda = 0.01 # lasso penalty (tune via CV if needed)
)

res <- fitFederated(cl, algorithm = "ADAP", config = config)
str(res$w) # final coefficients
res$globalObjective
