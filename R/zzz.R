utils::globalVariables(c("clientData", "plpData"))

.registerAlgorithm("DualAvgCpp",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp,
  supportsClientSampling = TRUE
)

.registerAlgorithm("DualAvg",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp,
  supportsClientSampling = TRUE
)
