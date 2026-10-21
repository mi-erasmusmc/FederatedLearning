utils::globalVariables(c("clientData", "plpData"))

.registerAlgorithm("DualAvgCpp",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp,
  supportsClientSampling = TRUE,
  protocol = .dualAvgProtocol
)

.registerAlgorithm("DualAvg",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp,
  supportsClientSampling = TRUE,
  protocol = .dualAvgProtocol
)
