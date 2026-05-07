utils::globalVariables(c("clientData", "clientState", "plpData"))

.registerAlgorithm("DualAvgCpp",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp,
  supportsClientSampling = TRUE
)
