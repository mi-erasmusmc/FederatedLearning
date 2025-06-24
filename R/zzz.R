.registerAlgorithm("DualAvgCpp",
  serverInit = serverInitDualAveragingCpp,
  clientUpdate = clientUpdateDualAveragingCpp,
  clientInit = NULL,
  serverRound  = serverRoundDualAveragingCpp
)
