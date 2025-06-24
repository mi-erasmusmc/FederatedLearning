serverInitDA <- function(config) {
  if (config$intercept) {
    intercept <- 1
  } else {
    intercept <- 0
  }
  z <- rep(0, config$p + intercept)
  list(z = z)
}

#' One client's K‐step dual‐averaging loop
#' @param clientState list(z=dualState)
#' @param xMatrix local xMatrix (n × p)
#' @param yLabels local yLabels (length n, in {0,1})
#' @param etaClient client learning rate
#' @param etaServer server learning rate
#' @param roundIdx integer current federated round (0‐based)
#' @param k integer number of local updates
#' @param lambda regularization weight
#' @return numeric deltaZ = z_final - z_initial
#' @export
clientUpdateDA <- function(clientData,
                           serverBroadcast,
                           config) {
  z <- serverBroadcast$z
  # client state is global in client
  for (i in seq_len(config$k) - 1) {
    # ˜η = η_s·η_c·round·K + η_c·k
    etaTilde <- 
      config$etaServer * config$etaClient * serverBroadcast$r * config$k + config$etaClient * i
    # primal retrieval
    w <- FederatedLearning::proxL1(z, alphaLambda = etaTilde * config$lambda)
    # full batch gradient
    g <- FederatedLearning::gradLogistic(w, clientData$xMatrix, clientData$yLabels)
    # dual update
    z <- z - config$etaClient * g
  }
  delta <- z - serverBroadcast$z
  list(delta = delta)
}

#' One server round
#' @param serverState state of server 
#' @param clientReports what to get from clients
#' @param config configuration list
#' @export
serverRoundDA <- function(serverState,
                          clientReports,
                          config) {
  # average over clients
  deltaList <- lapply(clientReports, function(x) x$delta)
  delta <- Reduce("+", deltaList) / length(deltaList)
  # server update
  z <- serverState$z + config$etaServer * delta
  # return new dual state
  # and primal state
  # primal retrieval
  coef <- config$etaServer * config$etaClient * (serverState$r + 1) * config$k
  w <- FederatedLearning::proxL1(z, alphaLambda = coef * config$lambda)
  list(state = list(z = z),
       report = list(w = w))
}

.registerAlgorithm("DualAvg",
  serverInit     = serverInitDA,
  clientInit     = NULL,
  clientUpdate   = clientUpdateDA,
  serverRound    = serverRoundDA
)
