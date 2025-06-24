serverInitProxDrift <- function(config) {
  p <- config$p + as.integer(config$intercept)
  x0 <- rep(0, p) # global model xᵣ
  # compute P_{~ηg}(x) one time
  pX0 <- proxL1(x0,
    alphaLambda = config$tildeEta * config$lambda,
    intercept = config$intercept
  )
  corr0 <- numeric(p)

  list(
    pX = pX0,
    corr = corr0
  )
}

clientInitProxDrift <- function(serverState, config) {
  clientState <- list(
    c = numeric(length(serverState$pX)),
    sumGrad = numeric(length(serverState$pX))
  )
  assign("clientState", clientState, envir = .GlobalEnv)
  NULL
}

clientUpdateProxDrift <- function(
    clientData,
    serverBroadcast,
    config) {
  state <- clientState
  cI <- state$c
  sumGrad <- state$sumGrad


  # pull in broadcast from server
  pXR <- serverBroadcast$pX
  corr <- serverBroadcast$corr

  eta <- config$etaClient
  etaG <- config$etaServer
  k <- config$k
  lambda <- config$lambda

  # update correction term

  cI <- corr - (1 / k) * sumGrad

  bzr <- pXR
  zr <- bzr

  sumGrad <- numeric(length(bzr))

  for (t in seq_len(k) - 1) {
    # average gradient at current post prox model
    grad <- gradLogistic(zr, clientData$xMatrix, clientData$yLabels)
    sumGrad <- sumGrad + grad

    # update pre prox with correction
    bzr <- bzr - eta * (grad + cI)

    # post prox step
    alphaT1 <- (t + 1) * etaG
    zr <- proxL1(bzr,
      alphaLambda = alphaT1 * lambda,
      intercept = config$intercept
    )
  }
  # update client state
  clientState <<- list(
    c = cI,
    sumGrad = sumGrad
  )
  list(
    bzr = bzr,
    sumGrad = sumGrad
  )
}

serverRoundProxDrift <- function(
    serverState,
    clientReports, config) {
  meanBzr <- Reduce(`+`, lapply(clientReports, `[[`, "bzr")) / length(clientReports)

  pXR <- serverState$pX


  # update global pre-prox model
  xNew <- pXR + config$etaServer * (meanBzr - pXR)


  # global post-prox step
  pXNew <- proxL1(
    z = xNew,
    alphaLambda = config$tildeEta * config$lambda,
    intercept = config$intercept
  )

  # update correction term
  corrNew <- (pXR - xNew) / (config$etaServer * config$etaClient * config$k)
  list(
    state = list(
      x = xNew,
      pX = pXNew,
      corr = corrNew
    ),
    report = list(
      w = pXNew
    )
  )
}


.registerAlgorithm("ProxDrift",
  serverInit   = serverInitProxDrift,
  clientInit   = clientInitProxDrift,
  clientUpdate = clientUpdateProxDrift,
  serverRound  = serverRoundProxDrift
)
