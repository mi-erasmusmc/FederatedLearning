serverInitFastDA <- function(config) {
  if (config$intercept) {
    intercept <- 1
  } else {
    intercept <- 0
  }
  p <- config$p + intercept
  wtilde <- numeric(p)
  g <- numeric(p)
  w <- numeric(p)
  list(
    g = g,
    wtilde = wtilde,
    w = w
  )
}

clientUpdateFastDA <- function(clientData,
                               serverBroadcast,
                               config) {
  g <- serverBroadcast$g
  wtilde <- serverBroadcast$wtilde
  w <- serverBroadcast$w

  r <- serverBroadcast$r # round index from server (0-based)


  for (i in seq_len(config$k)) {
    t <- r * config$k + (i - 1)
    alpha <- (t + config$a)^2

    grad <- gradLogistic(
      w,
      clientData$xMatrix,
      clientData$yLabels
    )
    g <- g + alpha * grad

    if (i < config$k) {
      At <- sum(((0:t) + config$a)^2) # A = sum_{j=0}^{t} (j + a)^2
      zIn <- g - (config$mu / 2) * wtilde # shifted dual
      w <- proxL1Ridge(
        zIn,
        A = At,
        mu = config$mu,
        gamma = config$gamma,
        lambda = config$lambda,
        intercept = config$intercept
      )
      alphaNext <- ((t + 1) + config$a)^2
      wtilde <- wtilde + alphaNext * w
    }
  }
  list(
    g = g,
    wtilde = wtilde
  )
}


serverRoundFastDA <- function(serverState,
                              clientReports,
                              config) {
  gBar <- Reduce(`+`, lapply(clientReports, `[[`, "g")) / length(clientReports)
  wtildeBar <-
    Reduce(`+`, lapply(clientReports, `[[`, "wtilde")) / length(clientReports)

  r <- serverState$r
  k <- config$k
  mu <- config$mu
  a <- config$a
  lambda <- config$lambda

  tNext <- (r + 1) * k - 1
  aNext <- sum(((0:tNext) + a)^2)
  alphaNext <- (tNext + a)^2

  zIn <- gBar - (mu / 2) * wtildeBar
  w <- proxFastL1(
    zIn,
    A = aNext,
    mu = mu,
    lambda = lambda,
    intercept = config$intercept
  )
  wTildeNew <- wtildeBar + alphaNext * w

  newState <- list(
    g = gBar,
    wtilde = wTildeNew,
    w = w
  )

  list(
    state = newState,
    report = list(w = w)
  )
}

.registerAlgorithm("FastDualAvg",
  serverInit = serverInitFastDA,
  clientInit = NULL,
  clientUpdate = clientUpdateFastDA,
  serverRound = serverRoundFastDA
)
