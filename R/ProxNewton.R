serverInitProxNewton <- function(config) {
  p <- config$p + as.integer(config$intercept)
  w0 <- numeric(p)
  list(w = w0)
}

clientInitProxNewton <- function(serverState, config) {
  assign("modelW", serverState$w, envir = .GlobalEnv)
  NULL
}

clientUpdateProxNewton <- function(clientData,
                                   serverBroadcast,
                                   config) {
  w <- serverBroadcast$w

  pVec <- as.numeric(plogis(as.vector(clientData$xMatrix %*% w)))
  residuals <- clientData$yLabels - pVec

  # gradient
  grad <- crossprod(clientData$xMatrix, residuals)
  sumResiduals <- sum(residuals)
  grad <- grad - clientData$xMeans * sumResiduals

  # Hessian diagonal: ∂²L/∂w_j² = 1/n ∑_i x_ij^2 * p_i*(1-p_i)
  wDiag <- pVec * (1 - pVec)
  wDiag <- pmax(wDiag, config$epsThresh)
  sumWd <- sum(wDiag)
  hess <- Matrix::colSums((clientData$xMatrix^2) * wDiag)
  c2 <- crossprod(clientData$xMatrix, wDiag)
  hess <- hess - 2 * clientData$xMeans * c2 + clientData$x2Means * sumWd
  list(
    grad = as.numeric(grad),
    hess = as.numeric(hess)
  )
}

serverRoundProxNewton <- function(serverState,
                                  clientReports,
                                  config) {
  # 1) aggregate gradient & Hessian‐diags
  grads <- sapply(clientReports, `[[`, "grad")
  hess <- sapply(clientReports, `[[`, "hess")
  gBar <- Matrix::rowMeans(grads)
  hBar <- Matrix::rowMeans(hess)
  hBar <- pmax(hBar, config$epsThresh)

  wNew <- serverState$w
  sweeps <- config$cdSweeps
  lambda <- config$lambda
  for (s in seq_len(sweeps)) {
    for (j in seq_along(wNew)) {
      if (config$intercept && j == 1) next
      # Newton‐style CD update for coordinate j
      uj <- wNew[j] + gBar[j] / hBar[j]
      wNew[j] <- sign(uj) * pmax(abs(uj) - lambda / hBar[j], 0)
    }
    if (config$intercept) {
      wNew[1] <- wNew[1] + gBar[1] / hBar[1]
    }
  }

  list(
    state     = list(w = wNew),
    report    = list(w = wNew) # for logging objective etc.
  )
}

.registerAlgorithm("ProxNewton",
  serverInit    = serverInitProxNewton,
  clientInit    = clientInitProxNewton,
  clientUpdate  = clientUpdateProxNewton,
  serverRound   = serverRoundProxNewton
)
