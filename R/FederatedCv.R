#' Outer + inner CV for federated hyperparameter tuning
#' @param dataList   list of length M, each element $X,$y
#' @param w0         initial parameter vector
#' @param hyperGrid  data.frame or list of lists of hyper‐parameters to try
#' @param k          clientFrac
#' @param rounds     outer rounds
#' @param popSettings        (only if you need to re‐build population)
#' @return a data.frame of outer‐fold test metrics + best hyperparams
#' @importFrom Metrics auc
#' @export
federatedNestedCv <- function(clientHosts,
                              clientPaths,
                              popSettings,
                              algorithm,
                              hyperGrid,
                              rounds,
                              clientFrac,
                              epsilon = 1e-6) {
  m <- length(clientHosts)
  outerResults <- vector("list", m)

  for (testIdx in seq_len(m)) {
    testHost <- clientHosts[testIdx]
    testPath <- clientPaths[testIdx]
    trainHosts <- clientHosts[-testIdx]
    trainPaths <- clientPaths[-testIdx]

    innerM <- length(trainHosts)
    hyperPerf <- lapply(seq_along(hyperGrid), function(hIdx) {
      hp <- hyperGrid[[hIdx]]
      config <- list(
        etaClient = hp$etaClient,
        etaServer = hp$etaServer,
        k = hp$k,
        rounds = rounds,
        lambda = hp$lambda,
        mapType = hp$mapType,
        intercept = hp$intercept,
        profile = hp$profile,
        epsilon = epsilon,
        clientFrac = clientFrac
      )
      foldMetrics <- lapply(seq_len(innerM), function(vIdx) {
        valHost <- trainHosts[vIdx]
        valPath <- trainPaths[vIdx]
        innerTrainHosts <- trainHosts[-vIdx]
        innerTrainPaths <- trainPaths[-vIdx]
        res <- runFederated(innerTrainHosts, innerTrainPaths, popSettings, algorithm, config)
        w <- res$w
        evaluateClient(w, valHost, valPath, popSettings)
      })
      # average metrics over inner folds
      do.call(rbind, foldMetrics) |> colMeans()
    })

    innerDf <- do.call(rbind, hyperPerf)
    bestIdx <- which.max(innerDf[, "accuracy"])
    bestHp <- hyperGrid[[bestIdx]]
    configBest <- list(
      etaClient = bestHp$etaClient,
      etaServer = bestHp$etaServer,
      k = bestHp$k,
      rounds = rounds,
      lambda = bestHp$lambda,
      mapType = bestHp$mapType,
      intercept = bestHp$intercept,
      profile = bestHp$profile,
      epsilon = epsilon,
      clientFrac = clientFrac
    )
    resFinal <- runFederated(trainHosts, trainPaths, popSettings, algorithm, configBest)
    wFinal <- resFinal$w
    testMetrics <- evaluateClient(wFinal, testHost, testPath, popSettings)
    outerResults[[testIdx]] <- c(bestHp, testMetrics)
  }

  do.call(rbind, lapply(outerResults, as.data.frame))
}
#' Evaluate model weights on a single client
#' @param w numeric vector of model coefficients
#' @param clientHost hostname
#' @param clientPath path to client data
#' @param popSettings population settings
#' @return named list of metrics: accuracy, auc, logloss, density
evaluateClient <- function(w, clientHost, clientPath, popSettings) {
  cl <- parallel::makeCluster(1)
  on.exit(stopCluster(cl))
  clusterLoadData(cl, clientHost, popSettings)
  data <- parallel::clusterEvalQ(cl, list(xMatrix = xMatrix, yLabels = yLabels))[[1]]
  xMatrix <- data$xMatrix
  yLabels <- data$yLabels
  pHat <- plogis(as.numeric(xMatrix %*% w))
  list(
    accuracy = FederatedLearning::accuracy(yLabels, pHat),
    auc      = Metrics::auc(yLabels, pHat),
    logloss  = FederatedLearning::logLoss(yLabels, pHat),
    density  = FederatedLearning::density(w)
  )
}
