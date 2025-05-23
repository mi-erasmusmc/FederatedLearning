#' Outer + inner CV for federated hyperparameter tuning
#' @param dataList   list of length M, each element $X,$y
#' @param w0         initial parameter vector
#' @param hyperGrid  data.frame or list of lists of hyper‐parameters to try
#' @param k          clientFrac
#' @param rounds     outer rounds
#' @param popSettings        (only if you need to re‐build population)
#' @return a data.frame of outer‐fold test metrics + best hyperparams
federatedNestedCv <- function(dataList,
                              w0,
                              hyperGrid,
                              k,
                              rounds,
                              clientFrac) {
  m <- length(dataList)
  outerResults <- vector("list", m)

  for (testIdx in seq_len(m)) {
    # split outer
    testClient <- dataList[[testIdx]]
    trainClients <- dataList[-testIdx]

    innerM <- length(trainClients)
    hyperPerf <- lapply(seq_along(hyperGrid), function(hIdx) {
      hp <- hyperGrid[[hIdx]]

      foldPerf <- sapply(seq_len(innerM), function(vIdx) {
        valClient <- trainClients[[vIdx]]
        innerTrain <- trainClients[-vIdx]

        hist <- FederatedLearning::federatedDualAveraging(
          dataList    = innerTrain,
          w0          = w0,
          etaC        = hp$etaC,
          etaS        = hp$etaS,
          k           = k,
          R           = rounds,
          clientFrac  = clientFrac,
          lambda      = hp$lambda
        )
        wFinal <- tail(hist$wHistory, 1)[[1]]
        pHatVal <- plogis(valClient$xMatrix %*% wFinal)

        c(
          accuracy = FederatedLearning::accuracy(valClient$y, pHatVal),
          logloss = FederatedLearning::logLoss(valClient$y, pHatVal),
          density = FederatedLearning::density(wFinal)
        )
      })
      # average over inner folds
      colMeans(foldPerf)
    })

    innerDf <- do.call(rbind, hyperPerf)
    bestIdx <- which.max(innerDf[, "accuracy"])
    bestHp <- hyperGrid[[bestIdx]]

    histFinal <- FederadLearning::federatedDualAveraging(
      dataList = trainClients,
      w0 = w0,
      etaC = bestHp$etaC,
      etaS = bestHp$etaS,
      k = k,
      rounds = rounds,
      clientFrac = clientFrac,
      lambda = bestHp$lambda
    )
    wFinal <- tail(histFinal$wHistory, 1)[[1]]
    pHatTest <- plogis(testClient$xMatrix %*% wFinal)
    testMetrics <- c(
      accuracy = FederatedLearning::accuracy(testClient$y, pHatTest),
      logloss  = FederatedLearning::logLoss(testClient$y, pHatTest),
      density  = FederatedLearning::density(wFinal)
    )

    outerResults[[testIdx]] <- c(bestHp, testMetrics)
  }

  # bind into a single data.frame
  do.call(rbind, lapply(outerResults, as.data.frame))
}
