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
                              resultDirectory,
                              epsilon = 1e-6) {
  logName <- "federatedLog"
  logger <- ParallelLogger::createLogger(
      name = logName,
      threshold = "INFO",
      appenders = list(ParallelLogger::createFileAppender(
            layout = ParallelLogger::layoutParallel,
            fileName = file.path(resultDirectory, paste0(logName, ".txt")),
            expirationTime = 60 * 60 * 48
        )
      )
  )
  ParallelLogger::registerLogger(logger)
  on.exit(ParallelLogger::unregisterLogger(logger))
  start <- Sys.time()
  message("Starting federated nested CV...")
  m <- length(clientHosts)
  outerResults <- vector("list", m)

  # init cluster and client states
  cl <- clusterInit(clientHosts, clientPaths)
  on.exit(stopCluster(cl), add = TRUE)
  popSizes <- clusterLoadData(cl, clientPaths, popSettings)
  totalPopSize <- Reduce("+", popSizes)
  initLambda <- hyperGrid[[1]]$lambda / (3 * totalPopSize / 5) # inner loop fits on 3
  clientIds <- seq_len(m)
  hyperGridNoLambda <- lapply(hyperGrid, function(x) x[!names(x) %in% "lambda"])
  for (testIdx in clientIds) {
    message(sprintf("Outer fold %d/%d", testIdx, m))
    trainIds <- clientIds[-testIdx]

    hyperResults <- lapply(hyperGridNoLambda, function(hpBase) {
      tuned <- tuneLambda(
        cl = cl[trainIds],
        algorithm = algorithm,
        configBase = hpBase,
        trainIds = seq_along(trainIds),
        rounds = rounds,
        clientFrac = clientFrac,
        epsilon = epsilon,
        initLambda =  initLambda
      )
      data.frame(
        etaClient = hpBase$etaClient,
        etaServer = hpBase$etaServer,
        k = hpBase$k,
        lambda = tuned$bestLambda,
        mapType = hpBase$mapType,
        intercept = hpBase$intercept,
        profile = hpBase$profile,
        auc = tuned$perf
      )
    })
    innerDf <- do.call(rbind, hyperResults)
    bestIdx <- which.max(innerDf$auc)
    bestRow <- innerDf[bestIdx, ]
    configBest <- as.list(bestRow[c(
      "etaClient", "etaServer", "k", "mapType", "intercept", "profile", "lambda"
    )])
    configBest$lambda <- configBest$lambda * 3 / 4 # renormalize to 4 sets instead of 3
    configBest$rounds <- rounds
    configBest$epsilon <- epsilon
    configBest$clientFrac <- clientFrac
    resFinal <- fitFederated(cl[trainIds], algorithm, configBest)
    wFinal <- resFinal$w
    testMetrics <- evaluateClient(cl[testIdx], wFinal, resFinal$config)
    outerResults[[testIdx]] <- c(configBest, list(auc = testMetrics))
  }
  delta <- Sys.time() - start
  message(sprintf("Federated nested CV completed in %s", delta))
  do.call(rbind, lapply(outerResults, as.data.frame))
}
#' Evaluate model weights on a single client
#' @param cl cluster object
#' @param w numeric vector of model coefficients
#' @param columnMap map for covariateId to column index
#' @return named list of metrics: accuracy, auc, logloss, density
evaluateClient <- function(cl, w, config) {
  clusterCreateMatrices(cl, config)
  results <- clusterPredict(cl, w)
  results
}
