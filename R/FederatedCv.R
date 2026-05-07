#' Outer + inner CV for federated hyperparameter tuning
#' @param clientHosts worker host names or addresses
#' @param clientPaths paths to client PLP data folders
#' @param popSettings PatientLevelPrediction population settings
#' @param algorithm registered federated learning algorithm name
#' @param hyperGrid  data.frame or list of lists of hyper-parameters to try
#' @param rounds     outer rounds
#' @param clientFrac fraction of clients sampled per round
#' @param resultDirectory directory for logs and results
#' @param epsilon convergence tolerance
#' @param mirai if TRUE, use `mirai`; otherwise use PSOCK workers
#' @return a data.frame of outer-fold test metrics + best hyperparams
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
                              epsilon = 1e-6,
                              mirai = FALSE) {
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

  algo <- .getAlgorithm(algorithm)
  if (is.null(algo)) {
    stop(sprintf("Algorithm '%s' is not registered", algorithm))
  }
  lambdaStrategy <- algo$lambdaStrategy %||% .lambdaStrategyDefault()

  # init cluster and client states
  cl <- clusterInit(clientHosts, clientPaths, mirai = mirai)
  on.exit(stopCluster(cl), add = TRUE)
  popSizes <- clusterLoadData(cl, clientPaths, popSettings)
  globalMap <- clusterCollectCovRefs(
    cl,
    type = hyperGrid[[1]]$mapType,
    featureSet = hyperGrid[[1]]$featureSet,
    covariateIds = hyperGrid[[1]]$covariateIds,
    analysisIds = hyperGrid[[1]]$analysisIds
  )
  totalPopSize <- Reduce("+", popSizes)
  clientIds <- seq_len(m)
  for (testIdx in clientIds) {
    message(sprintf("Outer fold %d/%d", testIdx, m))
    trainIds <- clientIds[-testIdx]
    clTrain <- subsetCluster(cl, trainIds)
    clTest <- subsetCluster(cl, testIdx)

    hyperResults <- lapply(hyperGrid, function(hp) {
      hpList <- if (is.data.frame(hp)) as.list(hp[1, , drop = FALSE]) else as.list(hp)
      hpBase <- hpList[!names(hpList) %in% "lambda"]
      tuned <- if (identical(algorithm, "ADAP2")) {
        tuneLambdaLead(
          cl = clTrain,
          algorithm = algorithm,
          configBase = hpBase,
          trainIds = seq_along(trainIds),
          rounds = rounds,
          clientFrac = clientFrac,
          epsilon = epsilon,
          lambdaStrategy = lambdaStrategy,
          lambdaDefault = hpList$lambda,
          totalPopSize = totalPopSize,
          globalMap = globalMap
        )
      } else {
        tuneLambda(
          cl = clTrain,
          algorithm = algorithm,
          configBase = hpBase,
          trainIds = seq_along(trainIds),
          rounds = rounds,
          clientFrac = clientFrac,
          epsilon = epsilon,
          lambdaStrategy = lambdaStrategy,
          lambdaDefault = hpList$lambda,
          totalPopSize = totalPopSize,
          globalMap = globalMap
        )
      }
      paramsOut <- hpBase
      paramsOut$lambda <- tuned$bestLambda
      paramsOut$auc <- tuned$perf
      if (identical(algorithm, "ADAP2")) {
        paramsOut$cacheKey <- tuned$cacheKey %||% NA_character_
      }
      as.data.frame(paramsOut, stringsAsFactors = FALSE)
    })
    innerDf <- do.call(rbind, hyperResults)
    bestIdx <- which.max(innerDf$auc)
    bestRow <- innerDf[bestIdx, , drop = FALSE]
    configCols <- setdiff(names(bestRow), c("auc", "cacheKey"))
    configBest <- as.list(bestRow[configCols])
    configBest$profile <- configBest$profile %||% FALSE
    configBest$rounds <- rounds
    configBest$epsilon <- epsilon
    configBest$clientFrac <- clientFrac
    contextFinal <- list(
      cl = clTrain,
      configBase = configBest,
      rounds = rounds,
      clientFrac = clientFrac,
      epsilon = epsilon,
      totalPopSize = totalPopSize,
      globalMap = globalMap
    )
    trainConfig <- configBest
    trainConfig$mapping <- globalMap
    trainConfig$p <- nrow(globalMap)
    cacheKeyBest <- if ("cacheKey" %in% names(bestRow)) bestRow$cacheKey else NA_character_
    if (identical(algorithm, "ADAP2")) {
      trainConfig$lambda <- configBest$lambda
      trainConfig$request <- "fit"
      if (!is.na(cacheKeyBest)) {
        trainConfig$cacheKey <- cacheKeyBest
      }
      trainConfig$rounds <- max(rounds, 3L)
    } else {
      trainConfig$lambda <- lambdaStrategy$initial(configBest$lambda, totalPopSize, contextFinal)
      trainConfig$rounds <- rounds
    }
    trainConfig$epsilon <- epsilon
    trainConfig$clientFrac <- clientFrac
    resFinal <- fitFederated(clTrain, algorithm, trainConfig)
    wFinal <- resFinal$w
    testMetrics <- evaluateClient(clTest, wFinal, resFinal$config)
    outerResults[[testIdx]] <- c(configBest, list(auc = testMetrics))
  }
  delta <- Sys.time() - start
  message(sprintf("Federated nested CV completed in %s", delta))
  do.call(rbind, lapply(outerResults, as.data.frame))
}
#' Evaluate model weights on a single client
#' @param cl cluster object
#' @param w numeric vector of model coefficients
#' @param config model configuration list
#' @return named list of metrics: accuracy, auc, logloss, density
evaluateClient <- function(cl, w, config) {
  clusterCreateMatrices(cl, config)
  results <- clusterPredict(cl, w)
  results
}
