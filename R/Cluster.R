#' @export
clusterInit <- function(clientHosts, clientPaths, mirai = TRUE) {
  stopifnot(length(clientHosts) == length(clientPaths))
  if (mirai) {
    cl <- mirai::make_cluster(n = length(clientHosts))
  } else {
    cl <- parallelly::makeClusterPSOCK(
      workers = clientHosts,
      rscript_libs = .libPaths(), # TODO is this needed?
      default_packages = c("PatientLevelPrediction", "FederatedLearning"),
    )
  }
  cl
}

#' @export
clusterLoadData <- function(cl, clientPaths, popSettings) {
  popSizes <- parallel::clusterApply(
    cl,
    seq_along(clientPaths),
    function(i, clientPaths, popSettings) {
      plpData <- FederatedLearning::loadClientData(
        clientPaths[i],
        popSettings = popSettings
      )
      assign("plpData", plpData, envir = .GlobalEnv)
      nrow(plpData$population)
    },
    clientPaths = clientPaths,
    popSettings = popSettings
  )
  popSizes
}

#' @export
clusterCollectCovRefs <- function(cl, type = "union") {
  covRefList <- parallel::clusterEvalQ(
    cl,
    {
      covariateRef <- FederatedLearning::getClientFeatures(plpData)
    }
  )
  # covRefList <- parallel::clusterCall(
  #   cl,
  #   function() {
  #     covariateRef <- FederatedLearning::getClientFeatures(plpData)
  #     covariateRef
  #   }
  # )
  globalMap <- FederatedLearning::createGlobalMap(covRefList, type = type)
  globalMap
}

#' @export
clusterCreateMatrices <- function(cl, config) {
  parallel::clusterCall(
    cl,
    function(mapping, config) {
      config <- within(config, {
        mapping <- mapping
        p <- nrow(mapping)
      })
      clientData <- FederatedLearning::createClientMatrix(
        plpData,
        config = config
      )
      assign("clientData", clientData, envir = .GlobalEnv)
      NULL
    },
    mapping = config$mapping,
    config = config
  )
}

clusterPredict <- function(cl, w) {
  metrics <- parallel::clusterCall(
    cl,
    function(w) {
      preds <- stats::plogis(as.numeric(clientData$xMatrix %*% w))
      auc <- Metrics::auc(clientData$yLabels, preds)
      auc
    },
    w = w
  )
  metrics <- unlist(metrics)
}

stopCluster <- function(cl) {
  parallel::stopCluster(cl)
}
