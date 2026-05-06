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
clusterCollectCovRefs <- function(cl,
                                  type = "union",
                                  featureSet = NULL,
                                  covariateIds = NULL,
                                  analysisIds = NULL) {
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
  globalMap <- FederatedLearning::createGlobalMap(
    covRefList,
    type = type,
    featureSet = featureSet,
    covariateIds = covariateIds,
    analysisIds = analysisIds
  )
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

#' Collect basic per-client diagnostics for the current PLP data
#' @param cl cluster object
#' @param config optional config with mapping/feature set
#' @return data.frame with sample, outcome, and feature diagnostics
#' @export
clusterDiagnostics <- function(cl, config = list()) {
  rows <- parallel::clusterApply(
    cl,
    seq_along(cl),
    function(i, config) {
      pop <- plpData$population
      covRef <- FederatedLearning::getClientFeatures(plpData)
      filtered <- FederatedLearning::filterCovariateRef(
        covRef,
        featureSet = config$featureSet %||% "all",
        covariateIds = config$covariateIds,
        analysisIds = config$analysisIds
      )
      if (exists("clientData", envir = .GlobalEnv, inherits = FALSE)) {
        pLocal <- ncol(clientData$xMatrix)
      } else if (!is.null(config$mapping)) {
        pLocal <- nrow(config$mapping) + as.integer(isTRUE(config$intercept))
      } else {
        pLocal <- NA_integer_
      }
      data.frame(
        client = i,
        n = nrow(pop),
        outcomes = sum(as.integer(pop$outcomeCount) > 0),
        outcomeRate = mean(as.integer(pop$outcomeCount) > 0),
        covariatesAvailable = nrow(covRef),
        covariatesSelected = nrow(filtered),
        matrixColumns = pLocal,
        stringsAsFactors = FALSE
      )
    },
    config = config
  )
  do.call(rbind, rows)
}

#' Evaluate model weights on every client with multiple metrics
#' @param cl cluster object
#' @param w numeric model coefficients
#' @param threshold density threshold
#' @return data.frame with AUC, log loss, calibration intercept/slope
#' @export
clusterEvaluateModel <- function(cl, w, threshold = 1e-4) {
  rows <- parallel::clusterApply(
    cl,
    seq_along(cl),
    function(i, w, threshold) {
      lin <- as.numeric(clientData$xMatrix %*% w)
      preds <- stats::plogis(lin)
      y <- clientData$yLabels
      aucVal <- if (length(unique(y)) == 2) {
        as.numeric(pROC::roc(response = y, predictor = preds, quiet = TRUE)$auc)
      } else {
        NA_real_
      }
      eps <- 1e-15
      pClip <- pmin(pmax(preds, eps), 1 - eps)
      logLossVal <- -mean(y * log(pClip) + (1 - y) * log(1 - pClip))
      calFit <- tryCatch(
        stats::glm(y ~ stats::qlogis(pClip), family = stats::binomial()),
        error = function(e) NULL
      )
      calIntercept <- if (!is.null(calFit)) unname(stats::coef(calFit)[1]) else NA_real_
      calSlope <- if (!is.null(calFit)) unname(stats::coef(calFit)[2]) else NA_real_
      data.frame(
        client = i,
        auc = aucVal,
        logLoss = logLossVal,
        calibrationIntercept = calIntercept,
        calibrationSlope = calSlope,
        density = mean(abs(w) > threshold),
        n = length(y),
        outcomes = sum(y),
        stringsAsFactors = FALSE
      )
    },
    w = w,
    threshold = threshold
  )
  do.call(rbind, rows)
}

stopCluster <- function(cl) {
  parallel::stopCluster(cl)
}
