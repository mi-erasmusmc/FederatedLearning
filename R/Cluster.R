#' Initialize a worker cluster for client execution
#' @param clientHosts worker host names or addresses
#' @param clientPaths client data paths; used to define cluster size
#' @param mirai if TRUE, use `mirai`; otherwise use PSOCK workers
#' @return cluster object
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

.workerStateNames <- c(
  "plpData",
  "clientData",
  "clientState",
  "clientLocalId",
  "serverState",
  "serverReport",
  "modelW",
  "algo",
  "config",
  ".assertWorkerState",
  ".evaluateBinaryMetrics",
  "logLoss",
  "clientUpdate",
  "getLocalObjective"
)

.assertWorkerState <- function(..., action = NULL) {
  required <- c(...)
  missing <- required[!vapply(
    required,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
  )]
  if (length(missing) > 0) {
    msg <- paste0(
      "Worker state is missing required object(s): ",
      paste(missing, collapse = ", ")
    )
    if (!is.null(action)) {
      msg <- paste(msg, action)
    }
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Clear FederatedLearning state from cluster workers
#' @param cl cluster object
#' @return invisibly, one NULL per worker
#' @export
clusterClearState <- function(cl) {
  invisible(parallel::clusterCall(
    cl,
    function(stateNames) {
      existing <- intersect(stateNames, ls(envir = .GlobalEnv, all.names = TRUE))
      if (length(existing) > 0) {
        rm(list = existing, envir = .GlobalEnv)
      }
      options(FederatedLearning.localId = NULL)
      NULL
    },
    stateNames = .workerStateNames
  ))
}

#' Load PLP data on each cluster worker
#' @param cl cluster object
#' @param clientPaths paths to client PLP data folders
#' @param popSettings PatientLevelPrediction population settings
#' @return integer vector of per-client population sizes
#' @export
clusterLoadData <- function(cl, clientPaths, popSettings) {
  popSizes <- parallel::clusterApply(
    cl,
    seq_along(clientPaths),
    function(i, clientPaths, popSettings, stateNames) {
      existing <- intersect(
        stateNames,
        ls(envir = .GlobalEnv, all.names = TRUE)
      )
      if (length(existing) > 0) {
        rm(list = existing, envir = .GlobalEnv)
      }
      options(FederatedLearning.localId = NULL)
      plpData <- FederatedLearning::loadClientData(
        clientPaths[i],
        popSettings = popSettings
      )
      assign("plpData", plpData, envir = .GlobalEnv)
      nrow(plpData$population)
    },
    clientPaths = clientPaths,
    popSettings = popSettings,
    stateNames = .workerStateNames
  )
  popSizes
}

#' Collect client covariate references and build a global map
#' @param cl cluster object
#' @param type "union" or "intersection"
#' @param featureSet optional named feature set: "all", "ageSex", "phenotypes",
#'   or "ageSexPhenotypes"
#' @param covariateIds optional explicit covariate ids to retain
#' @param analysisIds optional explicit analysis ids to retain
#' @return data.frame with covariateId and columnId
#' @export
clusterCollectCovRefs <- function(cl,
                                  type = "union",
                                  featureSet = NULL,
                                  covariateIds = NULL,
                                  analysisIds = NULL) {
  parallel::clusterExport(cl, ".assertWorkerState", envir = environment())
  covRefList <- parallel::clusterEvalQ(
    cl,
    {
      .assertWorkerState(
        "plpData",
        action = "Run clusterLoadData() before collecting covariates."
      )
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

#' Create local model matrices on each cluster worker
#' @param cl cluster object
#' @param config list containing `mapping` and model matrix options
#' @return list of NULL values, one per worker
#' @export
clusterCreateMatrices <- function(cl, config) {
  parallel::clusterExport(cl, ".assertWorkerState", envir = environment())
  parallel::clusterCall(
    cl,
    function(mapping, config) {
      if (exists("clientData", envir = .GlobalEnv, inherits = FALSE)) {
        rm("clientData", envir = .GlobalEnv)
      }
      .assertWorkerState(
        "plpData",
        action = "Run clusterLoadData() before creating client matrices."
      )
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

.evaluateBinaryMetrics <- function(y, preds, w, threshold = 1e-4) {
  aucVal <- if (length(unique(y)) == 2) {
    as.numeric(pROC::roc(response = y, predictor = preds, quiet = TRUE)$auc)
  } else {
    NA_real_
  }
  logLossVal <- logLoss(y, preds)
  eps <- 1e-15
  pClip <- pmin(pmax(preds, eps), 1 - eps)
  calFit <- if (length(unique(y)) == 2) {
    suppressWarnings(
      tryCatch(
        stats::glm(y ~ stats::qlogis(pClip), family = stats::binomial()),
        error = function(e) NULL
      )
    )
  } else {
    NULL
  }
  calIntercept <- if (!is.null(calFit)) unname(stats::coef(calFit)[[1]]) else NA_real_
  calSlope <- if (!is.null(calFit)) unname(stats::coef(calFit)[[2]]) else NA_real_
  data.frame(
    auc = aucVal,
    logLoss = logLossVal,
    calibrationIntercept = calIntercept,
    calibrationSlope = calSlope,
    density = mean(abs(w) > threshold),
    n = length(y),
    outcomes = sum(y),
    stringsAsFactors = FALSE
  )
}

clusterPredict <- function(cl, w) {
  parallel::clusterExport(
    cl,
    c(".assertWorkerState", ".evaluateBinaryMetrics", "logLoss"),
    envir = asNamespace("FederatedLearning")
  )
  metrics <- parallel::clusterCall(
    cl,
    function(w) {
      .assertWorkerState(
        "clientData",
        action = "Run fitFederated() or clusterCreateMatrices() before prediction."
      )
      if (ncol(clientData$xMatrix) != length(w)) {
        stop(
          "clusterPredict dimension mismatch: xMatrix has ",
          ncol(clientData$xMatrix), " columns but weights has length ",
          length(w),
          call. = FALSE
        )
      }
      preds <- stats::plogis(as.numeric(clientData$xMatrix %*% w))
      .evaluateBinaryMetrics(clientData$yLabels, preds, w)$auc
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
  parallel::clusterExport(cl, ".assertWorkerState", envir = environment())
  rows <- parallel::clusterApply(
    cl,
    seq_along(cl),
    function(i, config) {
      .assertWorkerState(
        "plpData",
        action = "Run clusterLoadData() before diagnostics."
      )
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
  parallel::clusterExport(
    cl,
    c(".assertWorkerState", ".evaluateBinaryMetrics", "logLoss"),
    envir = asNamespace("FederatedLearning")
  )
  rows <- parallel::clusterApply(
    cl,
    seq_along(cl),
    function(i, w, threshold) {
      .assertWorkerState(
        "clientData",
        action = "Run fitFederated() or clusterCreateMatrices() before evaluation."
      )
      if (ncol(clientData$xMatrix) != length(w)) {
        stop(
          "clusterEvaluateModel dimension mismatch: xMatrix has ",
          ncol(clientData$xMatrix), " columns but weights has length ",
          length(w),
          call. = FALSE
        )
      }
      lin <- as.numeric(clientData$xMatrix %*% w)
      preds <- stats::plogis(lin)
      y <- clientData$yLabels
      cbind(
        data.frame(client = i),
        .evaluateBinaryMetrics(y, preds, w, threshold = threshold)
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
