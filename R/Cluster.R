#' Initialize a worker cluster for client execution
#' @param clientHosts worker host names or addresses
#' @param clientPaths client data paths; used to define cluster size
#' @param mirai if TRUE, use local `mirai` daemons; otherwise use PSOCK workers.
#'   Both give the same results. PSOCK workers stall on each message of a few
#'   kilobytes or more, which slows round-based methods at thousands of
#'   predictors.
#' @return cluster object
#' @export
clusterInit <- function(clientHosts, clientPaths, mirai = TRUE) {
  stopifnot(length(clientHosts) == length(clientPaths))
  if (mirai) {
    # A mirai cluster's nodes share its daemons, so successive calls to a node
    # can reach different processes. Workers hold their site's data between
    # calls, so each node gets a profile with a single daemon of its own.
    profiles <- lapply(seq_along(clientHosts), function(i) mirai::make_cluster(1L))
    cl <- structure(lapply(profiles, `[[`, 1L), class = c("pinnedMiraiCluster", "cluster"),
      profiles = profiles)
    libraries <- .libPaths()
    tryCatch(parallel::clusterCall(cl, function(paths) {
      .libPaths(paths)
      NULL
    }, libraries), error = function(error) {
      parallel::stopCluster(cl)
      stop(error)
    })
  } else {
    cl <- parallelly::makeClusterPSOCK(
      workers = clientHosts,
      rscript_libs = .libPaths(),
      default_packages = c("datasets", "utils", "grDevices", "graphics", "stats", "methods")
    )
  }
  # Startup packages load before rscript_libs, so load study packages afterwards.
  tryCatch(parallel::clusterCall(cl, function(packages) {
    for (package in packages) {
      suppressPackageStartupMessages(library(package, character.only = TRUE))
    }
    NULL
  }, c("PatientLevelPrediction", "FederatedLearning")), error = function(error) {
    parallel::stopCluster(cl)
    stop(error)
  })
  cl
}

#' @exportS3Method parallel::stopCluster
stopCluster.pinnedMiraiCluster <- function(cl = NULL) {
  for (profile in attr(cl, "profiles")) {
    parallel::stopCluster(profile)
  }
  invisible(NULL)
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
  "getLocalObjective",
  "getLocalConvergenceObjective"
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

# Calibration against observed outcomes, from the linear predictor so extreme
# risks are not distorted by probability clipping. Calibration-in-the-large is
# the intercept of a logistic model with the linear predictor as an offset
# (slope fixed at one); the slope and its intercept come from a model with both
# free.
.calibrationMetrics <- function(y, linearPredictor) {
  out <- c(calibrationInTheLarge = NA_real_, calibrationIntercept = NA_real_, calibrationSlope = NA_real_)
  if (length(unique(y)) != 2L || any(!is.finite(linearPredictor))) return(out)
  fit <- function(formula) {
    suppressWarnings(tryCatch(stats::glm(formula, family = stats::binomial()), error = function(e) NULL))
  }
  large <- fit(y ~ 1 + stats::offset(linearPredictor))
  free <- fit(y ~ linearPredictor)
  if (!is.null(large)) out[["calibrationInTheLarge"]] <- unname(stats::coef(large)[[1]])
  if (!is.null(free)) out[c("calibrationIntercept", "calibrationSlope")] <- unname(stats::coef(free)[1:2])
  out
}

# Smooth calibration curve: a binomial GAM of the outcome on a cubic regression
# spline of the linear predictor, the estimator chosen for the PandemicPrediction
# study. The curve is evaluated at percentiles of the predictions with pointwise
# 95% intervals, so it describes the data without patient-level predictions.
# The integrated calibration index (ICI) is the mean absolute difference between
# predicted and smoothed observed risk over all patients.
.calibrationCurve <- function(y, linearPredictor, points = 50L) {
  none <- list(curve = NULL, ici = NA_real_)
  events <- sum(y == 1)
  distinct <- length(unique(linearPredictor))
  if (length(y) < 20L || events == 0L || events == length(y) || distinct < 5L ||
      any(!is.finite(linearPredictor))) {
    return(none)
  }
  k <- min(max(12L, min(24L, ceiling(6 * log10(max(20, events))))), distinct - 1L)
  data <- data.frame(y = as.numeric(y == 1), lp = linearPredictor)
  fit <- tryCatch(suppressWarnings(mgcv::bam(y ~ s(lp, bs = "cr", k = k), data = data,
    family = stats::binomial(), method = "fREML", discrete = TRUE, select = TRUE, gamma = 1.2)),
    error = function(e) NULL)
  if (is.null(fit)) return(none)
  grid <- unique(stats::quantile(linearPredictor, seq(0.01, 0.99, length.out = points), names = FALSE))
  smooth <- stats::predict(fit, newdata = data.frame(lp = grid), se.fit = TRUE)
  observed <- as.numeric(stats::fitted(fit))
  list(
    curve = data.frame(
      predicted = stats::plogis(grid),
      observed = stats::plogis(as.numeric(smooth$fit)),
      lower = stats::plogis(as.numeric(smooth$fit - 1.96 * smooth$se.fit)),
      upper = stats::plogis(as.numeric(smooth$fit + 1.96 * smooth$se.fit))
    ),
    ici = mean(abs(stats::plogis(linearPredictor) - observed))
  )
}

.evaluateBinaryMetrics <- function(y, preds, w, threshold = 1e-4, linearPredictor = NULL) {
  aucVal <- if (length(unique(y)) == 2) {
    as.numeric(pROC::roc(response = y, predictor = preds,
      levels = c(0, 1), direction = "<", quiet = TRUE)$auc)
  } else {
    NA_real_
  }
  # The linear predictor gives log loss without probability clipping.
  logLossVal <- if (is.null(linearPredictor)) logLoss(y, preds) else binaryLogLoss(linearPredictor, y)
  # Without a linear predictor, probabilities are bounded away from 0 and 1.
  eps <- 1e-15
  lp <- linearPredictor %||% stats::qlogis(pmin(pmax(preds, eps), 1 - eps))
  calibration <- .calibrationMetrics(y, lp)
  curve <- .calibrationCurve(y, lp)
  out <- data.frame(
    auc = aucVal,
    logLoss = logLossVal,
    calibrationIci = curve$ici,
    calibrationInTheLarge = calibration[["calibrationInTheLarge"]],
    calibrationIntercept = calibration[["calibrationIntercept"]],
    calibrationSlope = calibration[["calibrationSlope"]],
    density = mean(abs(w) > threshold),
    n = length(y),
    outcomes = sum(y),
    stringsAsFactors = FALSE
  )
  attr(out, "calibrationCurve") <- curve$curve
  out
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
        matrixRows <- nrow(clientData$xMatrix)
        matrixOutcomes <- sum(clientData$yLabels)
        matrixOutcomeRate <- mean(clientData$yLabels)
        matrixNonzero <- if (inherits(clientData$xMatrix, "sparseMatrix")) {
          length(clientData$xMatrix@x)
        } else {
          sum(clientData$xMatrix != 0)
        }
        matrixDensity <- matrixNonzero / (nrow(clientData$xMatrix) * ncol(clientData$xMatrix))
      } else if (!is.null(config$mapping)) {
        pLocal <- nrow(config$mapping) + as.integer(isTRUE(config$intercept))
        matrixRows <- NA_integer_
        matrixOutcomes <- NA_integer_
        matrixOutcomeRate <- NA_real_
        matrixNonzero <- NA_integer_
        matrixDensity <- NA_real_
      } else {
        pLocal <- NA_integer_
        matrixRows <- NA_integer_
        matrixOutcomes <- NA_integer_
        matrixOutcomeRate <- NA_real_
        matrixNonzero <- NA_integer_
        matrixDensity <- NA_real_
      }
      data.frame(
        client = i,
        n = nrow(pop),
        outcomes = sum(as.integer(pop$outcomeCount) > 0),
        outcomeRate = mean(as.integer(pop$outcomeCount) > 0),
        covariatesAvailable = nrow(covRef),
        covariatesSelected = nrow(filtered),
        matrixColumns = pLocal,
        matrixRows = matrixRows,
        matrixOutcomes = matrixOutcomes,
        matrixOutcomeRate = matrixOutcomeRate,
        matrixNonzero = matrixNonzero,
        matrixDensity = matrixDensity,
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
    c(".assertWorkerState", ".evaluateBinaryMetrics", ".calibrationMetrics", ".calibrationCurve",
      "logLoss", "binaryLogLoss", "%||%"),
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
      metrics <- .evaluateBinaryMetrics(y, preds, w, threshold = threshold, linearPredictor = lin)
      # cbind() drops attributes, so the calibration curve is carried over.
      out <- cbind(data.frame(client = i), metrics)
      attr(out, "calibrationCurve") <- attr(metrics, "calibrationCurve")
      out
    },
    w = w,
    threshold = threshold
  )
  curves <- do.call(rbind, lapply(seq_along(rows), function(i) {
    curve <- attr(rows[[i]], "calibrationCurve")
    if (is.null(curve)) NULL else cbind(client = i, curve)
  }))
  out <- do.call(rbind, rows)
  attr(out, "calibrationCurve") <- curves
  out
}

stopCluster <- function(cl) {
  parallel::stopCluster(cl)
}
