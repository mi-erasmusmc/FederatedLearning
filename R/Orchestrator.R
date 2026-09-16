#' Run any registered federated learning algorithm end-to-end
#'
#' @param cl          cluster object
#' @param algorithm    name, e.g. "DualAvg", "FedAvg"
#' @param config       algorithm configuration list
#'   For DualAvg, optional `dualAvgKktTolerance` enables a mean-loss lasso KKT
#'   safeguard on early stopping. `dualAvgKktCheckEvery` (default 100) sets its
#'   interval; the final round is always checked. Checks use existing objective
#'   replies, adding gradient payloads but no communication exchanges.
#'   `dualAvgGapDiagnostics = TRUE` records a numerical primal-dual gap without
#'   changing stopping. `dualAvgGapCheckEvery` (default 100) controls candidate
#'   creation. Dual evaluations arrive in the next existing objective exchange;
#'   the best completed lower bound is valid for later iterates of the same fit.
#'   Requires binary outcomes, positive lambda, and objective monitoring. Adds
#'   summary payloads; row-level residuals remain on workers. The gap can be
#'   conservative, especially when the best completed candidate is old.
#' @param verbose      if TRUE, print optimization progress
#' @return A fitted model list. Duality-gap diagnostics add `primalObjective`,
#'   `dualLowerBound`, `dualityGap`, `dualBoundRound`, `dualGapChecks`, and
#'   `dualGapHistory`. Objectives and gaps use the configured aggregation's
#'   mean-loss scale.
#' @export
fitFederated <- function(cl, algorithm, config, verbose = TRUE) {
  algo <- .getAlgorithm(algorithm)
  if (is.null(algo)) {
    stop(sprintf("Algorithm '%s' is not registered", algorithm))
  }
  kktTolerance <- config[["dualAvgKktTolerance", exact = TRUE]]
  kktEnabled <- !is.null(kktTolerance)
  if (kktEnabled) {
    if (!(algorithm %in% c("DualAvg", "DualAvgCpp", "DualAvgR"))) {
      stop("dualAvgKktTolerance is supported only for DualAvg")
    }
    if (!is.numeric(kktTolerance) || length(kktTolerance) != 1L ||
        !is.finite(kktTolerance) || kktTolerance <= 0) {
      stop("dualAvgKktTolerance must be a positive finite scalar")
    }
    checkEvery <- config$dualAvgKktCheckEvery %||% 100L
    if (!is.numeric(checkEvery) || length(checkEvery) != 1L ||
        !is.finite(checkEvery) || checkEvery < 1 || checkEvery != floor(checkEvery)) {
      stop("dualAvgKktCheckEvery must be a positive integer")
    }
    config$dualAvgKktCheckEvery <- checkEvery
    lambda <- config[["lambda", exact = TRUE]]
    if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda < 0) {
      stop("The DualAvg KKT safeguard requires a finite non-negative lambda")
    }
    if (identical(config$convergenceObjective, "none")) {
      stop("The DualAvg KKT safeguard requires objective monitoring")
    }
  }
  gapEnabled <- config$dualAvgGapDiagnostics %||% FALSE
  if (!is.logical(gapEnabled) || length(gapEnabled) != 1L || is.na(gapEnabled)) {
    stop("dualAvgGapDiagnostics must be TRUE or FALSE")
  }
  if (gapEnabled) {
    if (!(algorithm %in% c("DualAvg", "DualAvgCpp", "DualAvgR"))) {
      stop("Duality-gap diagnostics are supported only for DualAvg")
    }
    gapEvery <- config$dualAvgGapCheckEvery %||% 100L
    if (!is.numeric(gapEvery) || length(gapEvery) != 1L || !is.finite(gapEvery) ||
        gapEvery < 1 || gapEvery != floor(gapEvery)) {
      stop("dualAvgGapCheckEvery must be a positive integer")
    }
    config$dualAvgGapCheckEvery <- gapEvery
    lambda <- config[["lambda", exact = TRUE]]
    if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda <= 0) {
      stop("Duality-gap diagnostics require a positive finite lambda")
    }
    if (identical(config$convergenceObjective, "none")) {
      stop("Duality-gap diagnostics require objective monitoring")
    }
  }
  clientFrac <- config$clientFrac %||% 1
  if (!is.numeric(clientFrac) || length(clientFrac) != 1L ||
      !is.finite(clientFrac) || clientFrac <= 0 || clientFrac > 1) {
    stop("config$clientFrac must be a single finite value in (0, 1]")
  }
  if (clientFrac < 1 && !isTRUE(algo$supportsClientSampling)) {
    stop(sprintf("Algorithm '%s' does not support clientFrac < 1", algorithm))
  }
  config$clientFrac <- clientFrac

  if (!is.null(config$mapping)) {
    globalMap <- config$mapping
  } else {
    mapType <- config$mapType %||% "union"
    globalMap <- clusterCollectCovRefs(
      cl,
      type = mapType,
      featureSet = config$featureSet,
      covariateIds = config$covariateIds,
      analysisIds = config$analysisIds
    )
    config$mapping <- globalMap
  }
  if (is.null(globalMap) || nrow(globalMap) == 0) {
    stop("Global feature map is empty for the requested feature set")
  }
  if (is.null(config[["p"]])) {
    config[["p"]] <- nrow(globalMap)
  }
  clusterCreateMatrices(cl, config)
  parallel::clusterApply(
    cl,
    seq_along(cl),
    function(localId) {
      options(FederatedLearning.localId = localId)
      assign("clientLocalId", localId, envir = .GlobalEnv)
      invisible(NULL)
    }
  )

  if (is.null(config$profile)) {
    config$profile <- FALSE
  } else {
    if (config$profile == TRUE) {
      parallel::clusterEvalQ(
        cl,
        utils::Rprof(
          filename = sprintf("worker-%d.out", Sys.getpid()),
          line.profiling = TRUE
        )
      )
      on.exit(
        parallel::clusterEvalQ(cl, utils::Rprof(NULL)),
        add = TRUE,
        after = FALSE # before stopCluster
      )
    }
  }

  convergenceObjective <- config$convergenceObjective %||% "negLogLikelihood"
  validConvergenceObjectives <- c("negLogLikelihood", "cyclopsGradient", "none")
  if (!is.character(convergenceObjective) || length(convergenceObjective) != 1L ||
      !(convergenceObjective %in% validConvergenceObjectives)) {
    stop(
      "config$convergenceObjective must be one of: ",
      paste(validConvergenceObjectives, collapse = ", ")
    )
  }
  config$convergenceObjective <- convergenceObjective

  clientUpdate <- function(serverBroadcast) {
    .assertWorkerState(
      "clientData",
      action = "Run clusterCreateMatrices() before client updates."
    )
    report <- algo$clientUpdate(
      clientData,
      serverBroadcast,
      config
    )
  }

  dualCache <- new.env(parent = emptyenv())
  getLocalConvergenceObjective <- function(w, checkKkt = FALSE, prepareDual = FALSE,
                                           dualRequest = NULL, round = NULL) {
    .assertWorkerState(
      "clientData",
      action = "Run clusterCreateMatrices() before objective evaluation."
    )
    dualEvaluation <- NULL
    if (!is.null(dualRequest)) {
      if (is.null(dualCache$residual) || !isTRUE(dualCache$round == dualRequest$round)) {
        stop("Missing or stale worker dual candidate")
      }
      dualEvaluation <- logisticDualEntropyCpp(dualCache$residual,
        clientData$yLabels, dualRequest$scales)
      dualEvaluation$round <- dualCache$round
      dualCache$residual <- NULL
    }
    if (isTRUE(checkKkt) || gapEnabled) {
      if (prepareDual && isTRUE(config$intercept) && any(clientData$xMatrix[, 1L] != 1)) {
        stop("Dual diagnostics require a unit-valued first column for the intercept")
      }
      stats <- logisticObjectiveGradientCpp(.asDgCMatrix(clientData$xMatrix),
        w, clientData$yLabels, dualStats = prepareDual,
        computeGradient = isTRUE(checkKkt) || prepareDual)
      if (!is.null(stats$gradient)) stats$gradient <- as.numeric(stats$gradient)
      if (prepareDual) {
        dualCache$residual <- as.numeric(stats$dualResidual)
        dualCache$round <- round
        stats$dualResidual <- NULL
        stats$dualMass <- as.numeric(stats$dualMass)
      }
      stats$objective <- switch(config$convergenceObjective,
        negLogLikelihood = stats$loss,
        cyclopsGradient = stats$cyclopsObjective)
      stats$dualEvaluation <- dualEvaluation
      fields <- c("objective", "loss", "gradient", "n", "dualMass",
        "dualClassGradient", "dualGradientScale", "dualEvaluation")
      return(stats[intersect(fields, names(stats))])
    }
    n <- nrow(clientData$xMatrix)
    objective <- switch(config$convergenceObjective,
      negLogLikelihood = logisticNegLogLik(
        weights = w,
        xMatrix = clientData$xMatrix,
        yLabels = clientData$yLabels,
        meanLoss = FALSE
      ),
      cyclopsGradient = cyclopsGradientObjective(
        weights = w,
        xMatrix = clientData$xMatrix,
        yLabels = clientData$yLabels
      ),
      none = NA_real_
    )
    list(
      objective = objective,
      n = n
    )
  }
  getLocalFitDiagnostics <- function(w) {
    .assertWorkerState(
      "clientData",
      action = "Run clusterCreateMatrices() before fit diagnostics."
    )
    n <- nrow(clientData$xMatrix)
    list(
      loss = logisticNegLogLik(
        weights = w,
        xMatrix = clientData$xMatrix,
        yLabels = clientData$yLabels,
        meanLoss = FALSE
      ),
      gradient = gradLogistic(
        weights = w,
        xMatrix = clientData$xMatrix,
        yLabels = clientData$yLabels
      ),
      n = n
    )
  }
  parallel::clusterExport(
    cl,
    c(
      "algo",
      "config",
      ".assertWorkerState",
      "clientUpdate",
      "getLocalConvergenceObjective",
      "getLocalFitDiagnostics"
    ),
    envir = environment()
  )
  objectiveHelperNames <- c(
    "assertConformableWeights",
    "binaryLogLoss",
    "logisticNegLogLik",
    "gradLogistic",
    "logisticGradientCpp",
    "logisticObjectiveGradientCpp",
    "logisticDualEntropyCpp",
    "cyclopsGradientObjective",
    "cyclopsGradientObjectiveCpp",
    ".asDgCMatrix"
  )
  ns <- getNamespace("FederatedLearning")
  objectiveHelpers <- objectiveHelperNames[objectiveHelperNames %in% ls(envir = ns, all.names = TRUE)]
  if (length(objectiveHelpers) > 0) {
    parallel::clusterExport(cl, objectiveHelpers, envir = ns)
  }
  serverState <- algo$serverInit(config)
  if (!is.null(config$request)) {
    serverState$request <- config$request
    if (!is.null(serverState$mode)) {
      serverState$mode <- config$request
    }
  }
  parallel::clusterExport(cl, "serverState", envir = environment())

  if (!is.null(algo$clientInit)) {
    parallel::clusterEvalQ(
      cl,
      algo$clientInit(
        serverState = serverState,
        config = config
      )
    )
  }

  previousObjective <- NULL
  globalObjective <- NULL
  lastTick <- Sys.time()
  lastRound <- 0L
  serverReport <- list()
  roundOffset <- config$roundOffset %||% 0L
  if (!is.numeric(roundOffset) || length(roundOffset) != 1L ||
      !is.finite(roundOffset) || roundOffset < 0) {
    stop("config$roundOffset must be a single non-negative finite value")
  }
  roundOffset <- as.integer(roundOffset)
  roundsCompleted <- 0L
  kktChecks <- 0L
  kktMaxAbs <- NA_real_
  converged <- FALSE
  pendingDual <- NULL
  dualLowerBound <- 0 # r = 0 is always feasible for binary logistic lasso.
  dualBoundRound <- NA_integer_
  dualGapChecks <- 0L
  dualGapHistory <- list()
  if (!is.null(config$clientSampleSeed)) {
    oldSeed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    set.seed(config$clientSampleSeed)
    on.exit({
      if (is.null(oldSeed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", oldSeed, envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  for (r in 0:(config$rounds - 1)) {
    roundsCompleted <- r + 1L
    activeClients <- selectActiveClients(length(cl), config$clientFrac)
    clActive <- subsetCluster(cl, activeClients)
    serverState$r <- r + roundOffset
    serverState$activeClients <- activeClients
    # update client state
    parallel::clusterExport(clActive, "serverState", envir = environment())
    report <- parallel::clusterEvalQ(
      clActive,
      clientUpdate(serverBroadcast = serverState)
    )
    # step 2: server round
    srv <- algo$serverRound(
      serverState = serverState,
      clientReports = report,
      config = config
    )
    serverState <- srv$state
    serverReport <- srv$report
    serverReport$activeClients <- activeClients
    if (kktEnabled) {
      serverReport$checkKkt <- roundsCompleted %% checkEvery == 0L ||
        roundsCompleted == config$rounds
    }
    if (gapEnabled) {
      serverReport$dualGapRequest <- pendingDual
      serverReport$prepareDualGap <- roundsCompleted < config$rounds &&
        (roundsCompleted == 1L || roundsCompleted %% gapEvery == 0L)
      serverReport$dualGapRound <- roundsCompleted
    }
    parallel::clusterExport(cl, c("serverReport"), envir = environment())
    hasWeights <- !is.null(serverReport$w) &&
      !isTRUE(serverReport$skipConvergence) &&
      !identical(config$convergenceObjective, "none")
    criteria <- NA_real_
    if (hasWeights) {
      localObjectives <- parallel::clusterEvalQ(
        cl,
        getLocalConvergenceObjective(serverReport$w, isTRUE(serverReport$checkKkt),
          isTRUE(serverReport$prepareDualGap), serverReport$dualGapRequest,
          serverReport$dualGapRound)
      )
      lossVec <- vapply(localObjectives, `[[`, numeric(1), "objective")
      globalObjective <- sum(lossVec)
      kktPassed <- !kktEnabled
      if (kktEnabled && isTRUE(serverReport$checkKkt)) {
        gradient <- weightedReportAverage(localObjectives, "gradient",
          aggregation = config$aggregation %||% "sampleSize")
        kktMaxAbs <- max(.lassoKktResidual(serverReport$w, gradient,
          lambda, intercept = isTRUE(config$intercept)))
        kktChecks <- kktChecks + 1L
        kktPassed <- kktMaxAbs <= kktTolerance
        if (verbose) {
          cat(sprintf("KKT round %d: max residual = %.6g (tolerance %.6g)\n",
            roundsCompleted, kktMaxAbs, kktTolerance))
        }
      }
      if (gapEnabled) {
        aggregation <- config$aggregation %||% "sampleSize"
        evaluation <- NULL
        if (!is.null(pendingDual)) {
          evaluation <- .dualGapEvaluation(localObjectives, pendingDual, lambda,
            isTRUE(config$intercept), aggregation)
          dualGapChecks <- dualGapChecks + 1L
          if (evaluation$objective > dualLowerBound) {
            dualLowerBound <- evaluation$objective
            dualBoundRound <- pendingDual$round
          }
        }
        meanLosses <- vapply(localObjectives, function(x) x$loss / x$n, numeric(1))
        penalized <- seq_along(serverReport$w)
        if (isTRUE(config$intercept)) penalized <- penalized[-1L]
        primalObjective <- sum(reportWeights(localObjectives, aggregation) * meanLosses) +
          lambda * sum(abs(serverReport$w[penalized]))
        dualityGap <- .checkedDualityGap(primalObjective, dualLowerBound)
        gapRow <- data.frame(round = roundsCompleted, candidateRound = pendingDual$round %||% NA_integer_,
          primalObjective = primalObjective, candidateDual = evaluation$objective %||% NA_real_,
          dualLowerBound = dualLowerBound, dualBoundRound = dualBoundRound,
          dualityGap = dualityGap, rawGap = primalObjective - dualLowerBound,
          dualBalance = evaluation$balance %||% NA_real_,
          dualGradientMaxAbs = pendingDual$gradientMaxAbs %||% NA_real_,
          kktMaxAbs = if (isTRUE(serverReport$checkKkt)) kktMaxAbs else NA_real_)
        if (!is.null(evaluation) || roundsCompleted == config$rounds) {
          dualGapHistory[[length(dualGapHistory) + 1L]] <- gapRow
          if (verbose) cat(sprintf("Dual gap round %d: %.6g (bound from round %s)\n",
            roundsCompleted, dualityGap, dualBoundRound))
        }
        pendingDual <- if (isTRUE(serverReport$prepareDualGap)) {
          .dualGapRequest(localObjectives, lambda, isTRUE(config$intercept),
            aggregation, roundsCompleted)
        } else NULL
      }

      if (!is.null(previousObjective)) {
        deltaAbs <- globalObjective - previousObjective
        criteria <- deltaAbs / (abs(globalObjective) + 1)
      } else {
        criteria <- Inf
      }

      if (verbose) {
        if (r %% 100 == 0 || r == config$rounds - 1) {
          now <- Sys.time()
          nrounds <- r - lastRound
          if (r > 0 && nrounds > 0) {
            dt <- as.numeric(difftime(now, lastTick, units = "secs"))
            cat(sprintf(
              "Round %3d: obj = %12.6f  crit = %12.2e  dt(%3d) = %8.3fs\n",
              r,
              globalObjective,
              criteria,
              nrounds,
              dt
            ))
          } else {
            cat(sprintf(
              "Round %3d: obj = %12.6f  crit = %12.2e\n",
              r,
              globalObjective,
              criteria
            ))
          }
          lastTick <- now
          lastRound <- r
        }
      }
      previousObjective <- globalObjective
      if (!is.null(config$epsilon) && is.finite(criteria) &&
          abs(criteria) < config$epsilon && kktPassed) {
        converged <- TRUE
        if (verbose) {
          message("Convergence criteria met in round ", r, "\n")
        }
        break
      }
    }
    if (isTRUE(serverReport$done)) {
      break
    }
  }
  if (!is.null(globalObjective)) {
    message("Final objective value: ", globalObjective, "\n")
  }
  result <- list(
    w = if (!is.null(serverReport$w)) serverReport$w else serverState$w,
    config = config,
    roundsCompleted = roundsCompleted
  )
  if (kktEnabled) {
    result$converged <- converged
    result$stopReason <- if (converged) "converged" else "roundLimit"
    result$kktMaxAbs <- kktMaxAbs
    result$kktChecks <- kktChecks
  }
  if (gapEnabled) {
    if (length(dualGapHistory) == 0L || dualGapHistory[[length(dualGapHistory)]]$round != roundsCompleted) {
      dualGapHistory[[length(dualGapHistory) + 1L]] <- gapRow
    }
    result$primalObjective <- primalObjective
    result$dualLowerBound <- dualLowerBound
    result$dualityGap <- dualityGap
    result$dualBoundRound <- dualBoundRound
    result$dualGapChecks <- dualGapChecks
    result$dualGapHistory <- do.call(rbind, dualGapHistory)
  }
  if (!is.null(globalObjective)) {
    result$globalObjective <- globalObjective
  }
  if (!is.null(serverReport$cvMetric)) {
    result$cvMetric <- serverReport$cvMetric
  } else if (!is.null(serverState$cvMetric)) {
    result$cvMetric <- serverState$cvMetric
  }
  extraNames <- setdiff(names(serverReport), c("w", "done", "cvMetric", "skipConvergence", "checkKkt",
    "dualGapRequest", "prepareDualGap", "dualGapRound"))
  for (nm in extraNames) {
    result[[nm]] <- serverReport[[nm]]
  }
  if (isTRUE(config$pooledDiagnostics %||% FALSE)) {
    fitDiagnostics <- if (kktEnabled) localObjectives else parallel::clusterEvalQ(
      cl,
      getLocalFitDiagnostics(serverReport$w %||% serverState$w)
    )
    ns <- vapply(fitDiagnostics, `[[`, numeric(1), "n")
    totalN <- sum(ns)
    grads <- do.call(cbind, lapply(fitDiagnostics, `[[`, "gradient"))
    globalGradient <- as.numeric(grads %*% (ns / totalN))
    lambda <- config[["lambda", exact = TRUE]]
    lambda <- if (length(lambda) == 1L && is.finite(lambda) && lambda >= 0) lambda else NA_real_
    penalize <- rep(TRUE, length(globalGradient))
    if (length(penalize) > 0L && isTRUE(config$intercept)) {
      penalize[1] <- FALSE
    }
    active <- abs(result$w) > 1e-8
    kktViolation <- abs(globalGradient)
    if (is.finite(lambda)) {
      for (j in seq_along(globalGradient)) {
        if (isTRUE(penalize[j])) {
          kktViolation[j] <- if (isTRUE(active[j])) {
            abs(globalGradient[j] + lambda * sign(result$w[j]))
          } else {
            max(abs(globalGradient[j]) - lambda, 0)
          }
        }
      }
    }
    finiteKkt <- is.finite(kktViolation)
    result$pooledNegLogLik <- sum(vapply(fitDiagnostics, `[[`, numeric(1), "loss"))
    result$pooledMeanLogLoss <- result$pooledNegLogLik / totalN
    result$pooledGradientMaxAbs <- max(abs(globalGradient), na.rm = TRUE)
    result$pooledKktMaxAbs <- if (any(finiteKkt)) max(kktViolation[finiteKkt]) else NA_real_
    result$pooledKktViolating <- if (any(finiteKkt)) sum(kktViolation[finiteKkt] > (config$pooledKktTolerance %||% 1e-4)) else NA_integer_
    result$pooledKktMaxCoordinate <- if (any(finiteKkt)) which.max(kktViolation) else NA_integer_
  }
  result
}

selectActiveClients <- function(nClients, clientFrac = 1) {
  if (!is.numeric(clientFrac) || length(clientFrac) != 1L ||
      !is.finite(clientFrac) || clientFrac <= 0 || clientFrac > 1) {
    stop("clientFrac must be a single finite value in (0, 1]")
  }
  if (clientFrac >= 1 || nClients <= 1L) {
    return(seq_len(nClients))
  }
  sample(seq_len(nClients), max(1L, ceiling(clientFrac * nClients)))
}

subsetCluster <- function(cl, idx) {
  sub <- cl[idx]
  if (!is.null(names(cl))) {
    names(sub) <- names(cl)[idx]
  }
  class(sub) <- class(cl)
  attrNames <- setdiff(names(attributes(cl)), c("class", "names"))
  for (nm in attrNames) {
    attr(sub, nm) <- attr(cl, nm)
  }
  sub
}
