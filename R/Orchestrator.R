#' Run any registered federated learning algorithm end-to-end
#'
#' @param cl          cluster object
#' @param algorithm    name, e.g. "DualAvg", "FedAvg"
#' @param config       algorithm configuration list
#' @param verbose      if TRUE, print optimization progress
#' @return w
#' @export
fitFederated <- function(cl, algorithm, config, verbose = TRUE) {
  algo <- .getAlgorithm(algorithm)
  if (is.null(algo)) {
    stop(sprintf("Algorithm '%s' is not registered", algorithm))
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
  if (is.null(config$p)) {
    config$p <- nrow(globalMap)
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

  getLocalObjective <- function(w) {
    .assertWorkerState(
      "clientData",
      action = "Run clusterCreateMatrices() before objective evaluation."
    )
    n <- nrow(clientData$xMatrix)
    list(
      objective = logisticNegLogLik(
        weights = w,
        xMatrix = clientData$xMatrix,
        yLabels = clientData$yLabels,
        meanLoss = FALSE
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
      "getLocalObjective"
    ),
    envir = environment()
  )
  if (identical(algorithm, "ADAP2")) {
    helperNames <- c(".leadOptimizeSurrogate", ".leadSurrogateCV", ".leadLambdaRange")
    ns <- getNamespace("FederatedLearning")
    available <- helperNames[helperNames %in% ls(envir = ns, all.names = TRUE)]
    if (length(available) > 0) {
      parallel::clusterExport(cl, available, envir = ns)
    }
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
    activeClients <- selectActiveClients(length(cl), config$clientFrac)
    clActive <- subsetCluster(cl, activeClients)
    serverState$r <- r
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
    parallel::clusterExport(cl, c("serverReport"), envir = environment())
    hasWeights <- !is.null(serverReport$w) && !isTRUE(serverReport$skipConvergence)
    criteria <- NA_real_
    if (hasWeights) {
      localObjectives <- parallel::clusterEvalQ(
        cl,
        getLocalObjective(serverReport$w)
      )
      lossVec <- vapply(localObjectives, `[[`, numeric(1), "objective")
      globalObjective <- sum(lossVec)

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
      if (!is.null(config$epsilon) && is.finite(criteria) && abs(criteria) < config$epsilon) {
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
    config = config
  )
  if (!is.null(globalObjective)) {
    result$globalObjective <- globalObjective
  }
  if (!is.null(serverReport$cvMetric)) {
    result$cvMetric <- serverReport$cvMetric
  } else if (!is.null(serverState$cvMetric)) {
    result$cvMetric <- serverState$cvMetric
  }
  extraNames <- setdiff(names(serverReport), c("w", "done", "cvMetric", "skipConvergence"))
  for (nm in extraNames) {
    result[[nm]] <- serverReport[[nm]]
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
