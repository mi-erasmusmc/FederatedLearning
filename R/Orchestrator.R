#' Run *any* registered FL algorithm end‐to‐end
#'
#' @param cl          cluster object
#' @param algorithm    name, e.g. "DualAvg", "FedAvg"
#' @param config       list(etaClient, etaServer, K, rounds, lambda, clientFrac)
#' @return w
#' @export
fitFederated <- function(cl,
                         algorithm,
                         config,
                         verbose = TRUE) {
  algo <- .getAlgorithm(algorithm)

  globalMap <- clusterCollectCovRefs(cl, type = config$mapType)
  config$p <- nrow(globalMap)
  config$mapping <- globalMap
  clusterCreateMatrices(cl, config)

  if (is.null(config$profile)) {
    config$profile <- FALSE
  } else {
    if (config$profile == TRUE) {
      parallel::clusterEvalQ(cl, utils::Rprof(
        filename = sprintf("worker-%d.out", Sys.getpid()),
        line.profiling = TRUE
      ))
      on.exit(
        parallel::clusterEvalQ(cl, utils::Rprof(NULL)),
        add = TRUE,
        after = FALSE # before stopCluster
      )
    }
  }

  clientUpdate <- function(serverBroadcast) {
    report <- algo$clientUpdate(
      clientData,
      serverBroadcast,
      config
    )
  }

  getLocalObjective <- function(w) {
    n <- nrow(clientData$xMatrix)
    linearPred <- as.numeric(clientData$xMatrix %*% w)
    loss <- sum(clientData$yLabels * linearPred - log1p(exp(linearPred)))
    list(
      objective = loss,
      n = n
    )
  }
  parallel::clusterExport(cl, c(
    "algo", "config",
    "clientUpdate", "getLocalObjective"
  ),
  envir = environment()
  )
  serverState <- algo$serverInit(config)
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

  previousObjective <- -Inf

  for (r in 0:(config$rounds - 1)) {
    serverState$r <- r
    # update client state
    parallel::clusterExport(cl, "serverState", envir = environment())
    report <- parallel::clusterEvalQ(cl, clientUpdate(serverBroadcast = serverState))
    # step 2: server round
    srv <- algo$serverRound(
      serverState = serverState,
      clientReports = report,
      config = config
    )
    serverState <- srv$state
    serverReport <- srv$report
    parallel::clusterExport(cl, c("serverReport"), envir = environment())
    localObjectives <- parallel::clusterEvalQ(cl, getLocalObjective(serverReport$w))
    lossVec <- vapply(localObjectives, `[[`, numeric(1), "objective")
    globalLoss <- sum(lossVec)
    globalObjective <- globalLoss

    deltaAbs <- globalObjective - previousObjective
    criteria <- deltaAbs / (abs(globalObjective) + 1)
    if (verbose) {
      # only every 100 rounds
      if (r %% 100 == 0 || r == config$rounds - 1) {
        cat(sprintf(
          "Round %3d: obj = %12.6f  crit = %12.2e\n",
          r, globalObjective, criteria
        ))
      }
    }
    previousObjective <- globalObjective
    if (abs(criteria) < config$epsilon) {
      if (verbose) {
        message("Convergence criteria met in round ", r, "\n")
      }
      break
    }
  }
  message("Final objective value: ", globalObjective, "\n")
  list(w = serverReport$w, globalObjective = globalObjective, config = config)
}
