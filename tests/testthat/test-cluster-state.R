library(FederatedLearning)

test_that("clusterClearState removes package worker globals", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterEvalQ(cl, {
    plpData <- "loaded"
    clientData <- "matrix"
    clientState <- "state"
    clientLocalId <- 1L
    serverState <- "server"
    serverReport <- "report"
    modelW <- "model"
    algo <- "algorithm"
    config <- "config"
    .assertWorkerState <- "assert"
    .evaluateBinaryMetrics <- "metrics"
    logLoss <- "logLoss"
    clientUpdate <- "clientUpdate"
    getLocalObjective <- "objective"
    getLocalConvergenceObjective <- "convergenceObjective"
    assign("plpData", plpData, envir = .GlobalEnv)
    assign("clientData", clientData, envir = .GlobalEnv)
    assign("clientState", clientState, envir = .GlobalEnv)
    assign("clientLocalId", clientLocalId, envir = .GlobalEnv)
    assign("serverState", serverState, envir = .GlobalEnv)
    assign("serverReport", serverReport, envir = .GlobalEnv)
    assign("modelW", modelW, envir = .GlobalEnv)
    assign("algo", algo, envir = .GlobalEnv)
    assign("config", config, envir = .GlobalEnv)
    assign(".assertWorkerState", .assertWorkerState, envir = .GlobalEnv)
    assign(".evaluateBinaryMetrics", .evaluateBinaryMetrics, envir = .GlobalEnv)
    assign("logLoss", logLoss, envir = .GlobalEnv)
    assign("clientUpdate", clientUpdate, envir = .GlobalEnv)
    assign("getLocalObjective", getLocalObjective, envir = .GlobalEnv)
    assign("getLocalConvergenceObjective", getLocalConvergenceObjective, envir = .GlobalEnv)
    options(FederatedLearning.localId = 1L)
    NULL
  })

  clusterClearState(cl)

  present <- parallel::clusterEvalQ(
    cl,
    vapply(
      c(
        "plpData",
        "clientData",
        "clientState",
        "clientLocalId",
        "serverState",
        "serverReport",
        "modelW"
      ),
      exists,
      logical(1),
      envir = .GlobalEnv,
      inherits = FALSE
    )
  )[[1]]
  expect_false(any(present))
  morePresent <- parallel::clusterEvalQ(
    cl,
    vapply(
      c(
        "algo",
        "config",
        ".assertWorkerState",
        ".evaluateBinaryMetrics",
        "logLoss",
        "clientUpdate",
        "getLocalObjective",
        "getLocalConvergenceObjective"
      ),
      exists,
      logical(1),
      envir = .GlobalEnv,
      inherits = FALSE
    )
  )[[1]]
  expect_false(any(morePresent))
  expect_null(parallel::clusterEvalQ(cl, getOption("FederatedLearning.localId"))[[1]])
})

test_that("evaluation and prediction fail clearly when clientData is missing", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  expect_error(
    clusterEvaluateModel(cl, w = c(0, 1)),
    "Worker state is missing required object\\(s\\): clientData"
  )
  expect_error(
    FederatedLearning:::clusterPredict(cl, w = c(0, 1)),
    "Worker state is missing required object\\(s\\): clientData"
  )
})

test_that("diagnostics and feature collection fail clearly when plpData is missing", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  expect_error(
    clusterDiagnostics(cl),
    "Worker state is missing required object\\(s\\): plpData"
  )
  expect_error(
    clusterCollectCovRefs(cl),
    "Worker state is missing required object\\(s\\): plpData"
  )
})

test_that("matrix creation clears stale clientData before missing plpData errors", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterEvalQ(cl, {
    clientData <- list(stale = TRUE)
    assign("clientData", clientData, envir = .GlobalEnv)
    NULL
  })

  expect_error(
    clusterCreateMatrices(
      cl,
      config = list(
        mapping = data.frame(covariateId = 1, columnId = 1),
        intercept = TRUE
      )
    ),
    "Worker state is missing required object\\(s\\): plpData"
  )

  hasClientData <- parallel::clusterEvalQ(
    cl,
    exists("clientData", envir = .GlobalEnv, inherits = FALSE)
  )[[1]]
  expect_false(hasClientData)
})
