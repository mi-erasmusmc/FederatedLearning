test_that("client selection honors clientFrac bounds and sampling size", {
  expect_equal(FederatedLearning:::selectActiveClients(5, 1), 1:5)
  expect_equal(length(FederatedLearning:::selectActiveClients(5, 0.4)), 2L)
  expect_equal(length(FederatedLearning:::selectActiveClients(5, 0.01)), 1L)
  expect_error(FederatedLearning:::selectActiveClients(5, 0), "clientFrac")
  expect_error(FederatedLearning:::selectActiveClients(5, 1.1), "clientFrac")
})

test_that("client selection is reproducible under an explicit seed", {
  set.seed(1001)
  first <- FederatedLearning:::selectActiveClients(10, 0.3)
  set.seed(1001)
  second <- FederatedLearning:::selectActiveClients(10, 0.3)

  expect_equal(first, second)
  expect_equal(length(first), 3L)
})

test_that("report weights distinguish sample-size and equal-client aggregation", {
  reports <- list(
    list(delta = c(1, 0), n = 10),
    list(delta = c(3, 4), n = 30)
  )

  expect_equal(
    FederatedLearning:::reportWeights(reports, aggregation = "sampleSize"),
    c(0.25, 0.75)
  )
  expect_equal(
    FederatedLearning:::reportWeights(reports, aggregation = "equalClient"),
    c(0.5, 0.5)
  )
  expect_equal(
    FederatedLearning:::weightedReportAverage(reports, "delta", aggregation = "sampleSize"),
    c(2.5, 3)
  )
  expect_equal(
    FederatedLearning:::weightedReportAverage(reports, "delta", aggregation = "equalClient"),
    c(2, 2)
  )
})

test_that("sample-size aggregation requires client reports to include n", {
  reports <- list(list(delta = 1), list(delta = 2, n = 10))

  expect_error(
    FederatedLearning:::reportWeights(reports, aggregation = "sampleSize"),
    "requires each client report"
  )
  expect_equal(
    FederatedLearning:::reportWeights(reports, aggregation = "equalClient"),
    c(0.5, 0.5)
  )
})

test_that("DualAvg server aggregation can be sample-size weighted or equal-client", {
  state <- list(z = c(0, 0), r = 0L)
  reports <- list(
    list(delta = c(1, 0), n = 10),
    list(delta = c(3, 4), n = 30)
  )
  baseConfig <- list(
    etaServer = 1,
    etaClient = 1,
    k = 1L,
    lambda = 0,
    intercept = FALSE
  )

  sampleSize <- serverRoundDA(
    state,
    reports,
    modifyList(baseConfig, list(aggregation = "sampleSize"))
  )
  equalClient <- serverRoundDA(
    state,
    reports,
    modifyList(baseConfig, list(aggregation = "equalClient"))
  )

  expect_equal(sampleSize$state$z, c(2.5, 3))
  expect_equal(sampleSize$report$w, c(2.5, 3))
  expect_equal(equalClient$state$z, c(2, 2))
  expect_equal(equalClient$report$w, c(2, 2))
})

test_that("DualAvgCpp server aggregation matches R DualAvg weighting", {
  state <- list(z = c(0, 0), r = 0L)
  reports <- list(
    list(delta = c(1, 0), n = 10),
    list(delta = c(3, 4), n = 30)
  )
  baseConfig <- list(
    etaServer = 1,
    etaClient = 1,
    k = 1L,
    lambda = 0,
    intercept = FALSE
  )

  for (aggregation in c("sampleSize", "equalClient")) {
    config <- modifyList(baseConfig, list(aggregation = aggregation))
    expect_equal(
      serverRoundDualAveragingCpp(state, reports, config),
      serverRoundDA(state, reports, config)
    )
  }
  expect_error(
    serverRoundDualAveragingCpp(
      state,
      list(list(delta = c(1, 0)), list(delta = c(3, 4), n = 30)),
      modifyList(baseConfig, list(aggregation = "sampleSize"))
    ),
    "requires each client report"
  )
})

test_that("DualAvg server init accepts explicit warm-start dual state", {
  config <- list(p = 2L, intercept = TRUE, initialZ = c(1, -2, 3))

  expect_equal(serverInitDA(config)$z, config$initialZ)
  expect_equal(serverInitDualAveragingCpp(config)$z, config$initialZ)
  expect_error(serverInitDA(modifyList(config, list(initialZ = 1:2))), "initialZ length")
  expect_error(serverInitDualAveragingCpp(modifyList(config, list(initialZ = 1:2))), "initialZ length")
})

test_that("DualAvgCpp supports partial client participation", {
  expect_true(FederatedLearning:::.getAlgorithm("DualAvgCpp")$supportsClientSampling)
})

test_that("DualAvg defaults to C++ implementation and keeps R reference available", {
  dualAvg <- FederatedLearning:::.getAlgorithm("DualAvg")
  dualAvgCpp <- FederatedLearning:::.getAlgorithm("DualAvgCpp")
  dualAvgR <- FederatedLearning:::.getAlgorithm("DualAvgR")

  expect_identical(dualAvg$serverInit, dualAvgCpp$serverInit)
  expect_identical(dualAvg$clientUpdate, dualAvgCpp$clientUpdate)
  expect_identical(dualAvg$serverRound, dualAvgCpp$serverRound)
  expect_true(dualAvg$supportsClientSampling)
  expect_false(is.null(dualAvgR))
  expect_true(is.function(dualAvgR$serverRound))
  expect_identical(formals(dualAvgR$serverRound), formals(serverRoundDA))
  state <- list(z = c(0.1, -0.2), r = 1L)
  reports <- list(list(delta = c(0.3, -0.1), n = 2), list(delta = c(-0.1, 0.2), n = 3))
  config <- list(etaServer = 1, etaClient = 0.5, k = 2L, lambda = 0.01)
  expect_equal(
    dualAvgR$serverRound(state, reports, config),
    serverRoundDA(state, reports, config)
  )
})

test_that("fitFederated rejects unknown algorithms clearly", {
  expect_error(
    fitFederated(
      cl = list(1),
      algorithm = "NotARegisteredAlgorithm",
      config = list(mapping = data.frame(covariateId = 1, columnId = 1)),
      verbose = FALSE
    ),
    "not registered"
  )
})

test_that("fitFederated sends client updates only to selected active workers", {
  skip_on_cran()
  testthat::local_mocked_bindings(
    clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning"
  )

  ns <- asNamespace("FederatedLearning")
  registryLocked <- bindingIsLocked(".algRegistry", ns)
  if (registryLocked) {
    unlockBinding(".algRegistry", ns)
  }
  oldRegistry <- get(".algRegistry", envir = ns)
  on.exit({
    if (bindingIsLocked(".algRegistry", ns)) {
      unlockBinding(".algRegistry", ns)
    }
    assign(".algRegistry", oldRegistry, envir = ns)
    if (registryLocked) {
      lockBinding(".algRegistry", ns)
    }
  }, add = TRUE)
  FederatedLearning:::.registerAlgorithm(
    "SamplingTestAlgorithm",
    serverInit = function(config) list(w = numeric(0)),
    clientInit = NULL,
    clientUpdate = function(clientData, serverBroadcast, config) {
      list(id = getOption("FederatedLearning.localId"), n = clientData$n)
    },
    serverRound = function(serverState, clientReports, config) {
      list(
        state = serverState,
        report = list(
          skipConvergence = TRUE,
          ids = vapply(clientReports, `[[`, integer(1), "id")
        )
      )
    },
    supportsClientSampling = TRUE
  )

  cl <- parallel::makeCluster(4)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(
    cl,
    seq_along(cl),
    function(i) {
      clientData <<- list(n = i)
      NULL
    }
  )

  config <- list(
    mapping = data.frame(covariateId = 1, columnId = 1),
    rounds = 1L,
    clientFrac = 0.5,
    clientSampleSeed = 4001
  )
  set.seed(config$clientSampleSeed)
  expectedActive <- FederatedLearning:::selectActiveClients(length(cl), config$clientFrac)

  fit <- fitFederated(
    cl = cl,
    algorithm = "SamplingTestAlgorithm",
    config = config,
    verbose = FALSE
  )

  expect_equal(sort(fit$ids), sort(expectedActive))
  expect_equal(length(fit$ids), 2L)
})

test_that("fitFederated can monitor Cyclops-style gradient objective", {
  skip_on_cran()
  testthat::local_mocked_bindings(
    clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning"
  )

  ns <- asNamespace("FederatedLearning")
  registryLocked <- bindingIsLocked(".algRegistry", ns)
  if (registryLocked) {
    unlockBinding(".algRegistry", ns)
  }
  oldRegistry <- get(".algRegistry", envir = ns)
  on.exit({
    if (bindingIsLocked(".algRegistry", ns)) {
      unlockBinding(".algRegistry", ns)
    }
    assign(".algRegistry", oldRegistry, envir = ns)
    if (registryLocked) {
      lockBinding(".algRegistry", ns)
    }
  }, add = TRUE)
  FederatedLearning:::.registerAlgorithm(
    "CyclopsConvergenceObjectiveTest",
    serverInit = function(config) list(),
    clientInit = NULL,
    clientUpdate = function(clientData, serverBroadcast, config) list(n = clientData$n),
    serverRound = function(serverState, clientReports, config) {
      list(state = serverState, report = list(w = c(0.5, 1.0)))
    },
    supportsClientSampling = TRUE
  )

  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    library(FederatedLearning)
    clientData <- list(
      xMatrix = matrix(
        c(
          1, -1,
          1, 0,
          1, 2
        ),
        ncol = 2,
        byrow = TRUE
      ),
      yLabels = c(0, 1, 1),
      n = 3
    )
    assign("clientData", clientData, envir = .GlobalEnv)
    NULL
  })

  fit <- fitFederated(
    cl = cl,
    algorithm = "CyclopsConvergenceObjectiveTest",
    config = list(
      mapping = data.frame(covariateId = 1:2, columnId = 1:2),
      rounds = 1L,
      clientFrac = 1,
      convergenceObjective = "cyclopsGradient"
    ),
    verbose = FALSE
  )

  expect_equal(fit$globalObjective, 3.0, tolerance = 1e-12)
})

test_that("phase-based algorithms reject partial client participation", {
  expect_error(
    fitFederated(
      cl = list(1, 2),
      algorithm = "ODAL",
      config = list(
        clientFrac = 0.5,
        mapping = data.frame(covariateId = 1, columnId = 1)
      ),
      verbose = FALSE
    ),
    "does not support clientFrac"
  )
})
