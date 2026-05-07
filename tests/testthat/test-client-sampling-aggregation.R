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

test_that("DualAvgCpp supports partial client participation", {
  expect_true(FederatedLearning:::.getAlgorithm("DualAvgCpp")$supportsClientSampling)
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
