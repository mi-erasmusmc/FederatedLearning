library(FederatedLearning)

test_that("filterCovariateRef selects named feature sets", {
  covariateRef <- data.frame(
    covariateId = c(1002, 8532001, 111, 222, 333),
    analysisId = c(1, 1, 49, 49, 7),
    covariateName = letters[1:5]
  )

  expect_equal(
    filterCovariateRef(covariateRef, featureSet = "ageSex")$covariateId,
    c(1002, 8532001)
  )
  expect_equal(
    filterCovariateRef(covariateRef, featureSet = "phenotypes")$covariateId,
    c(111, 222)
  )
  expect_equal(
    filterCovariateRef(covariateRef, featureSet = "ageSexPhenotypes")$covariateId,
    c(1002, 8532001, 111, 222)
  )
  expect_equal(
    filterCovariateRef(
      covariateRef,
      featureSet = "ageSexPhenotypes",
      covariateIds = c(1002, 222),
      analysisIds = c(1, 49)
    )$covariateId,
    c(1002, 222)
  )
  expect_error(
    filterCovariateRef(covariateRef, featureSet = "unknown"),
    "Unknown featureSet"
  )
})

test_that("createGlobalMap applies feature filters before union or intersection", {
  client1 <- data.frame(
    covariateId = c(1002, 8532001, 111, 333),
    analysisId = c(1, 1, 49, 7)
  )
  client2 <- data.frame(
    covariateId = c(1002, 111, 222, 444),
    analysisId = c(1, 49, 49, 7)
  )

  unionMap <- createGlobalMap(
    list(client1, client2),
    type = "union",
    featureSet = "ageSexPhenotypes"
  )
  expect_equal(unionMap$covariateId, c(111, 222, 1002, 8532001))
  expect_equal(unionMap$columnId, seq_len(nrow(unionMap)))

  intersectionMap <- createGlobalMap(
    list(client1, client2),
    type = "intersection",
    featureSet = "ageSexPhenotypes"
  )
  expect_equal(intersectionMap$covariateId, c(111, 1002))
  expect_equal(intersectionMap$columnId, c(1L, 2L))
})

test_that("fitFederated rejects an empty precomputed feature map", {
  expect_error(
    fitFederated(
      cl = list(),
      algorithm = "DualAvg",
      config = list(mapping = data.frame(covariateId = numeric(), columnId = integer())),
      verbose = FALSE
    ),
    "Global feature map is empty"
  )
})

test_that("clusterEvaluateModel returns expected metrics for synthetic clients", {
  cl <- parallel::makeCluster(1)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterEvalQ(cl, {
    clientData <- list(
      xMatrix = Matrix::Matrix(
        c(
          1, -2,
          1, -1,
          1, 1,
          1, 2
        ),
        ncol = 2,
        byrow = TRUE,
        sparse = TRUE
      ),
      yLabels = c(0L, 0L, 1L, 1L)
    )
    assign("clientData", clientData, envir = .GlobalEnv)
    NULL
  })

  metrics <- clusterEvaluateModel(cl, w = c(0, 2), threshold = 0.1)

  expect_equal(nrow(metrics), 1L)
  expect_equal(metrics$client, 1L)
  expect_equal(metrics$n, 4L)
  expect_equal(metrics$outcomes, 2L)
  expect_equal(metrics$auc, 1, tolerance = 1e-8)
  expect_lt(metrics$logLoss, 0.15)
  expect_equal(metrics$density, 0.5)
})
