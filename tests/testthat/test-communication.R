communicationRow <- function(table, exchange, direction) {
  row <- table[table$exchange == exchange & table$direction == direction, , drop = FALSE]
  expect_equal(nrow(row), 1L)
  row
}

test_that("communication logs count messages, values and bytes per exchange", {
  log <- .newCommunicationLog()
  .recordExchange(log, "update", list(list(z = 1:3, r = 1L), NULL), list(list(delta = c(1, 2), n = 4), NULL))
  .recordExchange(log, "update", list(list(z = 1:3, r = 2L), list(z = 1:3, r = 2L)),
    list(list(delta = c(1, 2), n = 4), list(delta = c(3, 4), n = 5, label = "x")))
  table <- .communicationTable(log)
  down <- communicationRow(table, "update", "server to site")
  up <- communicationRow(table, "update", "site to server")
  expect_equal(c(down$exchanges, down$messages, down$values, down$maxMessageValues), c(2, 3, 12, 4))
  expect_equal(c(up$exchanges, up$messages, up$values), c(2, 3, 9))
  expect_equal(down$bytes, 3 * length(serialize(list(z = 1:3, r = 1L), NULL)))
  totals <- .communicationTotals(table)
  expect_equal(unlist(totals[c("exchanges", "messages", "values")]),
    c(exchanges = 2, messages = 6, values = 21))
  combined <- .combineCommunication(list(table, NULL, .asTuningCommunication(table), table))
  expect_equal(nrow(combined), 4L)
  expect_equal(combined$messages[combined$stage == "fit"], 2 * table$messages)
  expect_equal(.communicationTotals(NULL)$messages, 0)
})

test_that("PDA protocols count lead-only phases and protocol fields only", {
  log <- .newCommunicationLog()
  state <- list(phase = 2L, leadIndex = 2L, betaBar = 1:3, globalGrad = 1:3,
    globalHess = diag(3), hessianCondition = 1, siteHessianEigenMin = 0)
  replies <- list(NULL, list(w = 1:3, convergence = 0, returnHessian = diag(3)), NULL)
  .recordProtocolRound(log, list(protocol = .adapProtocol), state, 1:3, replies)
  table <- .communicationTable(log)
  down <- communicationRow(table, "lead surrogate fit", "server to site")
  up <- communicationRow(table, "lead surrogate fit", "site to server")
  expect_equal(c(down$messages, down$values), c(1, 1 + 3 + 3 + 9))
  expect_equal(c(up$messages, up$values), c(1, 3))

  transport <- .newCommunicationLog()
  .recordProtocolRound(transport, list(), state, 1:3, replies)
  expect_equal(.communicationTotals(.communicationTable(transport))$messages, 3 + 1)
})

test_that("modeled baseline communication covers model transfers and selection", {
  runner <- comparisonRunnerEnv()
  average <- runner$baselineCommunication
  avg <- average("LocalAvgLasso", p = 4L, nModels = 3L, nTrainSites = 3L)
  expect_equal(avg$basis, "modeled")
  expect_equal(.communicationTotals(avg$fit)$values, 12)
  expect_null(avg$tuning)
  best <- average("LocalBestLasso", p = 4L, nModels = 3L, nTrainSites = 3L)
  expect_equal(.communicationTotals(best$tuning)$messages, 2 * 6)
  expect_identical(average("PooledLasso", 4L, 1L, 3L)$fit, NA)
  expect_true(is.na(average("SparseLocalAvgLasso", 4L, 3L, 3L, thresholdTuned = TRUE)$tuning))
  columns <- runner$communicationColumns(NA, NA, "pooledData")
  expect_true(all(is.na(unlist(columns[setdiff(names(columns), "communicationBasis")]))))
})

test_that("real workers report ODAL2 and DualAvg protocol communication", {
  skip_on_cran()
  testthat::local_mocked_bindings(clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning")
  cl <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(cl, 1:2, function(i) {
    library(FederatedLearning)
    x <- Matrix::Matrix(cbind(1, rep(c(0, 1), each = 40)), sparse = TRUE)
    y <- c(rep(c(0, 0, 0, 1), 10), rep(c(0, 0, 1, 1), 10))
    keep <- if (i == 1) seq_along(y) else sort(c(seq(1, length(y), by = 4), seq(4, length(y), by = 4)))
    assign("clientData", list(xMatrix = methods::as(x[keep, , drop = FALSE], "dgCMatrix"),
      yLabels = y[keep], n = length(keep)), envir = .GlobalEnv)
    NULL
  })
  mapping <- data.frame(covariateId = 1002, columnId = 1L)
  odal <- fitFederated(cl, "ODAL2", list(mapping = mapping, p = 1L, intercept = TRUE,
    rounds = 3L, epsilon = 0, odalInit = "pda"), verbose = FALSE)
  expect_equal(odal$communicationBasis, "protocol")
  table <- odal$communication
  expect_equal(unique(table$exchange),
    c("local estimates", "gradients and Hessians", "lead surrogate fit"))
  expect_equal(table$messages, c(2, 2, 2, 2, 1, 1))
  # Two coefficients: (bhat, n), (grad, Hess, n) and w from sites; phase,
  # betaBar and, for the lead, the global gradient and Hessian from the server.
  expect_equal(table$values, c(2, 6, 6, 14, 9, 2))
  expect_false("objective check" %in% table$exchange)

  dual <- fitFederated(cl, "DualAvg", list(mapping = mapping, p = 1L, intercept = TRUE,
    rounds = 5L, k = 2L, etaClient = 0.1, etaServer = 1, lambda = 0.01, epsilon = 0),
    verbose = FALSE)
  table <- dual$communication
  p <- length(dual$w)
  update <- communicationRow(table, "dual averaging update", "server to site")
  expect_equal(c(update$exchanges, update$messages, update$values), c(5, 10, 10 * (p + 1)))
  reply <- communicationRow(table, "dual averaging update", "site to server")
  expect_equal(reply$values, 10 * (p + 1))
  check <- communicationRow(table, "objective check", "server to site")
  expect_equal(c(check$exchanges, check$values), c(5, 10 * p))
  expect_equal(.communicationTotals(table)$exchanges, 10)
})

test_that("penalty tuning sums every inner fit and validation exchange", {
  log <- .newCommunicationLog()
  .recordExchange(log, "dual averaging update", list(1:2, 1:2), list(1:2, NULL))
  fitTable <- .communicationTable(log)
  testthat::local_mocked_bindings(
    subsetCluster = function(cl, ids) list(ids = ids),
    fitFederated = function(cl, algorithm, config, verbose) {
      list(w = c(2 / (100 * config$lambda)^2, 1), roundsCompleted = 3L, config = config,
        communication = fitTable)
    },
    clusterCreateMatrices = function(...) NULL,
    clusterEvaluateModel = function(cl, w) data.frame(logLoss = 0.01 * abs(log(w[1] / 0.01)) - 0.8),
    .package = "FederatedLearning"
  )
  sizes <- c(40, 60)
  result <- tuneLambda(cl = list(), algorithm = "DualAvg", configBase = list(),
    trainIds = 1:2, rounds = 3L, clientFrac = 1, epsilon = 0,
    lambdaStrategy = .lambdaStrategyDefault(), lambdaDefault = 0.01,
    totalPopSize = 100, trainPopSizes = sizes,
    globalMap = data.frame(covariateId = 1, columnId = 1), verbose = FALSE)
  fits <- nrow(result$trace)
  table <- result$communication
  expect_equal(unique(table$stage), "tuning")
  update <- table[table$exchange == "dual averaging update", ]
  expect_equal(sum(update$messages), 3 * fits)
  validation <- table[table$exchange == "validation loss", ]
  expect_equal(c(sum(validation$messages), max(validation$exchanges)), c(2 * fits, fits))
})
