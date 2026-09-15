penaltyRunner <- function() {
  paths <- file.path(c(getwd(), "../..", "../../.."), "extras", "runComparisonMatrix.R")
  path <- paths[file.exists(paths)][1L]
  if (is.na(path)) skip("Comparison runner is not in this test installation")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

test_that("Laplace prior variance converts to mean-loss lambda exactly once", {
  convert <- FederatedLearning:::.laplaceVarianceToLambda
  expect_equal(convert(0.01, 1000), sqrt(200) / 1000)
  expect_equal(convert(4, 1000), convert(1, 1000) / 2)
  expect_equal(convert(0.01, 2000), convert(0.01, 1000) / 2)
  for (variance in c(1e-200, 0.01, 1, 1e200)) {
    lambda <- convert(variance, 500)
    expect_equal(2 / (500 * lambda)^2, variance, tolerance = 1e-12)
  }
  for (bad in list(0, -1, NA_real_, Inf, numeric(), c(1, 2), "0.01")) {
    expect_error(convert(bad, 100), "variance")
    expect_error(convert(0.01, bad), "row count")
  }
  expect_error(convert(.Machine$double.xmin, .Machine$double.xmin), "unrepresentable")
  for (method in c("DualAvg", "DualAvgCpp", "DualAvgR")) {
    strategy <- FederatedLearning:::.getAlgorithm(method)$lambdaStrategy
    expect_equal(strategy$scale, "priorVariance")
    expect_equal(strategy$initial(0.01, 1000, list()), 0.01)
    expect_equal(strategy$fit(0.01, 700, list()), sqrt(200) / 700)
    expect_equal(strategy$final(0.01, 1000, list()), sqrt(200) / 1000)
  }
  for (method in c("ADAP", "ADAP1", "ADAPDiag", "ODAL")) {
    expect_null(FederatedLearning:::.getAlgorithm(method)$lambdaStrategy$fit)
  }
})

test_that("variance CV uses each training split's row count and preserves fold-local warm starts", {
  calls <- list()
  testthat::local_mocked_bindings(
    subsetCluster = function(cl, ids) list(ids = ids),
    fitFederated = function(cl, algorithm, config, verbose) {
      calls[[length(calls) + 1L]] <<- list(ids = cl$ids, config = config)
      n <- sum(sizes[cl$ids])
      list(w = 2 / (n * config$lambda)^2, z = rep(sum(cl$ids), 2),
        roundsCompleted = 3L, config = config)
    },
    clusterCreateMatrices = function(...) NULL,
    clusterEvaluateModel = function(cl, w) data.frame(auc = 0.8 - 0.01 * abs(log(w / 0.01))),
    .package = "FederatedLearning"
  )
  for (sizes in list(c(10, 20, 70), c(50, 50, 50, 50), c(10, 30))) {
    calls <- list()
    result <- FederatedLearning:::tuneLambda(
      cl = list(), algorithm = "DualAvg", configBase = list(
        warmStartLambdaPath = TRUE, warmStartRoundOffset = FALSE),
      trainIds = seq_along(sizes), rounds = 3L, clientFrac = 1, epsilon = 0,
      lambdaStrategy = FederatedLearning:::.lambdaStrategyDefault(),
      lambdaDefault = 0.01, totalPopSize = sum(sizes), trainPopSizes = sizes,
      globalMap = data.frame(covariateId = 1, columnId = 1), verbose = FALSE)
    expect_equal(result$bestSearchValue, 0.01)
    expect_equal(result$bestLambda, sqrt(200) / sum(sizes))
    expect_equal(unname(result$bestFitLambdas), sqrt(200) / (sum(sizes) - sizes))
    expect_equal(result$searchScale, "priorVariance")
    expect_equal(result$stopReason, "searchConverged")
    expect_equal(nrow(result$trace), length(calls))
    expect_equal(result$trace$fitLambda * result$trace$trainingRows,
      sqrt(2 / result$trace$searchValue))
    expect_true(all(is.finite(result$trace$auc)))
    if (length(unique(sizes)) > 1L) expect_true(is.na(result$bestLambdaTrain))
    for (i in seq_along(calls)) {
      cfg <- calls[[i]]$config
      if (i <= length(sizes)) {
        expect_null(cfg$initialZ)
      } else {
        expect_equal(cfg$initialZ, rep(sum(calls[[i]]$ids), 2))
        expect_equal(cfg$roundOffset, 0L)
      }
    }
  }
  expect_error(FederatedLearning:::tuneLambda(
    cl = list(), algorithm = "DualAvg", configBase = list(), trainIds = 1:2,
    rounds = 1, clientFrac = 1, epsilon = 0,
    lambdaStrategy = FederatedLearning:::.lambdaStrategyDefault(), lambdaDefault = 0.01,
    totalPopSize = 100, trainPopSizes = c(20, 30), globalMap = NULL), "row counts")
})

test_that("runner passes the selected final lambda through without another conversion", {
  env <- penaltyRunner()
  cfg <- list(mapping = data.frame(covariateId = 1, columnId = 1),
    rounds = 10L, epsilon = 0, clientFrac = 1, lambda = 99)
  testthat::local_mocked_bindings(
    tuneLambda = function(trainPopSizes, lambdaDefault, configBase, ...) {
      expect_equal(trainPopSizes, c(10, 20, 70))
      expect_equal(lambdaDefault, 0.01)
      expect_null(configBase$lambda)
      list(bestLambda = 0.003, bestSearchValue = 2 / (100 * 0.003)^2,
        bestFitLambdas = c(0.003 * 100 / 90, 0.003 * 100 / 80, 0.003 * 100 / 30),
        bestLambdaTrain = NA_real_, searchScale = "priorVariance", perf = 0.75,
        trace = data.frame(iteration = 0), stopReason = "searchConverged")
    }, .package = "FederatedLearning"
  )
  out <- env$tuneDualAvgForFold("DualAvg", NULL, cfg, c(10, 20, 70), list(), FALSE)
  expect_equal(out$lambda, 0.003)
  expect_equal(out$lambdaSearchSelectedVariance, 2 / (100 * 0.003)^2)
  expect_equal(out$lambdaSearchScale, "priorVariance")
  expect_equal(out$lambdaSearchInnerFitLambdas * c(90, 80, 30), rep(0.3, 3))
  expect_equal(out$lambdaSearchTrace, data.frame(iteration = 0))
  expect_identical(env$tuneDualAvgForFold("DualAvg", NULL, cfg, c(10, 20, 70),
    list("dualavg-lambda" = "99"), FALSE), cfg)
})

test_that("flat AUC search reports its evaluation limit rather than convergence", {
  testthat::local_mocked_bindings(
    subsetCluster = function(cl, ids) list(),
    fitFederated = function(cl, algorithm, config, verbose) list(w = 0, config = config),
    clusterCreateMatrices = function(...) NULL,
    clusterEvaluateModel = function(...) data.frame(auc = 0.5),
    .package = "FederatedLearning")
  expect_warning(out <- FederatedLearning:::tuneLambda(
    cl = list(), algorithm = "DualAvg", configBase = list(lambdaSearchMaxEvals = 2),
    trainIds = 1:2, rounds = 1, clientFrac = 1, epsilon = 0,
    lambdaStrategy = FederatedLearning:::.lambdaStrategyDefault(), lambdaDefault = 0.01,
    totalPopSize = 100, trainPopSizes = c(30, 70), globalMap = NULL, verbose = FALSE),
    "lambdaSearchMaxEvals")
  expect_equal(out$stopReason, "maxEvaluations")
  expect_equal(nrow(out$trace), 4)
  expect_equal(out$bestSearchValue, 0.01)
})

test_that("both DualAvg implementations penalize only configured predictors", {
  for (intercept in c(TRUE, FALSE)) {
    cfg <- list(intercept = intercept, lambda = 0.2, k = 2L,
      etaClient = 1, etaServer = 1, aggregation = "sampleSize")
    state <- list(z = c(1, -0.5), r = 2L)
    reports <- list(list(delta = c(0, 0), n = 4))
    expected <- if (intercept) c(1, 0) else c(0, 0)
    for (method in c("DualAvg", "DualAvgR")) {
      algo <- FederatedLearning:::.getAlgorithm(method)
      expect_equal(as.numeric(algo$serverRound(state, reports, cfg)$report$w), expected)
      x <- methods::as(Matrix::Matrix(cbind(1, c(-2, -1, 1, 2)), sparse = TRUE), "dgCMatrix")
      data <- list(xMatrix = x, yLabels = c(0, 1, 0, 1), n = 4)
      z <- state$z
      for (i in 0:1) {
        w <- proxL1(z, (state$r * cfg$k + i) * cfg$lambda, intercept = intercept)
        z <- z - as.numeric(crossprod(x, plogis(as.numeric(x %*% w)) - data$yLabels)) / 4
      }
      expect_equal(as.numeric(algo$clientUpdate(data, state, cfg)$delta), z - state$z,
        tolerance = 1e-12)
    }
  }
})

test_that("Cyclops baselines leave only the configured intercept unpenalized", {
  skip_if_not_installed("Cyclops")
  env <- penaltyRunner()
  set.seed(817)
  x <- methods::as(Matrix::Matrix(cbind(1, matrix(rnorm(2400), 800, 3)), sparse = TRUE), "dgCMatrix")
  colnames(x) <- c("constant", "age", "feature", "noise")
  y <- rbinom(800, 1, plogis(-1.5 + x[, 2] * 0.9))
  variance <- 2 / (800 * 0.03)^2
  args <- list("cyclops-cv" = "false", "cyclops-variance" = as.character(variance),
    "cyclops-tolerance" = "1e-10", "cyclops-max-iterations" = "10000")
  for (intercept in c(TRUE, FALSE)) {
    data <- list(xMatrix = x, yLabels = y, n = 800)
    fit <- env$fitBaselineWeights(list(data), args, 42L, intercept = intercept)
    g <- as.numeric(crossprod(x, plogis(as.numeric(x %*% fit$w)) - y)) / length(y)
    expect_lt(abs(g[1] + if (intercept) 0 else 0.03 * sign(fit$w[1])), 1e-7)
    expect_equal(fit$fittingSettings$intercept, intercept)
    expect_equal(fit$fittingSettings$unpenalizedCovariates, if (intercept) 1 else NULL)
    active <- fit$w[-1L] != 0
    expect_true(all(abs(g[-1L][!active]) <= 0.03 + 1e-7))
    expect_true(all(abs(g[-1L][active] + 0.03 * sign(fit$w[-1L][active])) < 1e-7))
    local <- env$fitLocalBaselineSafely(list(data), 1L, "site", args, 42L, intercept = intercept)
    expect_true(local$ok)
    expect_equal(local$fit$w, fit$w, tolerance = 1e-10)
  }
  expect_error(env$fitCyclopsWeights(list(list(xMatrix = x[, -1], yLabels = y)),
    args, 42L, intercept = TRUE), "Configured intercept")
})
