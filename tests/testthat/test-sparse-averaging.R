sparseRunner <- function() {
  path <- testthat::test_path("..", "..", "extras", "runComparisonMatrix.R")
  if (!file.exists(path)) skip("Extras scripts unavailable")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

test_that("soft thresholding solves the coefficient-distance L1 objective", {
  w <- c(-3, 0.8, 0.04, -0.03, 0.25)
  result <- softThresholdAverage(w, 0.05)
  expect_equal(result, c(-3, 0.75, 0, 0, 0.2))
  expect_identical(softThresholdAverage(w, 0), w)
  expect_equal(softThresholdAverage(w, 2), c(-3, 0, 0, 0, 0))
  expect_equal(softThresholdAverage(2, 3), 2)
  expect_equal(softThresholdAverage(2, 3, FALSE), 0)
  slopes <- 2:5
  residual <- result[slopes] - w[slopes]
  nonzero <- result[slopes] != 0
  expect_equal(residual[nonzero] + 0.05 * sign(result[slopes][nonzero]), rep(0, sum(nonzero)))
  expect_true(all(abs(residual[!nonzero]) <= 0.05))
  expect_error(softThresholdAverage(w, -1), "nonnegative")
  expect_error(softThresholdAverage(c(1, NA), 1), "finite")
  expect_error(softThresholdAverage(w, 1, scales = 0), "scales")
})

test_that("thresholds are invariant to optional baseline normalization", {
  w <- c(-1, 0.4, -0.08)
  scales <- c(1, 100, 2)
  expect_equal(softThresholdAverage(w * scales, 0.1, scales = scales) / scales,
    softThresholdAverage(w, 0.1))
  expect_equal(softThresholdAverage(1e300, 1, FALSE, 1e-300), 1e300)
  expect_equal(softThresholdAverage(1e300, 1e300, FALSE, 1e300), 0)
})

test_that("nodewise precision has the correct sign, units and limiting inverse", {
  h <- matrix(c(2, 0.3, 0.1, 0.3, 1, -0.2, 0.1, -0.2, 0.5), 3)
  inverse <- nodewisePrecision(h, 1000, multiplier = 0)$precision
  expect_equal(inverse, solve(h), tolerance = 1e-5)
  scale <- c(1, 100, 0.5)
  regularized <- nodewisePrecision(h, 1000)
  scaled <- nodewisePrecision(h * outer(scale, scale), 1000)
  expect_equal(scaled$precision * outer(scale, scale), regularized$precision, tolerance = 1e-10)
  expect_equal(nodewisePrecision(matrix(0.2, 1, 1), 100)$precision, matrix(5, 1, 1))
  expect_equal(nodewisePrecision(h[1:2, 1:2], 1000, 0)$precision,
    solve(h[1:2, 1:2]), tolerance = 1e-5)
  duplicate <- matrix(1, 3, 3)
  expect_true(all(is.finite(nodewisePrecision(duplicate, 100)$precision)))
  expect_error(nodewisePrecision(duplicate, 100, 0), "singular")
  absent <- diag(c(1, 0, 2))
  expect_equal(nodewisePrecision(absent, 100)$precision, diag(c(1, 0, 0.5)))
  expect_identical(nodewisePrecision(absent, 100)$diagnostics$zeroCurvatureColumns, 2L)
  expect_error(nodewisePrecision(matrix(0, 2, 2), 100), "No positive")
  expect_error(nodewisePrecision(matrix(c(1, 2, 2, 1), 2), 100), "semidefinite")
  expect_error(nodewisePrecision(h, 0), "sample count")
})

test_that("local debiasing matches an unpenalized Newton correction without clipping", {
  set.seed(25)
  x <- cbind(1, rnorm(100), rbinom(100, 1, 0.3))
  y <- rbinom(100, 1, 0.3)
  w <- c(-0.5, 0.2, -0.1)
  data <- list(xMatrix = x, yLabels = y, n = 100)
  p <- plogis(drop(x %*% w))
  g <- drop(crossprod(x, p - y)) / 100
  h <- crossprod(x, (p * (1 - p)) * x) / 100
  fit <- debiasLocalLasso(data, w, multiplier = 0)
  expect_equal(fit$gradient, g, tolerance = 1e-14)
  expect_equal(fit$w, w - drop(solve(h, g)), tolerance = 1e-5)
  data$xMatrix <- Matrix::Matrix(x, sparse = TRUE)
  expect_equal(debiasLocalLasso(data, w, 0), fit)
  data$n <- 101
  expect_error(debiasLocalLasso(data, w), "sample counts")
  data$n <- 100
  expect_error(debiasLocalLasso(data, w, maxFeatures = 2), "at most")
  data$yLabels[1] <- NA
  expect_error(debiasLocalLasso(data, w), "binary")
  extreme <- list(xMatrix = matrix(1, 10, 1), yLabels = rep(1, 10), n = 10)
  fit <- debiasLocalLasso(extreme, 40)
  expect_equal(fit$w, 41, tolerance = 1e-10)
  expect_true(fit$gradient < 0)
})

test_that("the pseudo-design reproduces nodewise regression on weighted patient rows", {
  set.seed(88)
  x <- cbind(1, rnorm(150), rnorm(150), rbinom(150, 1, 0.2))
  eta <- drop(x %*% c(-2, 0.2, -0.3, 0.1))
  weighted <- sqrt(plogis(eta) * plogis(-eta)) * x
  h <- crossprod(weighted) / nrow(x)
  result <- nodewisePrecision(h, nrow(x))
  d <- sqrt(diag(h))
  z <- sweep(weighted, 2, d, "/")
  expected <- matrix(0, 4, 4)
  for (j in 1:4) {
    fitArgs <- list(x = z[, -j], y = z[, j], lambda = result$diagnostics$nodewiseLambda,
      alpha = 1, standardize = FALSE, intercept = FALSE)
    controls <- list(thresh = 1e-12, maxit = 100000L)
    if ("control" %in% names(formals(glmnet::glmnet))) fitArgs$control <- controls else fitArgs <- c(fitArgs, controls)
    fit <- do.call(glmnet::glmnet, fitArgs)
    gamma <- as.numeric(fit$beta)
    residual <- z[, j] - drop(z[, -j] %*% gamma)
    tau2 <- mean(residual^2) + result$diagnostics$nodewiseLambda * sum(abs(gamma))
    expected[j, j] <- 1 / tau2
    expected[j, -j] <- -gamma / tau2
  }
  expect_equal(result$precision, expected / outer(d, d), tolerance = 1e-5)
  permutation <- c(3, 1, 4, 2)
  expect_equal(nodewisePrecision(h[permutation, permutation], nrow(x))$precision,
    result$precision[permutation, permutation], tolerance = 1e-5)
})

test_that("averaging uses actual training sizes and is invariant to site order", {
  env <- sparseRunner()
  data <- list(list(xMatrix = matrix(0, 2, 3)), list(xMatrix = matrix(0, 6, 3)))
  fits <- list(list(w = c(-1, 0.2, 0)), list(w = c(-3, -0.2, 0.4)))
  expected <- c(-2.5, -0.1, 0.3)
  aggregate <- env$averageSparseLocalFits(fits, data, "SparseLocalAvgLasso", list())
  expect_equal(aggregate$w, expected)
  expect_equal(aggregate$weights, c(0.25, 0.75))
  expect_equal(env$averageSparseLocalFits(rev(fits), rev(data), "SparseLocalAvgLasso", list())$w, expected)
  fits[[1]]$w <- 1
  expect_error(env$averageSparseLocalFits(fits, data, "SparseLocalAvgLasso", list()), "dimension mismatch")
})

test_that("threshold validation excludes its site from fitting and preprocessing", {
  env <- sparseRunner()
  clients <- lapply(1:3, function(i) list(xMatrix = cbind(1, c(0, i, 0, i)),
    yLabels = c(0, 1, 0, 1), n = 4, site = i))
  calls <- list()
  preprocess <- env$preprocessBaselineData
  env$preprocessBaselineData <- function(trainData, testData, config, args) {
    training <- vapply(trainData, `[[`, integer(1), "site")
    validation <- testData[[1]]$site
    expect_false(validation %in% training)
    calls[[length(calls) + 1L]] <<- list(training = training, validation = validation)
    preprocess(trainData, testData, config, args)
  }
  env$fitBaselineWeights <- function(clientDataList, args, seed, intercept) {
    expect_length(clientDataList, 1)
    expect_true(clientDataList[[1]]$site %in% tail(calls, 1)[[1]]$training)
    list(w = c(-1, 0.2), elapsedSeconds = 0)
  }
  config <- list(intercept = TRUE)
  args <- list("local-average-thresholds" = "0,0.01,0.5")
  tuned <- env$tuneSparseAverage(clients, "SparseLocalAvgLasso", args, config, 42)
  expect_length(calls, 3)
  expect_equal(tuned$tau, 0)
  expect_equal(tuned$trace$score, rep(c(1, 1, 0.5), 3))
  expect_true(all(tuned$trace$successfulLocalFits == 2))
  expect_equal(env$tuneSparseAverage(clients, "SparseLocalAvgLasso",
    list("local-average-threshold" = "0.1"), config, 42)$tau, 0.1)
  expect_length(calls, 3)
  clients[[1]]$yLabels[] <- 0
  expect_error(env$tuneSparseAverage(clients, "SparseLocalAvgLasso", args, config, 42), "Non-finite")
  expect_error(env$sparseAverageSettings(list("local-average-threshold" = "-1")), "Invalid")
  expect_error(suppressWarnings(env$sparseAverageSettings(list("local-average-thresholds" = "0,bad"))), "Invalid")
  expect_error(env$sparseAverageSettings(list("local-average-metric" = "testAuc")), "must be")
  args[["local-average-metric"]] <- "logLoss"
  tuned <- env$tuneSparseAverage(clients, "SparseLocalAvgLasso", args, config, 42)
  meanScores <- aggregate(score ~ tau, tuned$trace, mean)
  expect_equal(tuned$tau, meanScores$tau[which.min(meanScores$score)])
})

test_that("Cyclops and both sparse aggregators fit a small end-to-end problem", {
  skip_if_not_installed("Cyclops")
  env <- sparseRunner()
  set.seed(490)
  clients <- lapply(1:3, function(i) {
    x <- Matrix::Matrix(cbind(1, rnorm(120), rbinom(120, 1, 0.4), rnorm(120)), sparse = TRUE)
    y <- rbinom(120, 1, plogis(as.numeric(x %*% c(-1, 0.8, -0.5, 0))))
    list(xMatrix = x, yLabels = y, n = 120)
  })
  args <- list("cyclops-cv" = "false", "local-average-thresholds" = "0,0.01,0.1")
  for (method in env$sparseAverageMethods) {
    tuned <- env$tuneSparseAverage(clients, method, args, list(intercept = TRUE), 91)
    expect_equal(nrow(tuned$trace), 9L)
    expect_true(all(is.finite(tuned$trace$score)))
    expect_true(tuned$tau %in% c(0, 0.01, 0.1))
  }
})

test_that("sparse baseline artifacts preserve components, threshold and exact zeros", {
  env <- sparseRunner()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  clients <- lapply(c(a = 4, b = 6, c = 8, test = 4), function(n) list(n = n,
    xMatrix = cbind(1, seq_len(n), rep(c(0, 1), length.out = n)),
    yLabels = rep(c(0, 1), length.out = n)))
  local_mocked_bindings(
    loadClientData = function(path, popSettings) clients[[path]],
    getClientFeatures = function(plpData) data.frame(covariateId = c(10, 20)),
    createClientMatrix = function(plpData, config) plpData,
    .package = "FederatedLearning"
  )
  env$assertCyclopsMethod <- function(method) NULL
  env$fitBaselineWeights <- function(clientDataList, args, seed, intercept) {
    n <- clientDataList[[1]]$n
    if (n == 6) stop("local failure")
    list(w = c(-2, 0.2, 0.01), selectedLambda = 0.01, elapsedSeconds = 0)
  }
  run <- function(method, tau) env$fitBaselineFold(method, c("a", "b", "c"), "test", list(),
    list(intercept = TRUE, mapType = "union", pooledDiagnostics = FALSE),
    list("local-average-threshold" = as.character(tau)), "task", "all", 1L,
    c("a", "b", "c"), "test", 4L, file.path(directory, "models"))
  plain <- run("LocalAvgLasso", 0)
  sparse <- run("SparseLocalAvgLasso", 0)
  expect_equal(sparse$auc, plain$auc)
  expect_equal(sparse$nonzeroPredictors, plain$nonzeroPredictors)
  sparse <- run("SparseLocalAvgLasso", 0.05)
  saved <- readRDS(file.path(directory, sparse$modelFile))
  expect_equal(saved$models[[1]]$coefficients$coefficient, c(-2, 0.15, 0))
  expect_equal(sparse$nonzeroPredictors, 1)
  expect_equal(sparse$aggregationThreshold, 0.05)
  expect_equal(vapply(saved$components, `[[`, numeric(1), "weight"), c(1 / 3, 2 / 3))
  expect_equal(saved$localFailures[[1]]$trainClientId, "b")
  expect_true(is.na(sparse$messages))
  expect_equal(saved$models[[1]]$fittingSettings$thresholdTuning$tau, 0.05)
  debiased <- run("DebiasedLocalAvgLasso", 0.05)
  saved <- readRDS(file.path(directory, debiased$modelFile))
  expect_length(saved$models[[1]]$fittingSettings$debias, 2)
  expect_true(all(is.finite(saved$models[[1]]$coefficients$coefficient)))
})
