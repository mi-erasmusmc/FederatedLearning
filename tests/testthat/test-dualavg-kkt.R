test_that("combined C++ objective and gradient match independent dense math", {
  x <- cbind(1, c(-2, -1, 0, 2, 3), c(0, 1, 0, 1, 1))
  y <- c(0, 1, 0, 1, 1)
  beta <- c(-0.3, 0.2, 0.1)
  evaluate <- function(x, y, beta) {
    FederatedLearning:::logisticObjectiveGradientCpp(
      FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE)), beta, y)
  }
  actual <- evaluate(x, y, beta)
  eta <- as.numeric(x %*% beta)
  expected <- as.numeric(crossprod(x, plogis(eta) - y)) / length(y)
  expect_equal(actual$loss, sum(log1p(exp(eta)) - y * eta), tolerance = 1e-12)
  expect_equal(actual$cyclopsObjective, sum(eta * y), tolerance = 1e-12)
  expect_equal(as.numeric(actual$gradient), expected, tolerance = 1e-12)
  numerical <- vapply(seq_along(beta), function(j) {
    step <- replace(numeric(length(beta)), j, 1e-6)
    (evaluate(x, y, beta + step)$loss - evaluate(x, y, beta - step)$loss) /
      (2e-6 * length(y))
  }, numeric(1))
  expect_equal(as.numeric(actual$gradient), numerical, tolerance = 1e-8)
  pieces <- list(evaluate(x[1, , drop = FALSE], y[1], beta),
    evaluate(x[-1, , drop = FALSE], y[-1], beta))
  expect_equal(FederatedLearning:::weightedReportAverage(pieces, "gradient", "sampleSize"),
    expected, tolerance = 1e-12, ignore_attr = TRUE)

  extreme <- evaluate(matrix(c(-1000, -100, 0, 100, 1000), ncol = 1),
    c(0, 0, 1, 1, 1), 1)
  expect_equal(extreme$loss, log(2), tolerance = 1e-12)
  expect_true(all(is.finite(extreme$gradient)))
  # The certificate uses the true logistic gradient, not a clipped probability.
  expect_lt(abs(extreme$gradient[1]), 1e-30)
  expect_error(evaluate(x, y[-1], beta), "conformable")
  expect_error(evaluate(x, replace(y, 1, NA_real_), beta), "finite")
  expect_error(evaluate(x, y, replace(beta, 1, Inf)), "finite")
  expect_error(evaluate(replace(x, 1, NA_real_), y, beta), "finite")
})

test_that("KKT residual treats zeros, signs and the unpenalized intercept exactly", {
  residual <- FederatedLearning:::.lassoKktResidual
  expect_equal(residual(c(0, 2, -2, 0, 0), c(0.2, -0.1, 0.1, 0.08, -0.14), 0.1),
    c(0.2, 0, 0, 0, 0.04), tolerance = 1e-14)
  expect_equal(residual(c(0, 0), c(0.08, -0.14), 0.1, intercept = FALSE), c(0, 0.04))
  expect_equal(residual(c(0, 1e-12), c(0, 0), 0.1), c(0, 0.1))
  expect_equal(residual(c(1, 0), c(-0.2, 0.3), 0), c(0.2, 0.3))
  expect_error(residual(c(0, 0), 1, 0.1), "conformable")
  expect_error(residual(c(0, 0), c(NA, 0), 0.1), "finite")
  expect_error(residual(c(0, 0), c(0, 0), -1), "non-negative")
})

test_that("KKT options reject invalid or unsupported configurations before worker calls", {
  base <- list(dualAvgKktTolerance = 1e-7, lambda = 0.01)
  for (value in list(0, -1, NA_real_, Inf, c(1, 2), "small")) {
    expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
      list(dualAvgKktTolerance = value))), "positive finite scalar")
  }
  for (value in list(0, -1, 0.5, NA_real_, Inf, c(1, 2))) {
    expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
      list(dualAvgKktCheckEvery = value))), "positive integer")
  }
  expect_error(fitFederated(NULL, "ODAL", base), "only for DualAvg")
  expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
    list(convergenceObjective = "none"))), "requires objective monitoring")
  expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
    list(lambda = -1))), "non-negative lambda")
})

test_that("KKT safeguards prevent false stops without extra per-round communication", {
  skip_on_cran()
  testthat::local_mocked_bindings(clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning")
  cl <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  data <- list(
    list(xMatrix = cbind(1, c(-1, 0, 1)), yLabels = c(0, 0, 1)),
    list(xMatrix = cbind(1, c(-2, -1, 0, 1, 2)), yLabels = c(0, 0, 1, 1, 1)))
  parallel::clusterApply(cl, data, function(d) {
    library(FederatedLearning)
    d$xMatrix <- methods::as(Matrix::Matrix(d$xMatrix, sparse = TRUE), "dgCMatrix")
    d$n <- length(d$yLabels)
    assign("clientData", d, envir = .GlobalEnv)
    NULL
  })
  calls <- c(evaluate = 0L, export = 0L)
  originalEval <- parallel::clusterEvalQ
  originalExport <- parallel::clusterExport
  testthat::local_mocked_bindings(
    clusterEvalQ = function(cl, expr) {
      calls[["evaluate"]] <<- calls[["evaluate"]] + 1L
      call <- match.call()
      call[[1]] <- originalEval
      eval(call, parent.frame())
    },
    clusterExport = function(cl, varlist, envir = .GlobalEnv) {
      calls[["export"]] <<- calls[["export"]] + 1L
      originalExport(cl, varlist, envir)
    }, .package = "parallel")
  base <- list(mapping = data.frame(covariateId = 1, columnId = 1), p = 1L,
    intercept = TRUE, rounds = 10L, k = 2L, etaClient = 0.1, etaServer = 1,
    lambda = 0.01, epsilon = 100, convergenceObjective = "cyclopsGradient")
  loose <- fitFederated(cl, "DualAvg", base, verbose = FALSE)
  expect_equal(loose$roundsCompleted, 2L)
  calls[] <- 0L
  fixed <- fitFederated(cl, "DualAvg", modifyList(base, list(epsilon = 0)), verbose = FALSE)
  fixedCalls <- calls
  calls[] <- 0L
  guarded <- fitFederated(cl, "DualAvg", modifyList(base,
    list(dualAvgKktTolerance = 1e-12, dualAvgKktCheckEvery = 3L)), verbose = FALSE)
  expect_equal(calls, fixedCalls)
  expect_equal(guarded$roundsCompleted, 10L)
  expect_false(guarded$converged)
  expect_identical(guarded$stopReason, "roundLimit")
  expect_equal(guarded$kktChecks, 4L)
  expect_equal(guarded$w, fixed$w, tolerance = 1e-14)
  expect_equal(guarded$z, fixed$z, tolerance = 1e-14)
  x <- do.call(rbind, lapply(data, `[[`, "xMatrix"))
  y <- unlist(lapply(data, `[[`, "yLabels"))
  g <- as.numeric(crossprod(x, plogis(as.numeric(x %*% guarded$w)) - y)) / length(y)
  expect_equal(guarded$kktMaxAbs, max(FederatedLearning:::.lassoKktResidual(
    guarded$w, g, base$lambda)), tolerance = 1e-12)

  # Final diagnostics reuse the already returned gradient, rather than another exchange.
  calls[] <- 0L
  diagnostics <- fitFederated(cl, "DualAvg", modifyList(base,
    list(dualAvgKktTolerance = 1e-12, dualAvgKktCheckEvery = 3L,
      pooledDiagnostics = TRUE)), verbose = FALSE)
  expect_equal(calls, fixedCalls)
  expect_equal(diagnostics$kktMaxAbs, diagnostics$pooledKktMaxAbs, tolerance = 1e-12)

  # Check every client's gradient even when only one participates in the update.
  for (aggregation in c("sampleSize", "equalClient")) {
    sampled <- fitFederated(cl, "DualAvg", modifyList(base,
      list(clientFrac = 0.5, clientSampleSeed = 10L, aggregation = aggregation,
        dualAvgKktTolerance = 1e-12, dualAvgKktCheckEvery = 20L)), verbose = FALSE)
    gradients <- lapply(data, function(d) {
      as.numeric(crossprod(d$xMatrix, plogis(as.numeric(d$xMatrix %*% sampled$w)) -
        d$yLabels)) / length(d$yLabels)
    })
    weights <- if (aggregation == "sampleSize") c(3, 5) / 8 else c(0.5, 0.5)
    gradient <- Reduce(`+`, Map(`*`, gradients, weights))
    expect_equal(sampled$kktChecks, 1L)
    expect_equal(sampled$kktMaxAbs, max(FederatedLearning:::.lassoKktResidual(
      sampled$w, gradient, base$lambda)), tolerance = 1e-12)
  }
})

test_that("KKT safeguard accepts a known optimum and preserves disabled early stopping", {
  skip_on_cran()
  testthat::local_mocked_bindings(clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning")
  cl <- parallel::makePSOCKcluster(1)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    library(FederatedLearning)
    clientData <- list(xMatrix = methods::as(Matrix::Matrix(cbind(1, c(-1, -1, 1, 1)),
      sparse = TRUE), "dgCMatrix"), yLabels = c(0, 1, 0, 1), n = 4L)
    NULL
  })
  base <- list(mapping = data.frame(covariateId = 1, columnId = 1), p = 1L,
    intercept = TRUE, rounds = 10L, k = 2L, etaClient = 0.1, etaServer = 1,
    lambda = 0.01, epsilon = 1e-6, dualAvgKktTolerance = 1e-12,
    dualAvgKktCheckEvery = 3L)
  for (algorithm in c("DualAvg", "DualAvgCpp", "DualAvgR")) {
    fit <- fitFederated(cl, algorithm, base, verbose = FALSE)
    expect_true(fit$converged)
    expect_identical(fit$stopReason, "converged")
    expect_equal(fit$roundsCompleted, 3L)
    expect_equal(fit$kktMaxAbs, 0)
    expect_equal(as.numeric(fit$w), c(0, 0))
  }
  fixed <- fitFederated(cl, "DualAvg", modifyList(base, list(epsilon = 0)), verbose = FALSE)
  expect_equal(fixed$roundsCompleted, 10L)
  expect_false(fixed$converged)
  expect_equal(fixed$kktMaxAbs, 0)
})
