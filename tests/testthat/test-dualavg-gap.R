gap_test_summary <- function(x, y, beta) {
  FederatedLearning:::logisticObjectiveGradientCpp(
    FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE)), beta, y,
    dualStats = TRUE)
}

gap_test_entropy <- function(q) {
  value <- numeric(length(q))
  inside <- q > 0 & q < 1
  value[inside] <- -q[inside] * log(q[inside]) - (1 - q[inside]) * log1p(-q[inside])
  mean(value)
}

test_that("dual candidate summaries and entropy agree with independent R calculations", {
  x <- cbind(1, c(-2, -1, 0, 1, 2), c(0, 1, 0, 1, 1))
  y <- c(0, 0, 1, 0, 1)
  beta <- c(0.4, -0.2, 0.1)
  eta <- as.numeric(x %*% beta)
  r <- plogis(eta) - y
  actual <- gap_test_summary(x, y, beta)
  expect_equal(as.numeric(actual$dualResidual), r, tolerance = 1e-14)
  expect_equal(as.numeric(actual$dualMass), c(sum(r[y == 0]), -sum(r[y == 1])) / length(y))
  expectedGradient <- crossprod(x, cbind(ifelse(y == 0, r, 0), ifelse(y == 1, -r, 0))) / length(y)
  expect_equal(actual$dualClassGradient, unname(expectedGradient), tolerance = 1e-14)
  for (scales in list(c(0, 0), c(1, 1), c(0.2, 0.7))) {
    scaled <- r * scales[y + 1L]
    evaluated <- FederatedLearning:::logisticDualEntropyCpp(r, y, scales)
    expect_equal(evaluated$entropy, gap_test_entropy(y + scaled), tolerance = 1e-14)
    expect_equal(evaluated$balance, mean(scaled), tolerance = 1e-14)
  }
  noGradient <- FederatedLearning:::logisticObjectiveGradientCpp(
    FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE)), beta, y,
    computeGradient = FALSE)
  expect_null(noGradient$gradient)
  expect_equal(noGradient$loss, actual$loss)
})

test_that("class balancing constructs a dual-feasible bound with unequal or single-class sites", {
  x <- cbind(1, c(-3, -1, 0, 1, 4, 5), c(0, 0, 1, 0, 1, 1))
  y <- c(0, 0, 0, 0, 1, 1)
  beta <- c(-0.7, 0.2, -0.1)
  lambda <- 0.02
  ids <- list(1:4, 5:6)
  pieces <- lapply(ids, function(i) gap_test_summary(x[i, , drop = FALSE], y[i], beta))
  pooled <- gap_test_summary(x, y, beta)
  for (intercept in c(TRUE, FALSE)) {
    for (aggregation in c("sampleSize", "equalClient")) {
      request <- FederatedLearning:::.dualGapRequest(pieces, lambda, intercept, aggregation, 1L)
      if (aggregation == "sampleSize") {
        centralRequest <- FederatedLearning:::.dualGapRequest(list(pooled), lambda, intercept, aggregation, 1L)
        expect_equal(request, centralRequest, tolerance = 1e-12)
      }
      weights <- if (aggregation == "sampleSize") c(4, 2) / 6 else c(0.5, 0.5)
      r <- as.numeric(pooled$dualResidual) * request$scales[y + 1L]
      q <- y + r
      observationWeights <- c(rep(weights[1] / 4, 4), rep(weights[2] / 2, 2))
      expect_true(all(q >= 0 & q <= 1))
      if (intercept) expect_lt(abs(sum(observationWeights * r)), 1e-14)
      penalized <- if (intercept) 2:3 else 1:3
      score <- as.numeric(crossprod(x, observationWeights * r))
      expect_lte(max(abs(score[penalized])), lambda * (1 + 1e-12))
      expect_equal(request$gradientMaxAbs, max(abs(score[penalized])), tolerance = 1e-12)
      for (i in seq_along(pieces)) {
        e <- FederatedLearning:::logisticDualEntropyCpp(pieces[[i]]$dualResidual, y[ids[[i]]], request$scales)
        e$round <- 1L
        pieces[[i]]$dualEvaluation <- e
      }
      value <- FederatedLearning:::.dualGapEvaluation(pieces, request, lambda, intercept, aggregation)
      expected <- sum(weights * vapply(ids, function(i) gap_test_entropy(q[i]), numeric(1)))
      expect_equal(value$objective, expected, tolerance = 1e-12)
      # Weak duality must hold at other primal iterates as well, not just the source iterate.
      for (w in list(beta, c(0, 0, 0), c(0.5, 2, -1))) {
        losses <- vapply(ids, function(i) {
          FederatedLearning:::logisticNegLogLik(w, x[i, , drop = FALSE], y[i], meanLoss = TRUE)
        }, numeric(1))
        primal <- sum(weights * losses) + lambda * sum(abs(w[penalized]))
        expect_gte(primal - value$objective, -1e-12)
      }
    }
  }
})

test_that("duality gap vanishes at a nontrivial analytic logistic-lasso solution", {
  x <- cbind(1, rep(c(-1, 1), each = 8))
  y <- c(1, rep(0, 7), 0, rep(1, 7))
  lambda <- 0.05
  optimum <- c(0, qlogis(7 / 8 - lambda))
  optimumValue <- FederatedLearning:::logisticNegLogLik(optimum, x, y, meanLoss = TRUE) +
    lambda * abs(optimum[2])
  for (beta in list(optimum, optimum + c(0.01, -0.01), c(-1, 0), c(1, 2))) {
    stats <- gap_test_summary(x, y, beta)
    request <- FederatedLearning:::.dualGapRequest(list(stats), lambda, TRUE, "sampleSize", 1L)
    evaluated <- FederatedLearning:::logisticDualEntropyCpp(stats$dualResidual, y, request$scales)
    primal <- stats$loss / length(y) + lambda * abs(beta[2])
    gap <- FederatedLearning:::.checkedDualityGap(primal, evaluated$entropy)
    expect_lte(evaluated$entropy, optimumValue + 1e-12)
    expect_gte(gap + 1e-12, primal - optimumValue)
    if (identical(beta, optimum)) expect_lt(gap, 1e-12)
  }
})

test_that("dual constraints remain feasible with large class totals and a small penalty", {
  # At this exact solution class gradients nearly cancel; naive large-row sums
  # can exceed lambda after scaling even though the summary norm appears valid.
  group <- rep(c(-1, 1), each = 10000)
  y <- c(rep(1, 100), rep(0, 9900), rep(1, 200), rep(0, 9800))
  lambda <- 1e-7
  probabilities <- c(0.01 + lambda, 0.02 - lambda)
  optimum <- c(mean(qlogis(probabilities)), diff(qlogis(probabilities)) / 2)
  compensatedSum <- function(values) {
    total <- 0
    correction <- 0
    for (value in values) {
      adjusted <- value - correction
      nextTotal <- total + adjusted
      correction <- (nextTotal - total) - adjusted
      total <- nextTotal
    }
    total
  }
  for (repeatCount in c(1L, 4L)) {
    x <- cbind(1, rep(group, repeatCount))
    labels <- rep(y, repeatCount)
    stats <- gap_test_summary(x, labels, optimum)
    eta <- as.numeric(x %*% optimum)
    residual <- ifelse(labels == 0, plogis(eta), -plogis(-eta))
    request <- FederatedLearning:::.dualGapRequest(list(stats), lambda, TRUE, "sampleSize", 1L)
    r <- residual * request$scales[labels + 1L]
    # Use an independent Kahan sum: long double is only double on macOS ARM.
    score <- vapply(seq_len(ncol(x)), function(j) {
      compensatedSum(x[, j] * r) / length(r)
    }, numeric(1))
    expect_lt(abs(score[1]), 1e-14)
    expect_lte(abs(score[2]), lambda)
    expect_lte(request$gradientMaxAbs, lambda)
    entropy <- FederatedLearning:::logisticDualEntropyCpp(stats$dualResidual,
      labels, request$scales)$entropy
    primal <- stats$loss / length(labels) + lambda * abs(optimum[2])
    expect_gte(primal - entropy, -1e-12)
    expect_lt(primal - entropy, 1e-8)
  }
})

test_that("dual diagnostics handle extreme logits, empty classes, and intercept-only fits", {
  x <- matrix(1, nrow = 5, ncol = 1)
  y <- c(0, 0, 0, 1, 1)
  for (beta in c(-1000, 1000, qlogis(mean(y)))) {
    stats <- gap_test_summary(x, y, beta)
    request <- FederatedLearning:::.dualGapRequest(list(stats), 0.1, TRUE, "sampleSize", 1L)
    evaluated <- FederatedLearning:::logisticDualEntropyCpp(stats$dualResidual, y, request$scales)
    gap <- FederatedLearning:::.checkedDualityGap(stats$loss / length(y), evaluated$entropy)
    expect_true(is.finite(gap))
    expect_gte(gap, 0)
    expect_lt(abs(evaluated$balance), 1e-14)
    if (beta == qlogis(mean(y))) expect_lt(gap, 1e-12)
  }
  for (label in c(0, 1)) {
    stats <- gap_test_summary(x, rep(label, 5), 0)
    request <- FederatedLearning:::.dualGapRequest(list(stats), 0.1, TRUE, "sampleSize", 1L)
    expect_equal(request$scales, c(0, 0))
    expect_equal(FederatedLearning:::logisticDualEntropyCpp(stats$dualResidual,
      rep(label, 5), request$scales)$entropy, 0)
  }
  tiny <- gap_test_summary(matrix(1, 1, 1), 1, 100)
  expect_lt(tiny$dualResidual[1], 0)
  expect_equal(tiny$dualResidual[1], -plogis(-100), tolerance = 1e-14)
  expect_gt(FederatedLearning:::logisticDualEntropyCpp(tiny$dualResidual, 1, c(1, 1))$entropy, 0)
  expect_error(gap_test_summary(x, c(0, 0, 0, 0.5, 1), 0), "binary")
  expect_error(FederatedLearning:::logisticDualEntropyCpp(-0.1, 0, c(1, 1)), "domain")
  expect_error(FederatedLearning:::logisticDualEntropyCpp(0.1, 0, c(2, 1)), "scales")
  expect_error(FederatedLearning:::logisticDualEntropyCpp(NA_real_, 0, c(1, 1)), "finite")
  expect_error(FederatedLearning:::.checkedDualityGap(0.2, 0.3), "Negative duality gap")
  expect_equal(FederatedLearning:::.checkedDualityGap(0.2, 0.2 + .Machine$double.eps), 0)
})

test_that("dual feasibility and round mismatches fail loudly", {
  request <- list(round = 1L, gradientMaxAbs = 0.01)
  report <- list(n = 2L, dualEvaluation = list(round = 1L, entropy = 0.2, balance = 0))
  evaluate <- function(reports = list(report), r = request) {
    FederatedLearning:::.dualGapEvaluation(reports, r, 0.1, TRUE, "sampleSize")
  }
  expect_error(evaluate(r = modifyList(request, list(round = 2L))), "round mismatch")
  expect_error(evaluate(r = modifyList(request, list(gradientMaxAbs = 0.2))), "feasibility")
  bad <- report
  bad$dualEvaluation$balance <- 1e-4
  expect_error(evaluate(list(bad)), "feasibility")
  bad$dualEvaluation$balance <- 0
  bad$dualEvaluation$entropy <- 1
  expect_error(evaluate(list(bad)), "feasibility")
  base <- list(dualAvgGapDiagnostics = TRUE, lambda = 0.1)
  expect_error(fitFederated(NULL, "ODAL", base), "only for DualAvg")
  expect_error(fitFederated(NULL, "DualAvg", modifyList(base, list(lambda = 0))), "positive finite lambda")
  expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
    list(convergenceObjective = "none"))), "objective monitoring")
  expect_error(fitFederated(NULL, "DualAvg", modifyList(base,
    list(dualAvgGapCheckEvery = 0.5))), "positive integer")
})

test_that("pipelined dual diagnostics do not change updates, stopping, or communication calls", {
  skip_on_cran()
  testthat::local_mocked_bindings(clusterCreateMatrices = function(cl, config) NULL,
    .package = "FederatedLearning")
  cl <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  data <- list(list(xMatrix = cbind(1, c(-1, 0, 1)), yLabels = c(0, 0, 1)),
    list(xMatrix = cbind(1, c(-2, -1, 0, 1, 2)), yLabels = c(0, 0, 1, 1, 1)))
  parallel::clusterApply(cl, data, function(d) {
    library(FederatedLearning)
    d$xMatrix <- methods::as(Matrix::Matrix(d$xMatrix, sparse = TRUE), "dgCMatrix")
    d$n <- length(d$yLabels)
    assign("clientData", d, envir = .GlobalEnv)
    NULL
  })
  originalEval <- parallel::clusterEvalQ
  originalExport <- parallel::clusterExport
  calls <- c(evaluate = 0L, export = 0L)
  testthat::local_mocked_bindings(
    clusterEvalQ = function(cl, expr) {
      calls[["evaluate"]] <<- calls[["evaluate"]] + 1L
      call <- match.call()
      call[[1]] <- originalEval
      result <- eval(call, parent.frame())
      # No row-level residual is allowed into any objective reply.
      if (grepl("getLocalConvergenceObjective", paste(deparse(substitute(expr)), collapse = ""))) {
        for (reply in result) expect_null(reply$dualResidual)
      }
      result
    },
    clusterExport = function(cl, varlist, envir = .GlobalEnv) {
      calls[["export"]] <<- calls[["export"]] + 1L
      originalExport(cl, varlist, envir)
    }, .package = "parallel")
  base <- list(mapping = data.frame(covariateId = 1, columnId = 1), p = 1L,
    intercept = TRUE, rounds = 10L, k = 2L, etaClient = 0.1, etaServer = 1,
    lambda = 0.01, epsilon = 0, convergenceObjective = "cyclopsGradient")
  plain <- fitFederated(cl, "DualAvg", base, verbose = FALSE)
  plainCalls <- calls
  calls[] <- 0L
  diagnostic <- fitFederated(cl, "DualAvg", modifyList(base,
    list(dualAvgGapDiagnostics = TRUE, dualAvgGapCheckEvery = 3L)), verbose = FALSE)
  expect_equal(calls, plainCalls)
  expect_equal(diagnostic$w, plain$w, tolerance = 1e-14)
  expect_equal(diagnostic$z, plain$z, tolerance = 1e-14)
  expect_equal(diagnostic$roundsCompleted, plain$roundsCompleted)
  expect_equal(diagnostic$dualGapChecks, 4L)
  expect_equal(diagnostic$dualGapHistory$round, c(2L, 4L, 7L, 10L))
  expect_equal(diagnostic$dualGapHistory$candidateRound, c(1L, 3L, 6L, 9L))
  expect_true(all(diff(diagnostic$dualGapHistory$dualLowerBound) >= 0))
  expect_true(all(diagnostic$dualGapHistory$dualityGap >= 0))
  expect_equal(diagnostic$dualityGap, diagnostic$primalObjective - diagnostic$dualLowerBound)
  # This fit reused the same workers, but each candidate is still specific to this fit.
  for (aggregation in c("sampleSize", "equalClient")) {
    fit <- fitFederated(cl, "DualAvg", modifyList(base, list(rounds = 1L,
      aggregation = aggregation, dualAvgGapDiagnostics = TRUE)), verbose = FALSE)
    losses <- vapply(data, function(d) {
      FederatedLearning:::logisticNegLogLik(fit$w, d$xMatrix, d$yLabels, meanLoss = TRUE)
    }, numeric(1))
    weights <- if (aggregation == "sampleSize") c(3, 5) / 8 else c(0.5, 0.5)
    expect_equal(fit$primalObjective, sum(losses * weights) + base$lambda * abs(fit$w[2]))
    expect_equal(fit$dualGapChecks, 0L)
    expect_equal(fit$dualLowerBound, 0)
  }
  for (objective in c("negLogLikelihood", "cyclopsGradient")) {
    cfg <- modifyList(base, list(epsilon = 100, convergenceObjective = objective))
    reference <- fitFederated(cl, "DualAvg", cfg, verbose = FALSE)
    fit <- fitFederated(cl, "DualAvg", modifyList(cfg,
      list(dualAvgGapDiagnostics = TRUE, dualAvgGapCheckEvery = 1L)), verbose = FALSE)
    expect_equal(fit$roundsCompleted, reference$roundsCompleted)
    expect_equal(fit$w, reference$w, tolerance = 1e-14)
    expect_equal(fit$dualGapHistory$round[nrow(fit$dualGapHistory)], fit$roundsCompleted)
  }
  guarded <- fitFederated(cl, "DualAvg", modifyList(base, list(clientFrac = 0.5,
    clientSampleSeed = 1L, dualAvgKktTolerance = 1e-12, dualAvgKktCheckEvery = 3L,
    dualAvgGapDiagnostics = TRUE, dualAvgGapCheckEvery = 3L, pooledDiagnostics = TRUE)),
    verbose = FALSE)
  expect_equal(guarded$roundsCompleted, 10L)
  expect_equal(guarded$dualGapChecks, 4L)
  expect_true(is.finite(guarded$dualityGap))
  expect_equal(guarded$primalObjective, guarded$pooledMeanLogLoss + base$lambda * abs(guarded$w[2]))
})
