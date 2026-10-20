fitStatusFixture <- function(seed = 7L) {
  set.seed(seed)
  n <- 200L
  x <- cbind(1, matrix(stats::rnorm(n * 3), n, 3))
  y <- stats::rbinom(n, 1, stats::plogis(as.numeric(x %*% c(-1, 0.8, -0.5, 0.3))))
  bar <- c(-0.8, 0.5, -0.3, 0.1)
  list(x = Matrix::Matrix(x, sparse = TRUE), y = y, bar = bar,
    g = .logisticNegGradient(bar, x, y), h = .logisticNegHessian(bar, x))
}

minEigen <- function(m) min(eigen((m + t(m)) / 2, symmetric = TRUE, only.values = TRUE)$values)

test_that("exact ADAP reports complete surrogate curvature at the returned point", {
  f <- fitStatusFixture()
  correction <- -diag(c(0, 0.02, 0.01, 0))
  for (sparse in c(FALSE, TRUE)) {
    x <- if (sparse) f$x else as.matrix(f$x)
    fit <- .fitPdaAdapSurrogate(xDesign = x, y = f$y, betaLead = f$bar, betaBar = f$bar,
      globalGrad = f$g, globalHess = f$h + correction, lambda = 0.01, returnDetails = TRUE,
      tol = 1e-8, curvatureAction = "report")
    expect_false(.adapFitFailed(fit))
    complete <- .logisticNegHessian(fit$beta, x) + correction
    expect_equal(fit$completeHessianEigenMin, minEigen(complete), tolerance = 1e-10)
    expect_equal(.adapFitDiagnostics(fit)[["completeHessianEigenMin"]], fit$completeHessianEigenMin)
    expect_equal(.adapFitDiagnosticsForReport(fit)$completeHessianEigenMin, fit$completeHessianEigenMin)
  }
  rejected <- .fitPdaAdapSurrogate(xDesign = f$x, y = f$y, betaLead = f$bar, betaBar = f$bar,
    globalGrad = f$g, globalHess = f$h + correction, lambda = 0.01, returnDetails = TRUE)
  expect_identical(rejected$failureReason, "negative_C_eigenvalue")
  expect_true(is.na(.adapFitDiagnostics(rejected)[["completeHessianEigenMin"]]))
})

test_that("ODAL2 reports stationarity and complete curvature at the returned point", {
  f <- fitStatusFixture()
  old <- options(FederatedLearning.localId = 1L)
  on.exit(options(old), add = TRUE)
  otherGrad <- f$g + c(0.01, -0.02, 0, 0.005)
  otherHess <- f$h - diag(c(0, 0.01, 0, 0))
  broadcast <- list(phase = 2L, leadIndex = 1L, betaBar = f$bar, otherGrad = otherGrad,
    otherHess = otherHess, odalVariant = "second")
  data <- list(xMatrix = f$x, yLabels = f$y, n = length(f$y))
  out <- .clientUpdateODAL(data, broadcast, list(intercept = TRUE, optimMaxit = 1000L))
  quadratic <- otherHess - f$h
  gradient <- .logisticNegGradient(out$w, f$x, f$y) + otherGrad - f$g +
    as.numeric(quadratic %*% (out$w - f$bar))
  expect_equal(out$returnGradientMaxAbs, max(abs(gradient)), tolerance = 1e-10)
  expect_lt(out$returnGradientMaxAbs, 1e-3)
  expect_equal(out$returnHessianEigenMin,
    minEigen(.logisticNegHessian(out$w, f$x) + quadratic), tolerance = 1e-10)
  expect_equal(out$returnBetaMaxAbs, max(abs(out$w)))
  expect_equal(out$returnEtaMaxAbs, max(abs(as.numeric(f$x %*% out$w))))
  expect_null(out$beta)

  state <- .serverInitODAL2(list(p = 3L))
  state$phase <- 2L
  state$leadIndex <- 1L
  report <- .serverRoundODAL(state, list(out), list())$report
  expect_identical(report$odalGradientMaxAbs, out$returnGradientMaxAbs)
  expect_identical(report$odalCompleteHessianEigenMin, out$returnHessianEigenMin)
  expect_identical(report$odalBetaMaxAbs, out$returnBetaMaxAbs)
  expect_identical(report$odalEtaMaxAbs, out$returnEtaMaxAbs)
})

test_that("ODAL return diagnostics degrade to missing values instead of failing", {
  f <- fitStatusFixture()
  bad <- .odalReturnDiagnostics(c(NA, 0, 0, 0), f$bar, f$x, f$y, rep(0, 4), diag(4))
  expect_true(all(vapply(bad, function(v) length(v) == 1L && !is.finite(v), logical(1))))
})
