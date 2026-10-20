curvaturePolicyFixture <- function() {
  x <- Matrix::Matrix(cbind(1, rep(c(-1, 0, 1), each = 40)), sparse = TRUE)
  y <- rep(c(0, 1, 0, 1, 1, 1, 0, 0), 15)
  bar <- c(0, 0)
  list(x = x, y = y, bar = bar,
    g = .logisticNegGradient(bar, x, y), h = .logisticNegHessian(bar, x))
}

test_that("report-only preserves positive-correction fits across numerical paths", {
  f <- curvaturePolicyFixture()
  for (sparse in c(FALSE, TRUE)) for (trace in c(FALSE, TRUE)) {
    args <- list(xDesign = if (sparse) f$x else as.matrix(f$x), y = f$y,
      betaLead = f$bar, betaBar = f$bar, globalGrad = f$g,
      globalHess = f$h + diag(0.02, 2), lambda = 0.01, returnDetails = TRUE,
      traceDiagnostics = trace, tol = 1e-7)
    unchanged <- do.call(.fitPdaAdapSurrogate, args)
    expect_identical(do.call(.fitPdaAdapSurrogate, c(args, list(curvatureAction = "report"))), unchanged)
  }
})

test_that("report-only skips the correction gate but retains numerical checks", {
  f <- curvaturePolicyFixture()
  args <- list(xDesign = f$x, y = f$y, betaLead = f$bar, betaBar = f$bar,
    globalGrad = f$g, globalHess = f$h - diag(0.01, 2), lambda = 0.01,
    returnDetails = TRUE, tol = 1e-7)
  expect_identical(do.call(.fitPdaAdapSurrogate, args)$failureReason, "negative_C_eigenvalue")
  fit <- do.call(.fitPdaAdapSurrogate, c(args, list(curvatureAction = "report")))
  expect_false(.adapFitFailed(fit))
  expect_lt(fit$C_eigen_min, 0)
  expect_equal(fit$rho, 0)
  args$globalHess <- -diag(1, 2)
  failed <- do.call(.fitPdaAdapSurrogate, c(args, list(curvatureAction = "report")))
  expect_true(.adapFitFailed(failed))
  expect_identical(failed$failureReason, "non_positive_coordinate_curvature")
  expect_error(do.call(.fitPdaAdapSurrogate, c(args, list(curvatureAction = "ignore"))), "arg")
  expect_error(do.call(.fitPdaAdapSurrogate, c(args,
    list(curvatureAction = "report", strictCorrection = "prox"))), "only for the exact")
})

test_that("exact ADAP2 reports curvature by default and captures the requested policy", {
  f <- curvaturePolicyFixture()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(FederatedLearning.localId = 1L)
  on.exit(options(old), add = TRUE)
  state <- list(phase = 2L, leadIndex = 1L, betaBar = f$bar, betaLead = f$bar,
    globalGrad = f$g, globalHess = f$h - diag(0.01, 2), totalN = 10 * length(f$y),
    lambdaSeq = c(0.2, 0.1, 0.01), adapMethodName = "ADAP2", adapSurrogateVariant = "exact")
  data <- list(xMatrix = f$x, yLabels = f$y, n = length(f$y), rowId = seq_along(f$y) + 1000L)
  config <- list(intercept = TRUE, foldsK = 5L, cvSeed = 42L, lambdaSearch = "optimize",
    lambdaSearchMaxEvals = 5L, adapCvDiagnostics = TRUE, maxOuter = 100L,
    maxInner = 100L, tol = 1e-7, adapCurvatureAction = "fail")
  expect_error(.clientUpdatePdaAdap(data, state, config), "lambda CV failed")
  config$adapCurvatureAction <- "report"
  uncaptured <- .clientUpdatePdaAdap(data, state, config)
  config$adapCurvatureAction <- NULL
  expect_identical(.clientUpdatePdaAdap(data, state, config), uncaptured)
  config$adapCurvatureAction <- "report"
  config$surrogateCaptureDirectory <- directory
  config <- .prepareSurrogateCapture(config, "ADAP2")
  captured <- .clientUpdatePdaAdap(data, state, config)
  expect_identical(captured, uncaptured)
  expect_true(all(is.finite(captured$w)))
  expect_true(all(captured$cvValid))
  files <- list.files(config$surrogateCaptureDirectory, "^input-", full.names = TRUE)
  inputs <- lapply(files, readRDS)
  expect_equal(length(inputs), 5L * length(captured$lambdaSeq) + 1L)
  expect_true(all(vapply(inputs, function(x) identical(x$settings$curvatureAction, "report"), logical(1))))
  expect_equal(sum(vapply(inputs, function(x) x$context$phase == "final", logical(1))), 1L)
  expect_equal(sum(vapply(inputs, function(x) x$context$phase == "cv", logical(1))), length(inputs) - 1L)
  final <- inputs[[which(vapply(inputs, function(x) x$context$phase == "final", logical(1)))]]
  expect_equal(final$inputs$quadraticCorrection, -diag(0.01, 2))
  expect_equal(final$inputs$lambda, captured$selectedLambda)
  expect_identical(final$inputs$betaBar, state$betaBar)
  config$adapSolveStyle <- "pda"
  config$surrogateCaptureDirectory <- NULL
  expect_error(.clientUpdatePdaAdap(data, state, config), "requires the exact")
  config$adapCurvatureAction <- NULL
  expect_no_error(.clientUpdatePdaAdap(data, state, config))
})

test_that("runner accepts an explicit policy without changing absent defaults", {
  path <- test_path("..", "..", "extras", "runComparisonMatrix.R")
  if (!file.exists(path)) skip("Extras scripts unavailable")
  runner <- new.env(parent = globalenv())
  sys.source(path, runner)
  expect_null(runner$methodConfig("ADAP2", "ageSex", list())$adapCurvatureAction)
  args <- list("adap-curvature-action" = "report")
  expect_identical(runner$methodConfig("ADAP2", "ageSex", args)$adapCurvatureAction, "report")
  expect_null(runner$methodConfig("ODAL2", "ageSex", args)$adapCurvatureAction)
  expect_error(runner$methodConfig("ADAP2", "ageSex", list("adap-curvature-action" = "ignore")), "arg")
})
