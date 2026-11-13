# Socket workers load the installed package, so run against an installed build.
adapEigenSites <- function(cl, p = 30L) {
  invisible(parallel::clusterApply(cl, seq_along(cl), function(i, p) {
    set.seed(100 + i)
    n <- c(3000L, 2000L, 1500L)[i]
    x <- Matrix::rsparsematrix(n, p, density = 0.1, rand.x = function(k) rep(1, k))
    y <- stats::rbinom(n, 1, stats::plogis(-2 + as.numeric(x[, 1:5] %*% c(0.8, -0.5, 0.6, 0.4, -0.3))))
    xm <- methods::as(cbind(Matrix::Matrix(1, n, 1, sparse = TRUE), x), "CsparseMatrix")
    assign("clientData", list(xMatrix = xm, yLabels = y, n = n, rowId = seq_len(n)), envir = .GlobalEnv)
    NULL
  }, p))
}

adapEigenConfig <- function(p = 30L) {
  list(mapping = data.frame(covariateId = seq_len(p), columnId = seq_len(p)), p = p, intercept = TRUE,
    epsilon = 1e-10, rounds = 3L, foldsK = 3L, cvStratify = TRUE, cvSeed = 42L, maxOuter = 500L,
    maxInner = 100L, lambdaSearch = "optimize", lambdaSearchMaxEvals = 8L, adapCvDiagnostics = TRUE,
    adapInitBackend = "glmnet_sparse", adapInitFolds = 3L, adapInitStratify = TRUE)
}

eigenColumns <- function(diagnostics) {
  if (!is.data.frame(diagnostics)) return(numeric())
  unlist(diagnostics[grepl("EigenMin$", names(diagnostics))], use.names = FALSE)
}

test_that("limiting ADAP's eigendecomposition diagnostics leaves the fits unchanged", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  cl <- clusterInit(rep("localhost", 3), paste0("site", 1:3), mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  adapEigenSites(cl)
  local_mocked_bindings(clusterCreateMatrices = function(...) NULL, .package = "FederatedLearning")
  for (method in c("ADAP1", "ADAP2")) {
    fits <- lapply(c(all = "all", final = "final", none = "none"), function(mode) {
      fitFederated(cl, method, c(adapEigenConfig(), list(adapEigenDiagnostics = mode)), verbose = FALSE)
    })
    expect_identical(fits$final$w, fits$all$w)
    expect_identical(fits$none$w, fits$all$w)
    expect_identical(fits$final$selectedLambda, fits$all$selectedLambda)
    expect_identical(fits$none$cvScores, fits$all$cvScores)
    finalEigen <- function(fit) fit$adapFinalDiagnostics$completeHessianEigenMin
    expect_true(is.finite(finalEigen(fits$all)))
    expect_equal(finalEigen(fits$final), finalEigen(fits$all))
    expect_true(is.na(finalEigen(fits$none)))
    expect_true(any(is.finite(eigenColumns(fits$all$adapCvDiagnostics))))
    expect_false(any(is.finite(eigenColumns(fits$final$adapCvDiagnostics))))
    if (identical(method, "ADAP2")) {
      expect_true(is.finite(fits$all$siteHessianEigenMin))
      expect_true(is.na(fits$final$siteHessianEigenMin))
      expect_equal(fits$final$correctionEigenMin, fits$all$correctionEigenMin)
      expect_identical(fits$final$curvatureStatus, fits$all$curvatureStatus)
      expect_true(is.na(fits$none$correctionEigenMin))
      expect_identical(fits$none$curvatureStatus, "unknown")
    }
  }
  # Where eigenvalues decide the fit, they cannot be skipped.
  expect_error(fitFederated(cl, "ADAP2", c(adapEigenConfig(),
    list(adapEigenDiagnostics = "final", adapCurvatureAction = "fail")), verbose = FALSE), "diagnostics only")
  expect_error(fitFederated(cl, "MaxConv-ADAP", c(adapEigenConfig(), list(adapEigenDiagnostics = "none")),
    verbose = FALSE), "diagnostics only")
})

test_that("the runner accepts adapEigenDiagnostics as a method option", {
  expect_s3_class(createMethodSettings("ADAP2", adapEigenDiagnostics = "final"), "methodSettings")
  expect_error(createMethodSettings("ADAP2", adapEigenDiagnostic = "final"), "Unknown method option")
})
