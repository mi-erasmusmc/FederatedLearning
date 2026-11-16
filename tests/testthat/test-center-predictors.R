test_that("restored coefficients give the same linear predictor as the reparametrized fit", {
  set.seed(7)
  n <- 50
  x <- cbind(1, age = 0.75 + 0.08 * rnorm(n), af = rbinom(n, 1, 0.95), b = rbinom(n, 1, 0.2))
  transform <- list(center = 2L, centerValues = mean(x[, 2]), flip = 3L)
  reparametrized <- x
  reparametrized[, 2] <- x[, 2] - transform$centerValues
  reparametrized[, 3] <- 1 - x[, 3]
  w <- c(-2, 1.5, -0.4, 0.3)
  restored <- FederatedLearning:::.restoreReparametrizedCoefficients(w, transform)
  expect_equal(as.numeric(x %*% restored), as.numeric(reparametrized %*% w), tolerance = 1e-12)
  expect_equal(abs(restored[-1]), abs(w[-1]))
  expect_identical(FederatedLearning:::.restoreReparametrizedCoefficients(w,
    list(center = integer(), centerValues = numeric(), flip = integer())), w)
})

# Socket workers load the installed package, so run against an installed build.
centerSites <- function(cl) {
  invisible(parallel::clusterApply(cl, seq_along(cl), function(i) {
    set.seed(300 + i)
    n <- c(3000L, 2000L, 1500L)[i]
    age <- 0.75 + 0.08 * stats::rnorm(n)
    af <- stats::rbinom(n, 1, 0.96)
    b <- matrix(stats::rbinom(n * 3, 1, 0.2), n, 3)
    y <- stats::rbinom(n, 1, stats::plogis(-6 + 4 * age - 0.3 * af + as.numeric(b %*% c(0.6, -0.4, 0))))
    xm <- methods::as(Matrix::Matrix(cbind(1, age, af, b), sparse = TRUE), "CsparseMatrix")
    assign("clientData", list(xMatrix = xm, yLabels = y, n = n, rowId = seq_len(n)), envir = .GlobalEnv)
    NULL
  }))
}

test_that("DualAvg with centred predictors reaches the pooled lasso and restores the sites", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  cl <- clusterInit(rep("localhost", 3), paste0("site", 1:3), mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  centerSites(cl)
  local_mocked_bindings(clusterCreateMatrices = function(...) NULL, .package = "FederatedLearning")
  before <- parallel::clusterEvalQ(cl, clientData$xMatrix)
  lambda <- 0.002
  config <- list(mapping = data.frame(covariateId = 1:5, columnId = 1:5), p = 5L, intercept = TRUE,
    rounds = 1000L, k = 1L, etaClient = 1, etaServer = 1, lambda = lambda, epsilon = 0)
  plain <- fitFederated(cl, "DualAvg", config, verbose = FALSE)
  centred <- fitFederated(cl, "DualAvg", c(config, list(centerPredictors = TRUE)), verbose = FALSE)

  after <- parallel::clusterEvalQ(cl, clientData$xMatrix)
  expect_identical(after, before)
  expect_false(any(unlist(parallel::clusterEvalQ(cl, exists(".reparametrizedOriginalX", envir = .GlobalEnv)))))
  expect_identical(centred$reparametrization$center, 2L)
  expect_identical(centred$reparametrization$flip, 3L)
  expect_true("design summary" %in% centred$communication$exchange)

  pooled <- do.call(rbind, lapply(before, as.matrix))
  y <- unlist(parallel::clusterEvalQ(cl, clientData$yLabels))
  reference <- glmnet::glmnet(pooled[, -1], y, family = "binomial", lambda = lambda, standardize = FALSE,
    control = list(thresh = 1e-14, maxit = 1e6))
  wReference <- as.numeric(stats::coef(reference))
  objective <- function(w) {
    eta <- as.numeric(pooled %*% w)
    mean(log1p(exp(eta)) - y * eta) + lambda * sum(abs(w[-1]))
  }
  # Same objective and rounds: the centred fit is at the pooled lasso, the plain one still short
  # along the age-intercept direction.
  gap <- function(w) objective(w) - objective(wReference)
  expect_lt(gap(centred$w), 1e-6)
  expect_gt(gap(plain$w), 100 * max(gap(centred$w), 1e-9))
  expect_lt(max(abs(centred$w - wReference)), 0.01)
  expect_error(fitFederated(cl, "ADAP2", list(mapping = config$mapping, p = 5L, intercept = TRUE,
    centerPredictors = TRUE), verbose = FALSE), "only for DualAvg")
})

test_that("the runner passes centerPredictors to DualAvg only", {
  expect_true(methodConfig("DualAvg", "all", list(`center-predictors` = "true"))$centerPredictors)
  expect_null(methodConfig("ADAP2", "all", list(`center-predictors` = "true"))$centerPredictors)
  expect_s3_class(createMethodSettings("DualAvg", centerPredictors = TRUE), "methodSettings")
})
