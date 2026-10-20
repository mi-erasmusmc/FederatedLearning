calibrationSample <- function(n = 20000L, seed = 41L) {
  set.seed(seed)
  eta <- stats::rnorm(n, -1.5, 1)
  list(eta = eta, y = stats::rbinom(n, 1, stats::plogis(eta)))
}

test_that("calibration measures recover known miscalibration", {
  s <- calibrationSample()
  calibrated <- .calibrationMetrics(s$y, s$eta)
  expect_lt(abs(calibrated[["calibrationInTheLarge"]]), 0.05)
  expect_lt(abs(calibrated[["calibrationSlope"]] - 1), 0.05)
  # Predicting one logit too high needs an offset intercept of about -1.
  expect_lt(abs(.calibrationMetrics(s$y, s$eta + 1)[["calibrationInTheLarge"]] + 1), 0.05)
  # Doubling the logits gives a slope of about one half.
  expect_lt(abs(.calibrationMetrics(s$y, 2 * s$eta)[["calibrationSlope"]] - 0.5), 0.03)
  direct <- stats::glm(s$y ~ 1 + stats::offset(s$eta), family = stats::binomial())
  expect_equal(calibrated[["calibrationInTheLarge"]], unname(stats::coef(direct)[1]), tolerance = 1e-10)
  free <- stats::glm(s$y ~ s$eta, family = stats::binomial())
  expect_equal(unname(calibrated[c("calibrationIntercept", "calibrationSlope")]),
    unname(stats::coef(free)), tolerance = 1e-10)
  expect_true(all(is.na(.calibrationMetrics(rep(0, 5), 1:5))))
})

test_that("single models use their linear predictor, not clipped probabilities", {
  set.seed(5)
  eta <- c(stats::rnorm(200, -2), 45, -45)
  # Mispredicted extremes are where clipping at 1e-15 distorts the logits.
  y <- c(stats::rbinom(200, 1, stats::plogis(eta[1:200])), 0, 1)
  row <- evaluatePredictions(list(yLabels = y), stats::plogis(eta), density = 1, clientId = "s",
    clientIndex = 1L, linearPredictor = eta)
  expect_equal(row$logLoss, binaryLogLoss(eta, y), tolerance = 1e-12)
  expect_equal(unlist(row[c("calibrationInTheLarge", "calibrationIntercept", "calibrationSlope")]),
    .calibrationMetrics(y, eta), tolerance = 1e-12, ignore_attr = TRUE)
  clipped <- evaluatePredictions(list(yLabels = y), stats::plogis(eta), density = 1, clientId = "s",
    clientIndex = 1L)
  expect_false(isTRUE(all.equal(row$calibrationSlope, clipped$calibrationSlope, tolerance = 1e-8)))
  expect_gt(row$logLoss, clipped$logLoss)
})

test_that("the smooth calibration curve and ICI recover known miscalibration", {
  s <- calibrationSample()
  calibrated <- .calibrationCurve(s$y, s$eta)
  curve <- calibrated$curve
  expect_identical(nrow(curve), 50L)
  expect_true(all(diff(curve$predicted) > 0))
  expect_true(all(curve$lower <= curve$observed & curve$observed <= curve$upper))
  expect_gte(mean(curve$lower <= curve$predicted & curve$predicted <= curve$upper), 0.9)
  expect_lt(calibrated$ici, 0.01)
  # Predicting one logit too high: observed risk is the logistic of one logit less.
  shifted <- .calibrationCurve(s$y, s$eta + 1)
  expect_lt(max(abs(shifted$curve$observed - stats::plogis(stats::qlogis(shifted$curve$predicted) - 1))), 0.03)
  expect_gt(shifted$ici, 5 * calibrated$ici)
  # The ICI averages |predicted - smoothed observed| over all patients.
  fit <- mgcv::bam(y ~ s(lp, bs = "cr", k = 24), data = data.frame(y = s$y, lp = s$eta + 1),
    family = stats::binomial(), method = "fREML", discrete = TRUE, select = TRUE, gamma = 1.2)
  expect_equal(shifted$ici, mean(abs(stats::plogis(s$eta + 1) - stats::fitted(fit))), tolerance = 1e-8)
})

test_that("the calibration curve is not estimated without enough distinct predictions or outcomes", {
  none <- list(curve = NULL, ici = NA_real_)
  expect_identical(.calibrationCurve(rep(0:1, 50), rep(c(-1, 0, 1), length.out = 100)), none)
  expect_identical(.calibrationCurve(rep(0, 100), stats::rnorm(100)), none)
  expect_identical(.calibrationCurve(c(0, 1), c(-1, 1)), none)
  row <- evaluatePredictions(list(yLabels = rep(0:1, 50)), rep(0.4, 100), density = 1, clientId = "s",
    clientIndex = 1L, linearPredictor = rep(stats::qlogis(0.4), 100))
  expect_true(is.na(row$calibrationIci))
  expect_null(attr(row, "calibrationCurve"))
})

test_that("evaluation rows keep their calibration curves when bound together", {
  rows <- lapply(c("a", "b"), function(site) {
    s <- calibrationSample(n = 300L, seed = utf8ToInt(site))
    evaluatePredictions(list(yLabels = s$y), stats::plogis(s$eta), density = 1, clientId = site,
      clientIndex = 1L, linearPredictor = s$eta)
  })
  bound <- bindEvaluations(rows)
  curves <- attr(bound, "calibrationCurve")
  expect_identical(nrow(bound), 2L)
  expect_identical(as.character(unique(curves$clientId)), c("a", "b"))
  expect_identical(as.numeric(table(curves$clientId)), c(50, 50))
  s <- calibrationSample(n = 400L)
  worker <- .evaluateBinaryMetrics(s$y, stats::plogis(s$eta), w = 1, linearPredictor = s$eta)
  expect_true(all(c("calibrationIci", "calibrationInTheLarge", "calibrationIntercept", "calibrationSlope") %in%
    names(worker)))
  expect_identical(nrow(attr(worker, "calibrationCurve")), 50L)
})

test_that("federated evaluation returns calibration curves from its workers", {
  skip_if_not_installed("parallel")
  cl <- parallel::makeCluster(2L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  set.seed(8)
  sites <- lapply(1:2, function(i) {
    x <- cbind(1, stats::rnorm(150))
    list(xMatrix = x, yLabels = stats::rbinom(150, 1, stats::plogis(x %*% c(-1, 1))))
  })
  parallel::clusterApply(cl, sites, function(site) {
    assign("clientData", site, envir = .GlobalEnv)
    NULL
  })
  w <- c(-1, 1)
  rows <- clusterEvaluateModel(cl, w)
  curves <- attr(rows, "calibrationCurve")
  expect_identical(nrow(rows), 2L)
  expect_identical(sort(unique(curves$client)), 1:2)
  expect_true(all(is.finite(rows$calibrationIci)))
  expect_equal(rows$logLoss[1], binaryLogLoss(as.numeric(sites[[1]]$xMatrix %*% w), sites[[1]]$yLabels),
    tolerance = 1e-12)
  expect_true(all(is.finite(rows$calibrationInTheLarge)))
})
