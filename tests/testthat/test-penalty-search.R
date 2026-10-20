fixtureClientData <- function(site = "site2") {
  path <- test_path("fixtures", "runner-golden", "data", "goldenTask", site)
  population <- PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = FALSE,
    minTimeAtRisk = 1, riskWindowEnd = 365)
  plpData <- suppressMessages(loadClientData(path, population))
  mapping <- createGlobalMap(list(getClientFeatures(plpData)), type = "intersection")
  suppressMessages(createClientMatrix(plpData, list(mapping = mapping, intercept = TRUE)))
}

test_that("stratified folds balance each outcome class and leave the RNG untouched", {
  y <- c(rep(0L, 23), rep(1L, 8))
  set.seed(99)
  before <- .Random.seed
  folds <- .stratifiedFoldId(y, 3L, seed = 5L)
  expect_identical(.Random.seed, before)
  expect_identical(folds, .stratifiedFoldId(y, 3L, seed = 5L))
  expect_false(identical(folds, .stratifiedFoldId(y, 3L, seed = 6L)))
  expect_identical(sort(unique(folds)), 1:3)
  for (class in 0:1) expect_lte(diff(range(tabulate(folds[y == class], 3L))), 1L)
})

test_that("the variance search finds the peak of a unimodal score within its stopping rule", {
  score <- function(v) c(-(log(v) - log(0.2))^2, 0)
  found <- .searchPriorVariance(score, startingVariance = 0.01)
  expect_identical(found$trace$variance[1], 0.01)
  expect_lt(abs(log(found$variance) - log(0.2)), log(1.5))
  expect_identical(found$stopReason, "searchConverged")
  expect_identical(found$score, max(found$trace$score))
  expect_warning(limited <- .searchPriorVariance(score, startingVariance = 0.01, maxEvals = 2L), "budget")
  expect_identical(limited$stopReason, "maxEvaluations")
  expect_identical(nrow(limited$trace), 2L)
  expect_error(.searchPriorVariance(function(v) c(NA, 0)), "not finite")
})

test_that("local lasso tunes on stratified site folds and refits on all site rows", {
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("PatientLevelPrediction")
  data <- fixtureClientData()
  args <- list(`cyclops-cv` = "true")
  fit <- suppressWarnings(fitBaselineWeights(list(data), args, seed = 11L))
  tuning <- fit$fittingSettings$tuning
  expect_identical(tuning$validation, "stratifiedFolds")
  expect_identical(tuning$folds, 3L)
  expect_identical(sum(tuning$foldSizes), data$n)
  expect_identical(tuning$foldOutcomes, tabulate(.stratifiedFoldId(data$yLabels, 3L, 11L)[data$yLabels == 1], 3L))
  expect_true(all(tuning$foldOutcomes > 0))
  best <- tuning$trace$variance[which.max(tuning$trace$score)]
  expect_identical(fit$selectedLambda, best)

  # The recorded score is the pooled held-out log likelihood at that variance.
  folds <- .stratifiedFoldId(data$yLabels, 3L, 11L)
  loss <- sum(vapply(1:3, function(f) {
    w <- fitCyclopsWeights(list(clientRows(data, which(folds != f))), args, 11L, variance = best)$w
    binaryLogLoss(as.numeric(data$xMatrix[folds == f, ] %*% w), data$yLabels[folds == f], meanLoss = FALSE)
  }, numeric(1)))
  expect_equal(max(tuning$trace$score), -loss / data$n, tolerance = 1e-10)
  # The final model is a fixed-variance refit on every training row of the site.
  refit <- fitCyclopsWeights(list(data), args, 11L, variance = best)
  expect_identical(fit$w, refit$w)
  expect_false(fit$fittingSettings$useCrossValidation)

  four <- suppressWarnings(fitBaselineWeights(list(data), c(args, list(`local-cv-folds` = "4")), seed = 11L))
  expect_identical(four$fittingSettings$tuning$folds, 4L)
  fixed <- fitBaselineWeights(list(data), list(`cyclops-cv` = "false", `cyclops-variance` = "0.1"), seed = 11L)
  expect_null(fixed$fittingSettings$tuning)
  expect_identical(fixed$selectedLambda, 0.1)
})
