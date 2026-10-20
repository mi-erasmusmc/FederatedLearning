settingsCovariateRef <- function() {
  data.frame(
    covariateId = c(1002, 8532001, 111, 222, 333),
    analysisId = c(1, 1, 49, 49, 7),
    covariateName = letters[1:5]
  )
}

test_that("feature settings validate and normalise their inputs", {
  settings <- createFeatureSettings(covariateIds = c(8532001, 1002, 1002), analysisIds = 49)
  expect_s3_class(settings, "featureSettings")
  expect_identical(settings$covariateIds, c(1002, 8532001))
  expect_identical(settings$analysisIds, 49)
  expect_identical(settings$minFraction, 0.001)
  expect_null(createFeatureSettings(minFraction = NULL)$minFraction)
  expect_null(createFeatureSettings()$covariateIds)
  expect_error(createFeatureSettings(covariateIds = 1.5), "whole numbers")
  expect_error(createFeatureSettings(analysisIds = -1), "whole numbers")
  expect_error(createFeatureSettings(covariateIds = numeric()), "whole numbers")
  expect_error(createFeatureSettings(minFraction = 2), "between 0 and 1")
  expect_error(createFeatureSettings(minFraction = c(0.1, 0.2)), "single number")
})

test_that("feature settings select covariates or analyses and reproduce the named sets", {
  covariateRef <- settingsCovariateRef()
  select <- function(...) filterCovariateRef(covariateRef, featureSet = createFeatureSettings(...))$covariateId
  presets <- list(
    all = list(),
    ageSex = list(covariateIds = c(1002, 8532001)),
    phenotypes = list(analysisIds = 49),
    ageSexPhenotypes = list(covariateIds = c(1002, 8532001), analysisIds = 49)
  )
  for (name in names(presets)) {
    expect_identical(do.call(select, presets[[name]]),
      filterCovariateRef(covariateRef, featureSet = name)$covariateId, label = name)
  }
  expect_identical(select(covariateIds = 333, analysisIds = 49), c(111, 222, 333))
  expect_identical(select(covariateIds = 999), numeric())
  expect_identical(filterCovariateRef(covariateRef, featureSet = createFeatureSettings(analysisIds = 49),
    covariateIds = 222)$covariateId, 222)
})
