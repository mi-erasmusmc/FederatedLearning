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

combinationSettings <- function(...) {
  createFeatureSettings(..., combinations = list(list(covariateId = 999001049, name = "Any of a and b",
    components = c(111, 222))))
}

test_that("covariate combinations are validated", {
  settings <- combinationSettings(analysisIds = 49)
  expect_identical(settings$combinations[[1]]$components, c(111, 222))
  combo <- function(...) createFeatureSettings(combinations = list(list(...)))
  expect_error(combo(covariateId = 9, name = "x"), "covariateId, name and components")
  expect_error(combo(covariateId = 9, name = "", components = c(1, 2)), "non-empty name")
  expect_error(combo(covariateId = 9, name = "x", components = 1), "at least two other components")
  expect_error(combo(covariateId = 1, name = "x", components = c(1, 2)), "at least two other components")
  expect_error(combo(covariateId = c(8, 9), name = "x", components = c(1, 2)), "one new covariateId")
  expect_error(createFeatureSettings(combinations = list(
    list(covariateId = 8, name = "x", components = c(1, 2)),
    list(covariateId = 9, name = "y", components = c(2, 3)))), "must not share")
  expect_error(createFeatureSettings(combinations = "x"), "must be a list")
})

test_that("combinations replace their components before selection", {
  covariateRef <- settingsCovariateRef()
  combined <- filterCovariateRef(covariateRef, combinationSettings())
  expect_identical(combined$covariateId, c(1002, 8532001, 333, 999001049))
  expect_identical(combined$analysisId[combined$covariateId == 999001049], 49)
  expect_identical(combined$covariateName[combined$covariateId == 999001049], "Any of a and b")
  expect_identical(filterCovariateRef(covariateRef, combinationSettings(analysisIds = 49))$covariateId, 999001049)
  expect_identical(filterCovariateRef(covariateRef, combinationSettings(covariateIds = 1002))$covariateId, 1002)
  # An unobserved component is zero; no observed component means no combined covariate.
  expect_identical(filterCovariateRef(covariateRef[covariateRef$covariateId != 222, ], combinationSettings())$covariateId,
    c(1002, 8532001, 333, 999001049))
  expect_identical(filterCovariateRef(covariateRef[!covariateRef$covariateId %in% c(111, 222), ],
    combinationSettings())$covariateId, c(1002, 8532001, 333))
  clash <- rbind(covariateRef, data.frame(covariateId = 999001049, analysisId = 1, covariateName = "z"))
  expect_error(filterCovariateRef(clash, combinationSettings()), "already an extracted covariate")
})

test_that("a combination stops when the extraction never requested a component", {
  cohortSettings <- structure(list(covariateCohorts = data.frame(cohortId = c(1201, 1202, 1203)),
    analysisId = 49), class = "covariateSettings")
  requested <- .requestedCohortCovariates(list(structure(list(), class = "covariateSettings"), cohortSettings))
  expect_identical(requested$covariateIds, c(1201049, 1202049, 1203049))
  expect_null(.requestedCohortCovariates(NULL))
  covariateRef <- data.frame(covariateId = c(1002, 1201049), analysisId = c(1, 49), covariateName = c("age", "a"))
  attr(covariateRef, "requestedCohortCovariates") <- requested
  settings <- function(components) createFeatureSettings(combinations = list(list(
    covariateId = 999001049, name = "Any antibiotic", components = components)))
  # 1202049 was requested but unobserved, so it is zero.
  expect_identical(filterCovariateRef(covariateRef, settings(c(1201049, 1202049)))$covariateId, c(1002, 999001049))
  expect_error(filterCovariateRef(covariateRef, settings(c(1201049, 1204049))),
    "1204049 that this site's extraction did not request")
  expect_error(createGlobalMap(list(covariateRef, covariateRef), type = "intersection",
    featureSet = settings(c(1201049, 1204049))), "Training site 1: .*did not request")
})

test_that("a combined column is the logical OR of its components in the design matrix", {
  skip_if_not_installed("PatientLevelPrediction")
  site <- test_path("fixtures", "runner-golden", "data", "goldenTask", "site1")
  population <- PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = FALSE,
    minTimeAtRisk = 1, riskWindowEnd = 365)
  plpData <- suppressMessages(loadClientData(site, population))
  components <- c(8507001, 30753102, 80180102)
  settings <- createFeatureSettings(combinations = list(list(covariateId = 999001049,
    name = "Any of three", components = components)))
  refs <- list(getClientFeatures(plpData))
  plain <- createGlobalMap(refs, type = "intersection")
  combinedMap <- createGlobalMap(refs, type = "intersection", featureSet = settings)
  expect_identical(nrow(combinedMap), nrow(plain) - length(components) + 1L)
  x <- suppressMessages(createClientMatrix(plpData, list(mapping = plain, intercept = FALSE)))$xMatrix
  y <- suppressMessages(createClientMatrix(plpData,
    list(mapping = combinedMap, intercept = FALSE, featureSet = settings)))$xMatrix
  parts <- plain$columnId[plain$covariateId %in% components]
  expected <- as.numeric(Matrix::rowSums(x[, parts] != 0) > 0)
  combinedColumn <- combinedMap$columnId[combinedMap$covariateId == 999001049]
  expect_identical(as.numeric(y[, combinedColumn]), expected)
  expect_gt(sum(Matrix::rowSums(x[, parts] != 0) > 1), 0)
  expect_lt(sum(expected), sum(x[, parts]))
  for (id in setdiff(plain$covariateId, components)) {
    expect_identical(as.numeric(y[, combinedMap$columnId[combinedMap$covariateId == id]]),
      as.numeric(x[, plain$columnId[plain$covariateId == id]]), label = id)
  }
})
