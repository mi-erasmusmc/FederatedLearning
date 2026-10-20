test_that("saved population settings must equal the task's settings", {
  population <- PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 365,
    requireTimeAtRisk = FALSE, minTimeAtRisk = 1)
  sites <- file.path(tempfile(), c("a", "b"))
  on.exit(unlink(dirname(sites[1]), recursive = TRUE), add = TRUE)
  for (site in sites) dir.create(site, recursive = TRUE)
  expect_true(checkSavedPopulationSettings(sites, population))
  saveRDS(population, file.path(sites[1], "populationSettings.rds"))
  expect_true(checkSavedPopulationSettings(sites, population))
  reconstructed <- population
  reconstructed$riskWindowEnd <- 365L
  expect_true(checkSavedPopulationSettings(sites, reconstructed))
  shorter <- population
  shorter$riskWindowEnd <- 30
  expect_error(checkSavedPopulationSettings(sites, shorter), "differ from the task's population settings")
})

test_that("feature labels resolve to supplied feature settings", {
  settings <- createFeatureSettings(analysisIds = 49)
  args <- list(featureSettings = list(custom = settings))
  expect_identical(featureSelection("custom", args), settings)
  expect_identical(featureSelection("ageSex", args), "ageSex")
  expect_identical(featureSelection("ageSex", list()), "ageSex")
  expect_identical(methodConfig("PooledLasso", "custom", args)$featureSet, settings)
  expect_identical(methodConfig("PooledLasso", "ageSex", list())$featureSet, "ageSex")
})

test_that("explicit site paths require a single task", {
  args <- list(tasks = "a,b", `result-directory` = tempfile())
  expect_error(runComparisonFromArgs(args, clientPaths = c(s1 = tempdir(), s2 = tempdir())),
    "exactly one task")
})
