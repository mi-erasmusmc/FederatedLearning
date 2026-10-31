sensitivityPopulation <- function() {
  PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = FALSE, minTimeAtRisk = 1L,
    riskWindowStart = 1L, riskWindowEnd = 365L, removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999L)
}

test_that("excluded covariates are never selected, and settings without exclusions keep their form", {
  expect_false("excludeCovariateIds" %in% names(createFeatureSettings(analysisIds = 49)))
  settings <- createFeatureSettings(covariateIds = c(1002, 8532001), analysisIds = 49,
    excludeCovariateIds = 1160049)
  expect_identical(settings$excludeCovariateIds, 1160049)
  ref <- data.frame(covariateId = c(1002, 8532001, 1160049, 1165049, 5), analysisId = c(2, 1, 49, 49, 3))
  expect_identical(filterCovariateRef(ref, settings)$covariateId, c(1002, 8532001, 1165049))
  everything <- createFeatureSettings(excludeCovariateIds = 5)
  expect_identical(filterCovariateRef(ref, everything)$covariateId, c(1002, 8532001, 1160049, 1165049))
  expect_error(createFeatureSettings(excludeCovariateIds = -1), "excludeCovariateIds")
})

test_that("the lead site is chosen by training-size rank, the largest by default", {
  rank <- FederatedLearning:::.leadIndexByRank
  ns <- c(10, 50, 30, 50)
  expect_identical(rank(ns), which.max(ns))
  expect_identical(rank(ns, 2L), 4L)
  expect_identical(rank(ns, 3L), 3L)
  expect_identical(rank(ns, 4L), 1L)
  expect_error(rank(ns, 5L), "leadRank")
  expect_error(rank(ns, 1.5), "leadRank")
})

test_that("ADAP sites start from the runner's local estimates when asked", {
  fit <- FederatedLearning:::.fitPdaStyleLocalLasso
  config <- list(adapInitBackend = "localCyclops", adapInitEstimates = list(c(1, 2, 3), c(-1, 0.5, 0)))
  x <- Matrix::Matrix(matrix(c(1, 0, 1, 1, 0, 1), 3, 2), sparse = TRUE)
  design <- cbind(1, x)
  oldOptions <- options(FederatedLearning.localId = 2L)
  on.exit(options(oldOptions), add = TRUE)
  start <- fit(x, design, c(0, 1, 1), config)
  expect_identical(start$beta, c(-1, 0.5, 0))
  expect_true(is.na(start$lambda))
  config$adapInitEstimates[[2]] <- c(1, 2)
  expect_error(fit(x, design, c(0, 1, 1), config), "local Cyclops estimate")
})

test_that("sensitivity options change the lead, the ADAP start, the pooled tuning and the averaging weights", {
  if (!identical(Sys.getenv("FEDERATEDLEARNING_GOLDEN"), "true")) {
    skip("Set FEDERATEDLEARNING_GOLDEN=true after installing the package under test")
  }
  skip_if_not_installed("Cyclops")
  skip_if_not_installed("PatientLevelPrediction")
  fixture <- normalizePath(test_path("fixtures", "runner-golden"))
  work <- tempfile("sensitivity-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(file.path(fixture, "data"), work, recursive = TRUE)
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  oldLibs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(oldLibs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = oldLibs), add = TRUE)

  task <- createTaskSettings("goldenTask", 1, 3, sensitivityPopulation())
  sites <- c(site1 = "data/goldenTask/site1", site2 = "data/goldenTask/site2", site3 = "data/goldenTask/site3")
  features <- list(ageSex = createFeatureSettings(covariateIds = c(1002, 8532001)))
  fast <- list(lambdaSearch = "grid", lambdaGridLen = 5)
  run <- function(output, methods) {
    runComparison(task, sites, features, methods, output, holdouts = "site1",
      runSettings = createRunSettings(verbose = FALSE))
    readComparisonResults(output)
  }

  results <- run("out", list(
    createMethodSettings("LocalAvgLasso"),
    createMethodSettings("PooledLasso", pooledTuning = "rows"),
    do.call(createMethodSettings, c(list("ADAP2", adapInitBackend = "localCyclops", leadRank = 2L), fast)),
    createMethodSettings("ODAL1", leadRank = 2L)))
  expect_true(all(is.na(results$error) | !nzchar(results$error)))

  # The lead is the second-largest training site.
  sizes <- vapply(sites[c("site2", "site3")], function(path) {
    nrow(loadClientData(path, task$populationSettings)$population)
  }, numeric(1))

  # ADAP's local starting estimates are the coefficient-averaging baseline's
  # local models, so their size-weighted average is that baseline's model.
  average <- loadModel("out", "goldenTask", "site1", "ageSex", "LocalAvgLasso")
  estimates <- loadModel("out", "goldenTask", "site1", "ageSex", "ADAP2")$config$adapInitEstimates
  expect_length(estimates, 2L)
  components <- sapply(average$components, function(component) component$coefficients$coefficientSharedScale)
  expect_equal(unname(do.call(cbind, estimates)), unname(components), tolerance = 1e-12)
  expect_equal(as.numeric(do.call(cbind, estimates) %*% (sizes / sum(sizes))),
    average$models[[1]]$coefficients$coefficientSharedScale, tolerance = 1e-12)
  expected <- order(-sizes, seq_along(sizes))[[2]]
  expect_identical(as.integer(unique(results$leadIndex[results$method %in% c("ADAP2", "ODAL1")])), expected)

  # Pooled lasso tuned by stratified folds over the pooled rows.
  pooled <- loadModel("out", "goldenTask", "site1", "ageSex", "PooledLasso")
  expect_identical(pooled$models[[1]]$fittingSettings$tuning$validation, "stratifiedFolds")
  expect_identical(unique(results$configLabel[results$method == "PooledLasso"]), "pooledTuning=rows")

  # Equal weights average the local models with weight 1/K.
  equal <- run("equal", list(createMethodSettings("LocalAvgLasso", localAverageWeighting = "equalClient")))
  expect_match(unique(equal$configLabel), "^coefficientAverage=equalClient")
  model <- loadModel("equal", "goldenTask", "site1", "ageSex", "LocalAvgLasso")
  weights <- vapply(model$components, `[[`, numeric(1), "weight")
  expect_equal(weights, rep(1 / length(weights), length(weights)))
  components <- sapply(model$components, function(component) component$coefficients$coefficientSharedScale)
  expect_equal(model$models[[1]]$coefficients$coefficientSharedScale, rowMeans(components), tolerance = 1e-12)
  expect_identical(model$aggregation, "coefficientAverage=equalClient")
})
