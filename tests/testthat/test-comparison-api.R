apiPopulation <- function(riskWindowEnd = 365L) {
  PatientLevelPrediction::createStudyPopulationSettings(requireTimeAtRisk = FALSE, minTimeAtRisk = 1L,
    riskWindowStart = 1L, riskWindowEnd = riskWindowEnd, removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999L)
}

test_that("task, method and execution settings validate their inputs", {
  task <- createTaskSettings("dvt", 1, 2, apiPopulation())
  expect_s3_class(task, "taskSettings")
  expect_identical(task$populationSettings$riskWindowEnd, 365L)
  expect_error(createTaskSettings("dvt", 1, 2, list(riskWindowEnd = 365)), "createStudyPopulationSettings")
  expect_error(createTaskSettings("", 1, 2, apiPopulation()), "non-empty string")
  expect_error(createTaskSettings("dvt", 1.5, 2, apiPopulation()), "targetId")

  method <- createMethodSettings("DualAvg", dualavgLambda = c(0.01, 0.001), dualavgTuneLambda = FALSE)
  expect_s3_class(method, "methodSettings")
  expect_identical(method$options$dualavgLambda, c(0.01, 0.001))
  expect_error(createMethodSettings("Lasso"), "Valid methods: .*PooledLasso")
  expect_error(createMethodSettings("DualAvg", dualAvgLambda = 1), "Unknown method option.*dualAvgLambda.*dualavgLambda")
  expect_error(createMethodSettings("DualAvg", 0.01), "must be named")
  expect_error(createMethodSettings("DualAvg", lambda = NA), "non-missing")
  expect_true(all(c("PooledLasso", "LocalEnsembleLasso", "DualAvg", "ODAL2", "ADAP2") %in% .comparisonMethods()))

  expect_s3_class(createRunSettings(), "runSettings")
  expect_error(createRunSettings(mirai = "yes"), "mirai must be TRUE or FALSE")
})

test_that("every runner option is either a method option or set by runComparison", {
  path <- test_path("..", "..", "R", "ComparisonRunner.R")
  if (!file.exists(path)) skip("Comparison runner source is unavailable")
  source <- paste(readLines(path), collapse = "\n")
  flags <- unique(c(
    regmatches(source, gregexpr('argValue\\(args, "[a-z0-9-]+"\\)', source))[[1]],
    regmatches(source, gregexpr('args\\[\\["[a-z0-9-]+"\\]\\]', source))[[1]]))
  flags <- sub('.*"([a-z0-9-]+)".*', "\\1", flags)
  runLevel <- c("data-root", "result-directory", "tasks", "feature-sets", "methods", "client-ids",
    "clients", "folds", "hosts", "mirai", "verbose", "resume", "rerun-errors", "rerun-missing-models",
    "save-models", "debug-diagnostics", "surrogate-capture-directory", "preprocessing-audit-only",
    "preprocessing-reference-directory", "federated-preprocess", "baseline-preprocess",
    "baseline-preprocess-min-fraction", "baseline-preprocess-normalize",
    "baseline-preprocess-remove-redundancy", "preprocess-min-fraction", "preprocess-normalize",
    "preprocess-remove-redundancy", "require-time-at-risk", "min-time-at-risk", "risk-window-start",
    "remove-prior-outcomes", "prior-outcome-lookback", "map-type", "intercept")
  expect_setequal(setdiff(flags, runLevel), .methodOptionFlags)
})

test_that("runComparison passes one option set per runner call", {
  calls <- list()
  local_mocked_bindings(
    runComparisonFromArgs = function(args, populationSettings, clientPaths) {
      calls[[length(calls) + 1L]] <<- list(args = args, population = populationSettings, paths = clientPaths)
      invisible(NULL)
    },
    readComparisonResults = function(outputDirectory) data.frame(),
    .package = "FederatedLearning"
  )
  task <- createTaskSettings("dvt", 1, 2, apiPopulation())
  sites <- c(MDCD = "d/MDCD", CCAE = "d/CCAE", JMDC = "d/JMDC")
  features <- list(phenotypes = createFeatureSettings(analysisIds = 49),
    ageSex = createFeatureSettings(covariateIds = c(1002, 8532001)))
  methods <- list(
    createMethodSettings("PooledLasso", cyclopsCv = FALSE, cyclopsVariance = 0.1),
    createMethodSettings("DualAvg", dualavgLambda = c(0.01, 0.001), dualavgTuneLambda = FALSE),
    createMethodSettings("LocalAvgLasso", cyclopsCv = FALSE, cyclopsVariance = 0.1))
  runComparison(task, sites, features, methods, "out", holdouts = c("JMDC", "MDCD"),
    runSettings = createRunSettings(hosts = "h1"))

  expect_length(calls, 2L)
  first <- calls[[1]]$args
  expect_identical(first$methods, "PooledLasso,LocalAvgLasso")
  expect_identical(first$`cyclops-cv`, "false")
  expect_identical(first$`cyclops-variance`, "0.1")
  expect_null(first$`dualavg-lambda`)
  expect_identical(calls[[2]]$args$methods, "DualAvg")
  expect_identical(calls[[2]]$args$`dualavg-lambda`, "0.01,0.001")
  expect_null(calls[[2]]$args$`cyclops-variance`)
  expect_identical(first$folds, "3,1")
  expect_identical(first$`feature-sets`, "phenotypes,ageSex")
  expect_identical(first$featureSettings, features)
  expect_identical(first$`federated-preprocess`, "baseline")
  expect_identical(first$`baseline-preprocess-min-fraction`, "0.001")
  expect_identical(first$hosts, "h1")
  expect_identical(calls[[1]]$population, task$populationSettings)
  expect_identical(calls[[1]]$paths, sites)

  expect_error(runComparison(task, sites, features, list(methods[[1]], methods[[1]]), "out"), "only once")
  expect_error(runComparison(task, sites, list(a = createFeatureSettings(), b = createFeatureSettings(minFraction = 0.01)),
    methods, "out"), "share minFraction")
  expect_error(runComparison(task, sites, features, methods, "out", holdouts = "Optum"), "holdouts")
  expect_error(runComparison(task, unname(sites), features, methods, "out"), "unique site names")
})

test_that("audit-only comparisons return the feature-filter summary", {
  output <- tempfile("audit-")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  local_mocked_bindings(
    runComparisonFromArgs = function(args, populationSettings, clientPaths) {
      expect_identical(args$`preprocessing-audit-only`, "true")
      directory <- file.path(args$`result-directory`, "preprocessing")
      dir.create(directory, recursive = TRUE)
      utils::write.csv(data.frame(task = "dvt", fold = 1L, retainedPredictors = 3L),
        file.path(directory, "summary.csv"), row.names = FALSE)
      invisible(NULL)
    },
    .package = "FederatedLearning"
  )
  summary <- runComparison(createTaskSettings("dvt", 1, 2, apiPopulation()),
    c(A = "d/A", B = "d/B"), list(all = createFeatureSettings()),
    createMethodSettings("PooledLasso"), output, runSettings = createRunSettings(auditOnly = TRUE))
  expect_identical(summary, data.frame(task = "dvt", fold = 1L, retainedPredictors = 3L))
})

test_that("the public API reproduces the runner's golden outputs", {
  if (!identical(Sys.getenv("FEDERATEDLEARNING_GOLDEN"), "true")) {
    skip("Set FEDERATEDLEARNING_GOLDEN=true after installing the package under test")
  }
  skip_if_not_installed("Cyclops")
  fixture <- normalizePath(test_path("fixtures", "runner-golden"))
  golden <- readRDS(file.path(fixture, "golden.rds"))
  work <- tempfile("golden-api-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(file.path(fixture, "data"), work, recursive = TRUE)
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  oldLibs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(oldLibs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = oldLibs), add = TRUE)

  options <- list(cyclopsCv = FALSE, cyclopsVariance = 0.1, localAverageThreshold = 0.05,
    dualavgTuneLambda = FALSE, dualavgLambda = 0.01, dualavgRounds = 200, lambdaSearch = "grid",
    lambdaGridLen = 5)
  methods <- lapply(c("PooledLasso", "LocalAvgLasso", "BiggestSiteLasso", "LocalEnsembleLasso",
    "LocalBestLasso", "SparseLocalAvgLasso", "DualAvg", "ODAL1", "ODAL2", "ADAP2"),
    function(method) do.call(createMethodSettings, c(list(method), options)))
  runComparison(
    createTaskSettings("goldenTask", 1, 3, apiPopulation()),
    sites = c(site1 = "data/goldenTask/site1", site2 = "data/goldenTask/site2", site3 = "data/goldenTask/site3"),
    featureSettings = list(all = createFeatureSettings(), ageSex = createFeatureSettings(covariateIds = c(1002, 8532001))),
    methodSettings = methods, outputDirectory = "out", holdouts = c("site1", "site3"),
    runSettings = createRunSettings(verbose = FALSE))
  actual <- goldenRunnerSnapshot("out")

  # Artifacts also record how the run was specified (arguments and configuration),
  # which legitimately differs between the API and the command line.
  specification <- function(artifacts) lapply(artifacts, function(a) a[setdiff(names(a), c("args", "config"))])
  expect_identical(names(actual$artifacts), names(golden$artifacts))
  expect_equal(specification(actual$artifacts), specification(golden$artifacts), tolerance = 1e-7)
  for (part in c("results", "summary", "diagnostics", "lambdaPaths", "preprocessing")) {
    expect_equal(actual[[part]], golden[[part]], tolerance = 1e-7, label = part)
  }
})

test_that("saved models reproduce their recorded held-out metrics without refitting", {
  skip_if_not_installed("PatientLevelPrediction")
  fixture <- test_path("fixtures", "runner-golden")
  golden <- readRDS(file.path(fixture, "golden.rds"))
  task <- createTaskSettings("goldenTask", 1, 3, apiPopulation())
  baselines <- c("PooledLasso", "LocalAvgLasso", "BiggestSiteLasso", "LocalEnsembleLasso",
    "LocalBestLasso", "SparseLocalAvgLasso")
  for (name in names(golden$artifacts)) {
    model <- structure(golden$artifacts[[name]], class = c("comparisonModel", "list"))
    holdout <- model$testClientIds
    recorded <- golden$results[golden$results$task == model$task & golden$results$fold == model$fold &
      golden$results$featureSet == model$featureSet & golden$results$method == model$method, , drop = FALSE]
    expect_identical(nrow(recorded), 1L, label = name)
    scored <- evaluateModel(model, file.path(fixture, "data", "goldenTask", holdout), task)
    expect_identical(scored$clientId, holdout, label = name)
    expect_equal(scored$n, recorded$n, label = name)
    expect_equal(scored$outcomes, recorded$outcomes, label = name)
    expect_equal(scored$auc, recorded$auc, tolerance = 1e-10, label = name)
    expect_equal(scored$logLoss, recorded$logLoss, tolerance = 1e-10, label = name)
    if (model$method %in% baselines) {
      fields <- c("calibrationIntercept", "calibrationSlope", "density")
      expect_equal(unlist(scored[fields]), unlist(recorded[fields]), tolerance = 1e-8, label = name)
    }
  }
  expect_error(evaluateModel(golden$artifacts[[1]], fixture, task), "loadModel")
})

test_that("loadModel finds a saved model by its held-out site", {
  directory <- tempfile()
  dir.create(file.path(directory, "models"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  artifact <- list(task = "t", fold = 2L, featureSet = "f", method = "PooledLasso", schemaVersion = 1L,
    modelId = "m.rds", models = list(list(coefficients = data.frame(coefficient = c(-1, 0.5),
      covariateId = c(NA, "1002"), isIntercept = c(TRUE, FALSE)))))
  saveRDS(artifact, file.path(directory, "models", "m.rds"))
  write.csv(data.frame(task = "t", fold = 2L, featureSet = "f", method = "PooledLasso", auc = 0.7,
    clientId = "JMDC", modelFile = "models/m.rds", modelId = "m.rds"),
    file.path(directory, "comparison_results.csv"), row.names = FALSE)
  model <- loadModel(directory, "t", "JMDC", "f", "PooledLasso")
  expect_s3_class(model, "comparisonModel")
  expect_identical(model$models[[1]]$coefficients$coefficient, c(-1, 0.5))
  expect_error(loadModel(directory, "t", "MDCD", "f", "PooledLasso"), "No result")
  expect_error(readComparisonResults(tempfile()), "No comparison results")
})
