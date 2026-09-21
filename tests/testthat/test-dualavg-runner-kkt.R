kktRunner <- function() {
  path <- test_path("..", "..", "extras", "runComparisonMatrix.R")
  if (!file.exists(path)) skip("Extras scripts unavailable")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

test_that("comparison KKT flags are opt-in and restricted to DualAvg", {
  env <- kktRunner()
  args <- list("dualavg-kkt-tolerance" = "1e-7", "dualavg-kkt-check-every" = "25")
  for (method in c("DualAvg", "DualAvgR", "DualAvgCpp")) {
    cfg <- env$methodConfig(method, "ageSexPhenotypes", args)
    expect_equal(cfg$dualAvgKktTolerance, 1e-7)
    expect_equal(cfg$dualAvgKktCheckEvery, 25)
  }
  expect_null(env$methodConfig("DualAvg", "ageSex", list())$dualAvgKktTolerance)
  expect_null(env$methodConfig("PooledLasso", "ageSex", args)$dualAvgKktTolerance)
  expect_equal(env$methodConfig("DualAvg", "ageSex", args[1])$dualAvgKktCheckEvery, 100)
})

test_that("unconverged penalty candidates are saved but never scored", {
  scoreCalls <- 0L
  fitCalls <- 0L
  cfg <- list(dualAvgKktTolerance = 1e-7, dualAvgKktCheckEvery = 100L)
  converged <- FALSE
  local_mocked_bindings(
    subsetCluster = function(cl, ids) list(ids = ids),
    fitFederated = function(cl, algorithm, config, verbose) {
      fitCalls <<- fitCalls + 1L
      expect_equal(config$dualAvgKktTolerance, 1e-7)
      expect_equal(config$dualAvgKktCheckEvery, 100L)
      list(w = c(0, 0), config = config, roundsCompleted = 1000L,
        converged = converged, stopReason = if (converged) "converged" else "roundLimit",
        kktMaxAbs = if (converged) 1e-8 else 0.01, kktChecks = 10L,
        kktHistory = data.frame(round = 1000L, kktMaxAbs = 0.01))
    },
    clusterCreateMatrices = function(cl, config) NULL,
    clusterEvaluateModel = function(cl, w) {
      scoreCalls <<- scoreCalls + 1L
      data.frame(auc = 0.7)
    }, .package = "FederatedLearning"
  )
  run <- function() tuneLambda(list(1, 2), "DualAvg", cfg, 1:2,
    rounds = 1000L, clientFrac = 1, epsilon = 1e-6,
    lambdaStrategy = list(initial = function(x, n, context) x,
      final = function(x, n, context) x), lambdaDefault = 0.01,
    totalPopSize = 20, globalMap = data.frame(covariateId = 1, columnId = 1),
    verbose = FALSE)
  err <- tryCatch(run(), dualAvgConvergenceError = identity)
  expect_s3_class(err, "dualAvgConvergenceError")
  expect_equal(fitCalls, 1L)
  expect_equal(scoreCalls, 0L)
  d <- err$dualAvgConvergence
  expect_match(d$stage, "validation client 1")
  expect_equal(d$kktMaxAbs, 0.01)
  expect_equal(d$coefficients, c(0, 0))
  expect_equal(d$lambdaSearchTrace$roundsCompleted, 1000L)
  expect_true(is.na(d$lambdaSearchTrace$auc))
  expect_false(d$lambdaSearchTrace$converged)
  converged <- TRUE
  result <- run()
  expect_gt(scoreCalls, 0L)
  expect_true(all(result$trace$converged))
  expect_true(all(result$trace$kktMaxAbs == 1e-8))
  expect_true(all(result$trace$auc == 0.7))
})

test_that("final and config-CV guards reject before evaluating held-out data", {
  env <- kktRunner()
  cfg <- list(dualAvgKktTolerance = 1e-7, lambda = 0.01,
    mapping = data.frame(covariateId = 1, columnId = 1))
  local_mocked_bindings(
    fitFederated = function(cl, algorithm, config, verbose) list(config = config,
      w = c(0, 0), converged = FALSE, stopReason = "roundLimit",
      roundsCompleted = 10L, kktMaxAbs = 0.01),
    clusterCreateMatrices = function(...) stop("must not create test matrices"),
    .package = "FederatedLearning"
  )
  expect_error(env$fitFederatedFold("DualAvg", NULL, NULL, cfg, tempdir(),
    "task", "ageSex", 1, "test", FALSE), "DualAvg final fit did not converge")
  local_mocked_bindings(subsetCluster = function(cl, ids) list(ids = ids),
    .package = "FederatedLearning")
  expect_error(env$scoreFederatedConfigInnerCv("DualAvg", list(1, 2), cfg, 1:2, FALSE),
    "DualAvg config CV.*did not converge")
  expect_invisible(.requireDualAvgConvergence(list(), list(), "disabled"))
  expect_invisible(.requireDualAvgConvergence(list(converged = TRUE, kktMaxAbs = 1e-8), cfg, "pass"))
  expect_error(.requireDualAvgConvergence(list(converged = TRUE, kktMaxAbs = 0.1), cfg, "bad"),
    "did not converge")
})

test_that("the runner writes convergence failures and their diagnostics", {
  env <- kktRunner()
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (id in c("a", "b", "c")) dir.create(file.path(directory, "data", "task", id), recursive = TRUE)
  resultDir <- file.path(directory, "results")
  args <- list("data-root" = file.path(directory, "data"), "result-directory" = resultDir,
    tasks = "task", "feature-sets" = "ageSex", methods = "DualAvg",
    "client-ids" = "a,b,c", folds = "1", "dualavg-kkt-tolerance" = "1e-7",
    "debug-diagnostics" = "true", "resume" = "false")
  history <- data.frame(round = c(100, 200), kktMaxAbs = c(0.02, 0.01), objective = c(4, 3))
  env$selectMethodConfigForFold <- function(method, clTrain, configs, trainPopSizes, args, verbose) {
    .requireDualAvgConvergence(list(converged = FALSE, stopReason = "roundLimit",
      roundsCompleted = 200L, kktMaxAbs = 0.01, kktChecks = 2L, kktHistory = history,
      w = c(0, 0)), configs[[1]], "inner CV", data.frame(auc = NA_real_))
  }
  env$collectDebugDiagnostics <- function(...) NULL
  env$safeStopCluster <- function(...) NULL
  local_mocked_bindings(
    clusterInit = function(...) list(),
    clusterLoadData = function(cl, clientPaths, popSettings) rep(10, length(clientPaths)),
    clusterCollectCovRefs = function(...) data.frame(covariateId = 10, columnId = 1L),
    clusterCreateMatrices = function(...) NULL,
    clusterDiagnostics = function(...) data.frame(client = 1L, n = 10),
    .package = "FederatedLearning"
  )
  env$runComparison(args)
  rows <- read.csv(file.path(resultDir, "comparison_results.csv"))
  expect_false(rows$dualAvgConverged)
  expect_equal(rows$dualAvgStopReason, "roundLimit")
  expect_equal(rows$dualAvgFailedFitRounds, 200L)
  expect_equal(rows$dualAvgConvergenceStage, "inner CV")
  expect_true(is.na(rows$auc))
  expect_match(rows$error, "did not converge")
  file <- list.files(file.path(resultDir, "debug"), pattern = "error.rds$", full.names = TRUE)
  expect_length(file, 1)
  saved <- readRDS(file)$dualAvgConvergence
  expect_equal(saved$kktHistory, history)
  expect_true(is.na(saved$lambdaSearchTrace$auc))
})
