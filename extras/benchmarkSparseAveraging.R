# Local smoke benchmark using the real-data matrices cached by the defaults sweep.
# Run from the package root; patient-level caches remain outside version control.
source("extras/runComparisonMatrix.R", local = TRUE)

benchmarkSparseAveraging <- function(args) {
  if (!requireNamespace("Matrix", quietly = TRUE)) stop("Matrix is required")
  cacheRoot <- argValue(args, "cache-root")
  if (is.null(cacheRoot)) stop("Supply --cache-root (containing sweep/data and autoPenalty)")
  output <- argValue(args, "output") %||% "results/sparseAveraging"
  tasks <- csvArg(argValue(args, "tasks"), c("lungCancerPhenotypes", "dementiaPhenotypes",
    "readmissionPhenotypes", "copdPneumoniaPhenotypes", "diabetesHeartFailurePhenotypes",
    "hypertensionAfPhenotypes"))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  config <- list(intercept = TRUE)
  run <- function(task) {
    start <- Sys.time()
    cached <- readRDS(file.path(cacheRoot, "sweep", "data", paste0(task, ".rds")))
    test <- readRDS(file.path(cacheRoot, "autoPenalty", task, "heldout_matrix.rds"))
    message("Starting ", task, ": ", sum(vapply(cached$data, `[[`, numeric(1), "n")), " training rows")
    processed <- preprocessBaselineData(cached$data, list(test), config, args)
    training <- processed$trainData
    test <- processed$testData[[1]]
    seed <- intArg(args[["baseline-seed"]], 42L) + cached$heldOut
    pooled <- fitBaselineWeights(training, args, seed, TRUE)
    attempts <- lapply(seq_along(training), function(i) fitLocalBaselineSafely(
      training, i, as.character(i), args, seed + i, TRUE))
    ok <- vapply(attempts, `[[`, logical(1), "ok")
    if (!any(ok)) stop("No successful local fits for ", task)
    local <- lapply(attempts[ok], `[[`, "fit")
    localSeconds <- sum(vapply(attempts, `[[`, numeric(1), "elapsedSeconds"))
    settings <- sparseAverageSettings(args)
    plain <- averageSparseLocalFits(local, training[ok], "SparseLocalAvgLasso", settings)
    referenceLambda <- sqrt(2 / pooled$selectedLambda) / sum(vapply(training, `[[`, numeric(1), "n"))
    score <- function(method, w, seconds, tau = NA_real_) {
      evaluation <- evaluateWeights(test, w, as.character(cached$heldOut), cached$heldOut)
      diagnostics <- pooledFitDiagnostics(training, w, referenceLambda, TRUE)
      data.frame(task = task, method = method, auc = evaluation$auc, logLoss = evaluation$logLoss,
        nonzeroPredictors = exactNonzero(w, TRUE), p = length(w), tau = tau,
        referenceLambda = referenceLambda,
        referenceObjective = diagnostics$pooledMeanLogLoss + referenceLambda * sum(abs(w[-1])),
        referenceKktMaxAbs = diagnostics$pooledKktMaxAbs,
        elapsedSeconds = seconds, successfulLocalFits = if (method == "PooledLasso") NA_integer_ else sum(ok),
        trainN = sum(vapply(training, `[[`, numeric(1), "n")),
        trainCases = sum(vapply(training, function(x) sum(x$yLabels), numeric(1))),
        testN = test$n, testCases = sum(test$yLabels))
    }
    rows <- list(score("PooledLasso", pooled$w, pooled$elapsedSeconds),
      score("LocalAvgLasso", plain$w, localSeconds))
    sparseFits <- list()
    for (method in sparseAverageMethods) {
      t0 <- Sys.time()
      tuning <- tuneSparseAverage(cached$data, method, args, config, seed)
      average <- averageSparseLocalFits(local, training[ok], method, settings)
      scales <- sparseAverageScales(processed$preprocessor, length(average$w), TRUE)
      w <- FederatedLearning:::softThresholdAverage(average$w, tuning$tau, TRUE, scales)
      seconds <- localSeconds + as.numeric(difftime(Sys.time(), t0, units = "secs"))
      rows[[length(rows) + 1L]] <- score(method, w, seconds, tuning$tau)
      sparseFits[[method]] <- list(w = w, tuning = tuning, average = average)
    }
    rows <- do.call(rbind, rows)
    rows$referenceObjectiveExcess <- rows$referenceObjective - rows$referenceObjective[1]
    saveRDS(list(results = rows, pooled = pooled, localAttempts = attempts, sparseFits = sparseFits,
      preprocessing = processed$preprocessor, mapping = cached$mapping,
      populationSettings = cached$populationSettings, trainingCounts = cached$counts,
      args = args, sessionInfo = utils::sessionInfo()), file.path(output, paste0(task, ".rds")))
    utils::write.csv(rows, file.path(output, paste0(task, ".csv")), row.names = FALSE)
    message("Finished ", task, " in ", round(as.numeric(difftime(Sys.time(), start, units = "secs"))), "s")
    rows
  }
  workers <- intArg(args[["workers"]], 1L)
  if (workers > 1L && .Platform$OS.type == "windows") stop("Use --workers=1 on Windows")
  results <- if (workers > 1L) parallel::mclapply(tasks, run, mc.cores = min(workers, length(tasks))) else lapply(tasks, run)
  failed <- vapply(results, inherits, logical(1), "try-error")
  if (any(failed)) stop(paste(unlist(results[failed]), collapse = "\n"))
  results <- do.call(rbind, results)
  utils::write.csv(results, file.path(output, "comparison.csv"), row.names = FALSE)
  print(results[, c("task", "method", "auc", "nonzeroPredictors", "tau", "elapsedSeconds")], row.names = FALSE)
  invisible(results)
}

if (sys.nframe() == 0L) benchmarkSparseAveraging(parseArgs())
