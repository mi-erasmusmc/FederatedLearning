# Export exact coefficient support without loading patient data or refitting models.
# Rscript extras/summarizeCoefficients.R --result-directory=results/comparisonMatrix
# Existing DualAvg debug fits are accepted only if their evaluation matches the CSV.

summarizeCoefficients <- function(resultDirectory,
                                 methods = c("PooledLasso", "DualAvg", "LocalAvgLasso"),
                                 outputDirectory = file.path(resultDirectory, "coefficient_summary"),
                                 runnerPath = file.path("extras", "runComparisonMatrix.R")) {
  if (!all(methods %in% c("PooledLasso", "DualAvg", "LocalAvgLasso"))) {
    stop("This export supports PooledLasso, DualAvg and LocalAvgLasso")
  }
  runner <- new.env(parent = globalenv())
  sys.source(runnerPath, runner)
  rows <- utils::read.csv(file.path(resultDirectory, "comparison_results.csv"), stringsAsFactors = FALSE)
  rows <- rows[rows$method %in% methods, , drop = FALSE]
  keys <- c("task", "fold", "featureSet", "method")
  combinations <- unique(rows[, keys, drop = FALSE])
  summaries <- list()
  coefficients <- list()
  for (i in seq_len(nrow(combinations))) {
    key <- combinations[i, , drop = FALSE]
    selected <- rows[runner$matchingCombination(rows, key$task, key$fold, key$featureSet, key$method), , drop = FALSE]
    tab <- NULL
    status <- "missing_model"
    config <- list()
    if (!any(runner$successfulRows(selected))) {
      status <- "fit_failed"
    } else if (nrow(selected) != 1L) {
      status <- "ambiguous_result_rows"
    } else if (runner$hasSavedModel(selected, resultDirectory, key$task, key$fold, key$featureSet, key$method)) {
      artifact <- readRDS(file.path(resultDirectory, selected$modelFile))
      tab <- artifact$models[[1]]$coefficients
      config <- artifact$config
      status <- "saved_model"
    } else if (key$method == "DualAvg") {
      path <- runner$debugPath(file.path(resultDirectory, "debug"), key$task,
        key$fold, key$featureSet, key$method, "fit")
      if (file.exists(path)) {
        legacy <- runner$readMatchingDebugFit(selected, resultDirectory, key$task, key$fold, key$featureSet, key$method)
        if (!is.null(legacy)) {
          tab <- runner$coefficientTable(legacy$coefficients, legacy$config$mapping, legacy$config$intercept)
          config <- legacy$config
          status <- "saved_debug"
        } else {
          status <- "stale_or_invalid_debug"
        }
      }
    }
    predictors <- if (!is.null(tab)) tab[!tab$isIntercept, , drop = FALSE] else NULL
    nonzero <- if (!is.null(tab)) predictors$coefficient[predictors$coefficient != 0] else numeric()
    summaries[[i]] <- cbind(key, data.frame(
      status = status,
      predictorCount = if (is.null(tab)) NA_integer_ else nrow(predictors),
      nonzeroPredictors = if (is.null(tab)) NA_integer_ else length(nonzero),
      minAbsNonzero = if (length(nonzero)) min(abs(nonzero)) else NA_real_,
      selectedLambda = if (nrow(selected) == 1L) selected$selectedLambda else NA_real_,
      penaltyScale = if (key$method == "DualAvg") "mean negative log-likelihood L1 multiplier" else "Cyclops Laplace prior variance",
      innerCvFitLambda = if (is.null(config$lambdaSearchBestTrain)) NA_real_ else config$lambdaSearchBestTrain,
      finalToInnerCvLambdaRatio = if (is.null(config$lambdaSearchBestTrain) ||
        !is.finite(config$lambdaSearchBestTrain) || config$lambdaSearchBestTrain <= 0) NA_real_ else
          selected$selectedLambda / config$lambdaSearchBestTrain,
      stringsAsFactors = FALSE
    ))
    if (!is.null(tab)) coefficients[[length(coefficients) + 1L]] <- cbind(key[rep(1L, nrow(tab)), , drop = FALSE], tab)
  }
  summary <- if (length(summaries)) do.call(rbind, summaries) else data.frame()
  coef <- if (length(coefficients)) do.call(rbind, coefficients) else data.frame()
  dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, file.path(outputDirectory, "coefficient_summary.csv"), row.names = FALSE)
  utils::write.csv(coef, file.path(outputDirectory, "coefficients.csv"), row.names = FALSE)
  message("Exact nonzero counts exclude the intercept; no magnitude threshold is used.")
  if (nrow(summary)) print(table(summary$status))
  message("Coefficient reports: ", outputDirectory)
  invisible(list(summary = summary, coefficients = coef))
}

if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
  runnerPath <- file.path(dirname(script), "runComparisonMatrix.R")
  runner <- new.env(parent = globalenv())
  sys.source(runnerPath, runner)
  args <- runner$parseArgs()
  directory <- args[["result-directory"]]
  if (is.null(directory)) stop("Supply --result-directory")
  summarizeCoefficients(directory,
    methods = runner$csvArg(args[["methods"]], c("PooledLasso", "DualAvg", "LocalAvgLasso")),
    outputDirectory = if (is.null(args[["output-directory"]])) file.path(directory, "coefficient_summary") else args[["output-directory"]],
    runnerPath = runnerPath
  )
}
