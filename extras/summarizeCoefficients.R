# Export exact coefficient support without loading patient data or refitting models.
# Rscript extras/summarizeCoefficients.R --result-directory=results/comparisonMatrix
# Existing DualAvg debug fits are accepted only if their evaluation matches the CSV.

legacyDebugPath <- function(resultDirectory, task, fold, featureSet, method) {
  parts <- vapply(list(task, paste0("fold", fold), featureSet, method, "fit"),
    FederatedLearning:::safeFilePart, character(1))
  file.path(resultDirectory, "debug", paste0(paste(parts, collapse = "_"), ".rds"))
}

readMatchingDebugFit <- function(rows, resultDirectory, task, fold, featureSet, method) {
  if (is.null(resultDirectory) || nrow(rows) != 1L || method != "DualAvg") return(NULL)
  path <- legacyDebugPath(resultDirectory, task, fold, featureSet, method)
  if (!file.exists(path)) return(NULL)
  tryCatch({
    fit <- readRDS(path)
    keys <- list(task = task, fold = fold, featureSet = featureSet, method = method)
    metrics <- c("auc", "n", "outcomes", "clientId")
    if (!is.list(fit) || !is.data.frame(fit$evaluation) || nrow(fit$evaluation) != 1L ||
        !all(metrics %in% names(rows)) || !all(metrics %in% names(fit$evaluation)) ||
        !all(vapply(names(keys), function(k) identical(as.character(fit[[k]]), as.character(keys[[k]])), logical(1))) ||
        !all(vapply(metrics, function(k) isTRUE(all.equal(fit$evaluation[[k]], rows[[k]],
          tolerance = 1e-12, check.attributes = FALSE)), logical(1))) ||
        length(fit$coefficients) != rows$p) return(NULL)
    lambda <- fit$selectedLambda
    if (is.null(lambda)) lambda <- fit$config[["lambda", exact = TRUE]]
    expected <- rows$selectedLambda
    if (length(lambda) != 1L || length(expected) != 1L ||
        !(is.na(lambda) && is.na(expected) || is.finite(lambda) && is.finite(expected) &&
          abs(lambda - expected) <= 1e-12 * max(abs(lambda), abs(expected), .Machine$double.xmin))) return(NULL)
    FederatedLearning:::coefficientTable(fit$coefficients, fit$config$mapping, fit$config$intercept)
    fit
  }, error = function(e) NULL)
}

summarizeCoefficients <- function(resultDirectory,
                                 methods = c("PooledLasso", "DualAvg", "LocalAvgLasso",
                                   "SparseLocalAvgLasso", "DebiasedLocalAvgLasso"),
                                 outputDirectory = file.path(resultDirectory, "coefficient_summary")) {
  if (!all(methods %in% c("PooledLasso", "DualAvg", "LocalAvgLasso",
      "SparseLocalAvgLasso", "DebiasedLocalAvgLasso"))) {
    stop("This export supports PooledLasso, DualAvg and coefficient averaging baselines")
  }
  rows <- utils::read.csv(file.path(resultDirectory, "comparison_results.csv"), stringsAsFactors = FALSE)
  rows <- rows[rows$method %in% methods, , drop = FALSE]
  keys <- c("task", "fold", "featureSet", "method")
  combinations <- unique(rows[, keys, drop = FALSE])
  summaries <- list()
  coefficients <- list()
  cvFolds <- list()
  for (i in seq_len(nrow(combinations))) {
    key <- combinations[i, , drop = FALSE]
    selected <- rows[FederatedLearning:::matchingCombination(rows, key$task, key$fold, key$featureSet, key$method), , drop = FALSE]
    tab <- NULL
    status <- "missing_model"
    config <- list()
    successful <- any(FederatedLearning:::successfulRows(selected))
    artifact <- if (successful && nrow(selected) == 1L) {
      FederatedLearning:::readModelArtifact(selected, resultDirectory,
        key$task, key$fold, key$featureSet, key$method)
    } else NULL
    if (!successful) {
      status <- "fit_failed"
    } else if (nrow(selected) != 1L) {
      status <- "ambiguous_result_rows"
    } else if (!is.null(artifact)) {
      tab <- artifact$models[[1]]$coefficients
      config <- artifact$config
      status <- "saved_model"
    } else if (key$method == "DualAvg") {
      path <- legacyDebugPath(resultDirectory, key$task, key$fold, key$featureSet, key$method)
      if (file.exists(path)) {
        legacy <- readMatchingDebugFit(selected, resultDirectory, key$task, key$fold, key$featureSet, key$method)
        if (!is.null(legacy)) {
          tab <- FederatedLearning:::coefficientTable(legacy$coefficients, legacy$config$mapping, legacy$config$intercept)
          config <- legacy$config
          status <- "saved_debug"
        } else {
          status <- "stale_or_invalid_debug"
        }
      }
    }
    predictors <- if (!is.null(tab)) tab[!tab$isIntercept, , drop = FALSE] else NULL
    nonzero <- if (!is.null(tab)) predictors$coefficient[predictors$coefficient != 0] else numeric()
    variance <- if (key$method == "DualAvg") config$selectedVariance else
      if (key$method == "PooledLasso" && nrow(selected) == 1L) selected$selectedLambda else NULL
    summaries[[i]] <- cbind(key, data.frame(
      status = status,
      predictorCount = if (is.null(tab)) NA_integer_ else nrow(predictors),
      nonzeroPredictors = if (is.null(tab)) NA_integer_ else length(nonzero),
      minAbsNonzero = if (length(nonzero)) min(abs(nonzero)) else NA_real_,
      selectedLambda = if (nrow(selected) == 1L) selected$selectedLambda else NA_real_,
      aggregationThreshold = if (nrow(selected) == 1L && "aggregationThreshold" %in% names(selected))
        selected$aggregationThreshold else NA_real_,
      penaltyScale = if (key$method == "DualAvg") "mean negative log-likelihood L1 multiplier" else "Cyclops Laplace prior variance",
      selectedVariance = if (is.null(variance)) NA_real_ else variance,
      stringsAsFactors = FALSE
    ))
    if (!is.null(tab)) coefficients[[length(coefficients) + 1L]] <- cbind(key[rep(1L, nrow(tab)), , drop = FALSE], tab)
    trace <- config$lambdaSearchTrace
    if (is.data.frame(trace) && nrow(trace) > 0L) {
      trace$selected <- if (is.null(variance)) NA else trace$searchValue == variance
      trace$finalLambda <- selected$selectedLambda
      cvFolds[[length(cvFolds) + 1L]] <- cbind(key[rep(1L, nrow(trace)), , drop = FALSE], trace)
    }
  }
  summary <- if (length(summaries)) do.call(rbind, summaries) else data.frame()
  coef <- if (length(coefficients)) do.call(rbind, coefficients) else data.frame()
  cv <- if (length(cvFolds)) do.call(rbind, cvFolds) else data.frame()
  dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, file.path(outputDirectory, "coefficient_summary.csv"), row.names = FALSE)
  utils::write.csv(coef, file.path(outputDirectory, "coefficients.csv"), row.names = FALSE)
  utils::write.csv(cv, file.path(outputDirectory, "lambda_search_folds.csv"), row.names = FALSE)
  message("Exact nonzero counts exclude the intercept; no magnitude threshold is used.")
  if (nrow(summary)) print(table(summary$status))
  message("Coefficient reports: ", outputDirectory)
  invisible(list(summary = summary, coefficients = coef, cvFolds = cv))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  getArg <- function(name, default = NULL) {
    prefix <- paste0("--", name, "=")
    matches <- args[startsWith(args, prefix)]
    if (length(matches)) substring(tail(matches, 1L), nchar(prefix) + 1L) else default
  }
  directory <- getArg("result-directory")
  if (is.null(directory)) stop("Supply --result-directory")
  summarizeCoefficients(directory,
    methods = trimws(strsplit(getArg("methods", "PooledLasso,DualAvg,LocalAvgLasso,SparseLocalAvgLasso,DebiasedLocalAvgLasso"), ",", fixed = TRUE)[[1]]),
    outputDirectory = getArg("output-directory", file.path(directory, "coefficient_summary"))
  )
}
