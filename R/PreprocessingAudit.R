#' Audit matched feature filtering against saved pooled models
#'
#' Reconstructs training matrices using the settings stored in each saved
#' PooledLasso model. No models are fitted and held-out data are not loaded.
#' Writes summary.csv and features.csv for review before a matched-filtering run.
#' Errors in individual folds are recorded in the summary and do not discard
#' completed folds. A matched status confirms feature masks and training counts,
#' not that every value in the underlying data is unchanged.
#'
#' @param resultDirectory Previous comparison directory containing
#'   comparison_results.csv and saved models.
#' @param outputDirectory Audit output directory. Defaults to a new
#'   preprocessing-audit subdirectory within resultDirectory.
#' @param tasks Optional vector of task names. Defaults to all matching tasks.
#' @param folds Optional vector of held-out fold numbers. Defaults to all folds.
#' @param featureSet Feature set to audit. Defaults to ageSexPhenotypes.
#' @param dataRoot Optional replacement data root if data have moved. It must
#'   contain task/clientId subdirectories. Otherwise saved training paths are used.
#' @return Invisibly, a data frame of per-fold audit results.
#' @export
auditMatchedPreprocessing <- function(resultDirectory,
                                     outputDirectory = file.path(resultDirectory, "preprocessing-audit"),
                                     tasks = NULL, folds = NULL,
                                     featureSet = "ageSexPhenotypes", dataRoot = NULL) {
  resultDirectory <- normalizePath(resultDirectory, winslash = "/", mustWork = TRUE)
  manifest <- utils::read.csv(file.path(resultDirectory, "comparison_results.csv"), stringsAsFactors = FALSE)
  required <- c("task", "fold", "featureSet", "method", "modelFile", "modelId")
  if (!all(required %in% names(manifest))) stop("Comparison results must include saved model references")
  rows <- manifest[manifest$method == "PooledLasso" & manifest$featureSet == featureSet, , drop = FALSE]
  if (!is.null(tasks)) {
    if (any(!tasks %in% rows$task)) stop("Requested tasks have no pooled results for this feature set")
    rows <- rows[rows$task %in% tasks, , drop = FALSE]
  }
  if (!is.null(folds)) rows <- rows[rows$fold %in% folds, , drop = FALSE]
  if (!nrow(rows)) stop("No pooled results found for the requested tasks, folds and feature set")
  if (anyNA(rows[c("task", "fold", "featureSet")]) ||
      anyDuplicated(rows[c("task", "fold", "featureSet")])) {
    stop("Expected one pooled model per task, fold and feature set")
  }
  dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
  outputDirectory <- normalizePath(outputDirectory, winslash = "/", mustWork = TRUE)
  if (identical(outputDirectory, resultDirectory)) stop("Audit output must be separate from the original results")

  summary <- details <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    message("Auditing ", row$task, " / fold ", row$fold)
    current <- data.frame(task = row$task, fold = row$fold, featureSet = row$featureSet,
      riskWindowEnd = NA_integer_, candidatePredictors = NA_integer_, retainedPredictors = NA_integer_,
      requiresRefit = NA, referenceStatus = "error", error = NA_character_, modelId = row$modelId)
    tryCatch({
      model <- readModelArtifact(row, resultDirectory, row$task, row$fold, row$featureSet, "PooledLasso")
      if (is.null(model)) stop("Saved pooled model is missing, unreadable, or does not match this row")
      current$riskWindowEnd <- model$populationSettings$riskWindowEnd %||% NA_integer_
      p <- auditSavedPreprocessing(model, row, dataRoot)
      current$candidatePredictors <- nrow(p$audit)
      current$retainedPredictors <- sum(p$audit$retained)
      current$requiresRefit <- any(!p$audit$retained)
      current$referenceStatus <- if (preprocessingMatchesModel(p, model, trimws(model$trainClientIds)))
        "matched" else "mismatch"
      if (current$referenceStatus == "mismatch") {
        current$error <- "Feature map, retained mask, or per-site training counts differ from the saved pooled model"
      }
      details[[i]] <- cbind(task = row$task, fold = row$fold, featureSet = row$featureSet,
        riskWindowEnd = current$riskWindowEnd, fingerprint = p$fingerprint, p$audit)
    }, error = function(e) {
      current$error <<- conditionMessage(e)
    })
    summary[[i]] <- current
    utils::write.csv(do.call(rbind, summary), file.path(outputDirectory, "summary.csv"), row.names = FALSE)
    featureRows <- if (length(details)) do.call(rbind, details) else data.frame()
    utils::write.csv(featureRows, file.path(outputDirectory, "features.csv"), row.names = FALSE)
    message("  ", current$referenceStatus, if (!is.na(current$error)) paste0(": ", current$error) else "")
  }
  summary <- do.call(rbind, summary)
  message("Audit complete: ", sum(summary$referenceStatus == "matched"), "/", nrow(summary),
    " matched. No models were fitted.\nShare: ", file.path(outputDirectory, "summary.csv"))
  invisible(summary)
}

auditSavedPreprocessing <- function(model, row, dataRoot) {
  settings <- model$preprocessing$settings
  if (!isTRUE(settings$enabled) || !identical(settings$normalize, FALSE) ||
      length(settings$minFraction) != 1L ||
      !is.finite(settings$minFraction) || settings$minFraction < 0 || settings$minFraction > 1) {
    stop("Saved baseline must use enabled feature filtering without additional normalization")
  }
  ids <- trimws(model$trainClientIds)
  if (!length(ids) || anyNA(ids) || anyDuplicated(ids) || any(ids %in% trimws(model$testClientIds))) {
    stop("Saved model has invalid training/test site identities")
  }
  paths <- if (is.null(dataRoot)) model$trainPaths else file.path(dataRoot, row$task, ids)
  if (length(paths) != length(ids) || anyNA(paths) || !all(dir.exists(paths))) {
    stop("Training data paths are missing. Supply dataRoot if the data have moved")
  }
  if (is.null(model$populationSettings) || is.null(model$config$mapType) ||
      is.null(model$config$intercept) || is.null(model$originalMapping) ||
      length(model$preprocessing$keep) != nrow(model$originalMapping)) {
    stop("Saved model lacks population, feature-map, or preprocessing metadata")
  }
  config <- model$config
  config$featureSet <- row$featureSet
  cl <- clusterInit(rep("localhost", length(ids)), paths, mirai = FALSE)
  on.exit(stopCluster(cl), add = TRUE)
  clusterLoadData(cl, paths, model$populationSettings)
  collectTrainingFeatureFilter(cl, config, settings)
}
