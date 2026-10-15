# Source this script from the repository, then run:
# auditSavedSurrogatePredictions("results/comparison", tasks = "taskName", folds = 1)
# Only held-out data are loaded. No model is fitted and original results are unchanged.
# Run in a fresh R session with the intended FederatedLearning installation.
# Output: summary.csv, code-fingerprints.csv, session-info.txt, README.txt.
# Historical artifacts lack data hashes and the full frozen surrogate state.

.surrogatePredictionSummary <- function(data, coefficients) {
  x <- data$xMatrix
  y <- data$yLabels
  w <- coefficients$coefficient
  if (nrow(x) != length(y) || ncol(x) != length(w) ||
      length(data$n) != 1L || anyNA(data$n) || data$n != length(y) || !length(y) ||
      anyNA(y) || any(!y %in% c(0, 1)) || any(!is.finite(w))) {
    stop("Invalid matrix, coefficient, label, or sample-count dimensions")
  }
  eta <- as.numeric(x %*% w)
  p <- stats::plogis(eta)
  finite <- all(is.finite(eta))
  auc <- function(scores) {
    if (!finite || length(unique(y)) != 2L) return(NA_real_)
    as.numeric(pROC::roc(y, scores, levels = c(0, 1), direction = "<", quiet = TRUE)$auc)
  }
  maxIndex <- which.max(abs(w))
  data.frame(
    n = length(y), outcomes = sum(y), matrixColumns = ncol(x),
    etaNonFinite = sum(!is.finite(eta)),
    etaMin = if (finite) min(eta) else NA_real_,
    etaMax = if (finite) max(eta) else NA_real_,
    probabilityMin = if (finite) min(p) else NA_real_,
    probabilityMax = if (finite) max(p) else NA_real_,
    etaDistinct = if (finite) length(unique(eta)) else NA_integer_,
    probabilityDistinct = if (finite) length(unique(p)) else NA_integer_,
    probabilityZeroFraction = if (finite) mean(p == 0) else NA_real_,
    probabilityOneFraction = if (finite) mean(p == 1) else NA_real_,
    etaAuc = auc(eta), probabilityAuc = auc(p),
    meanLogLoss = if (finite) mean(log1p(exp(-abs(eta))) +
      ifelse(y == 0, pmax(eta, 0), pmax(-eta, 0))) else NA_real_,
    coefficientMaxAbs = abs(w[maxIndex]),
    coefficientMaxId = if (coefficients$isIntercept[maxIndex]) "(Intercept)" else
      as.character(coefficients$covariateId[maxIndex]),
    nonzeroSlopes = sum(w[!coefficients$isIntercept] != 0)
  )
}

.surrogatePredictionInputs <- function(model, row, dataRoot) {
  config <- model$config
  if (length(model$models) != 1L || is.null(config$mapping) ||
      !is.logical(config$intercept) || length(config$intercept) != 1L || is.na(config$intercept)) {
    stop("Audit requires a single federated coefficient vector and its saved feature mapping")
  }
  tab <- model$models[[1]]$coefficients
  expected <- FederatedLearning:::coefficientTable(tab$coefficient, config$mapping, config$intercept)
  columns <- c("matrixColumn", "covariateId", "isIntercept", "normalizationFactor")
  if (!all(columns %in% names(tab)) || anyNA(tab$isIntercept) ||
      !identical(as.numeric(tab$matrixColumn), as.numeric(expected$matrixColumn)) ||
      !identical(as.character(tab$covariateId), expected$covariateId) ||
      !identical(tab$isIntercept, expected$isIntercept) ||
      anyNA(tab$normalizationFactor) || any(tab$normalizationFactor != 1)) {
    stop("Saved coefficients do not match the feature order or shared matrix scale")
  }
  provenance <- model$provenance
  train <- provenance$trainClientIds
  test <- provenance$testClientIds
  validIds <- function(ids) length(ids) > 0L && !anyNA(ids) &&
    all(nzchar(trimws(ids))) && !anyDuplicated(ids)
  if (!validIds(train) || !validIds(test) || any(train %in% test) ||
      !identical(model$testClientIds, test) || !row$clientId %in% test ||
      !is.list(provenance$populationSettings) || !length(provenance$populationSettings)) {
    stop("Missing or inconsistent training/test identities or population settings")
  }
  saved <- model$results
  required <- c("clientId", "n", "outcomes", "auc")
  if (!all(required %in% names(saved))) stop("Saved model has no evaluation counts or AUC")
  saved <- saved[saved$clientId == row$clientId, required, drop = FALSE]
  if (nrow(saved) != 1L || anyNA(saved[c("n", "outcomes")]) ||
      saved$n <= 0 || saved$outcomes < 0 || saved$outcomes > saved$n ||
      !identical(as.character(saved$clientId), as.character(row$clientId)) ||
      !isTRUE(saved$n == row$n && saved$outcomes == row$outcomes) ||
      !isTRUE(all.equal(saved$auc, row$auc, tolerance = 1e-12))) {
    stop("Comparison CSV and saved model disagree on evaluation counts or AUC")
  }
  paths <- provenance$testPaths
  if (!is.null(dataRoot)) paths <- file.path(dataRoot, model$task, test)
  if (length(paths) != length(test) || anyNA(paths)) stop("Saved test paths are missing")
  path <- paths[match(row$clientId, test)]
  if (!dir.exists(path)) stop("Test data path does not exist. Set dataRoot if data have moved")
  if (length(provenance$populationSettings$riskWindowStart) != 1L ||
      length(provenance$populationSettings$riskWindowEnd) != 1L) {
    stop("Saved population settings do not include the risk window")
  }
  config$p <- nrow(config$mapping)
  list(path = path, populationSettings = provenance$populationSettings,
    config = config, coefficients = tab)
}

.surrogatePredictionSite <- function(inputs) {
  cl <- FederatedLearning::clusterInit("localhost", inputs$path, mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  FederatedLearning::clusterLoadData(cl, inputs$path, inputs$populationSettings)
  FederatedLearning::clusterCreateMatrices(cl, inputs$config)
  parallel::clusterCall(cl, function(summarize, coefficients) {
    summarize(get("clientData", envir = .GlobalEnv), coefficients)
  }, summarize = .surrogatePredictionSummary, coefficients = inputs$coefficients)[[1L]]
}

.surrogateAuditFingerprint <- function(object) {
  # Only code and non-patient metadata are passed here, never rows or predictions.
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2, compress = FALSE)
  unname(tools::md5sum(path))
}

# This is a scoring audit of existing federated fits, not a surrogate refit.
# dataRoot, when needed, must contain task/clientId subdirectories.
# Review aggregate outputs under the study's disclosure rules before sharing.
auditSavedSurrogatePredictions <- function(resultDirectory,
                                          outputDirectory = file.path(resultDirectory, "surrogate-prediction-audit"),
                                          tasks = NULL, folds = NULL,
                                          methods = c("ODAL1", "ODAL2"),
                                          featureSet = "ageSexPhenotypes", dataRoot = NULL) {
  resultDirectory <- normalizePath(resultDirectory, winslash = "/", mustWork = TRUE)
  if (file.exists(outputDirectory) || dir.exists(outputDirectory)) {
    stop("Choose a new output directory. Existing files will not be overwritten")
  }
  allowed <- c("ODAL", "ODAL1", "ODAL2", "ADAP", "ADAP1", "ADAP2", "ADAP_PDA", "ADAPDiag")
  if (!length(methods) || anyNA(methods) || any(!methods %in% allowed)) {
    stop("Select supported ODAL/ADAP methods with a single saved coefficient vector")
  }
  csv <- file.path(resultDirectory, "comparison_results.csv")
  manifest <- utils::read.csv(csv, stringsAsFactors = FALSE)
  required <- c("task", "fold", "featureSet", "method", "clientId", "n", "outcomes", "auc",
    "modelFile", "modelId")
  if (!all(required %in% names(manifest))) stop("Comparison results lack saved-model references or evaluation counts")
  rows <- manifest[manifest$method %in% methods & manifest$featureSet %in% featureSet, , drop = FALSE]
  if (!is.null(tasks)) rows <- rows[rows$task %in% tasks, , drop = FALSE]
  if (!is.null(folds)) rows <- rows[rows$fold %in% folds, , drop = FALSE]
  keys <- c("task", "fold", "featureSet", "method", "clientId")
  if (!nrow(rows)) stop("No matching results")
  if (anyNA(rows[keys]) || anyDuplicated(rows[keys])) stop("Expected one result per task/fold/method/test site")
  if (!dir.create(outputDirectory, recursive = TRUE)) stop("Could not create output directory")

  ns <- asNamespace("FederatedLearning")
  dll <- getLoadedDLLs()[["FederatedLearning"]][["path"]]
  code <- data.frame(
    component = c("comparison_results.csv", "FederatedLearning DLL", "loadClientData", "createClientMatrix",
      "audit script functions"),
    md5 = c(unname(tools::md5sum(csv)), unname(tools::md5sum(dll)),
      .surrogateAuditFingerprint(body(get("loadClientData", ns))),
      .surrogateAuditFingerprint(body(get("createClientMatrix", ns))),
      .surrogateAuditFingerprint(lapply(list(auditSavedSurrogatePredictions, .surrogatePredictionSummary,
        .surrogatePredictionInputs, .surrogatePredictionSite, .surrogateAuditFingerprint), body))))
  utils::write.csv(code, file.path(outputDirectory, "code-fingerprints.csv"), row.names = FALSE)
  writeLines(capture.output(utils::sessionInfo()), file.path(outputDirectory, "session-info.txt"))
  writeLines(c(
    "Existing saved coefficients rescored without fitting. No training data were loaded.",
    "AUC uses event=1 and increasing scores, on the same reconstructed rows for logits and probabilities.",
    "Saved feature order, population settings, and the loader's age/100 transformation were reused.",
    "Counts matching does NOT establish historical row/matrix identity. Historical data hashes were not saved.",
    "Historical schema-1 artifacts do not contain the frozen expansion point and full correction derivatives.",
    "This audit does not reconstruct, refit, or certify the original surrogate or its optimum.",
    "Distinct-score counts and exact 0/1 probability fractions expose saturation and ties.",
    "probabilityAucDifference compares rescored probabilities to the recorded AUC, not to a refit.",
    "Code/DLL fingerprints and session info describe this audit environment, not the historical fitting environment.",
    "Review summaries, counts, score ranges, and session metadata for disclosure before sharing.",
    "No patient-level scores, labels, feature rows, or identifiers are written. No upload is performed."
  ), file.path(outputDirectory, "README.txt"))

  summaries <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    message("Scoring audit: ", row$task, " / fold ", row$fold, " / ", row$method, " / ", row$clientId)
    current <- cbind(row[keys], data.frame(modelId = row$modelId, status = "error", error = NA_character_,
      recordedAuc = row$auc, recordedN = row$n, recordedOutcomes = row$outcomes,
      modelMd5 = NA_character_, mappingMd5 = NA_character_, populationSettingsMd5 = NA_character_,
      riskWindowStart = NA_real_, riskWindowEnd = NA_real_, countsMatch = NA,
      probabilityAucDifference = NA_real_, etaMinusProbabilityAuc = NA_real_,
      historicalDataIdentity = "unverified_no_saved_data_hash", frozenSurrogate = "unavailable_not_saved"))
    tryCatch({
      model <- FederatedLearning:::readModelArtifact(row, resultDirectory,
        row$task, row$fold, row$featureSet, row$method)
      if (is.null(model)) stop("Saved model is missing, unreadable, or inconsistent with this row")
      current$modelMd5 <- unname(tools::md5sum(file.path(resultDirectory, row$modelFile)))
      inputs <- .surrogatePredictionInputs(model, row, dataRoot)
      current$mappingMd5 <- .surrogateAuditFingerprint(inputs$config$mapping)
      current$populationSettingsMd5 <- .surrogateAuditFingerprint(inputs$populationSettings)
      current$riskWindowStart <- inputs$populationSettings$riskWindowStart
      current$riskWindowEnd <- inputs$populationSettings$riskWindowEnd
      scores <- .surrogatePredictionSite(inputs)
      current[names(scores)] <- scores
      current$countsMatch <- scores$n == row$n && scores$outcomes == row$outcomes
      current$probabilityAucDifference <- scores$probabilityAuc - row$auc
      current$etaMinusProbabilityAuc <- scores$etaAuc - scores$probabilityAuc
      current$status <- if (!current$countsMatch) "count_mismatch" else if (scores$etaNonFinite > 0) {
        "nonfinite_predictions"
      } else if (!is.finite(scores$etaAuc)) "single_class" else "rescored"
    }, error = function(e) current$error <<- conditionMessage(e))
    summaries[[i]] <- current
    utils::write.csv(dplyr::bind_rows(summaries[seq_len(i)]),
      file.path(outputDirectory, "summary.csv"), row.names = FALSE)
    message("  ", current$status, if (!is.na(current$error)) paste0(": ", current$error) else "")
  }
  message("No models were fitted. Review before sharing: ", file.path(outputDirectory, "summary.csv"))
  invisible(dplyr::bind_rows(summaries))
}
