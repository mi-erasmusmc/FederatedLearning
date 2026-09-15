matchingCombination <- function(rows, task, fold, featureSet, method) {
  if (is.null(rows) || nrow(rows) == 0L) {
    return(logical())
  }
  required <- c("task", "fold", "featureSet", "method")
  if (!all(required %in% names(rows))) {
    return(rep(FALSE, nrow(rows)))
  }
  rows$task == task &
    rows$fold == fold &
    rows$featureSet == featureSet &
    rows$method == method
}

successfulRows <- function(rows) {
  if (!"error" %in% names(rows)) {
    return(rep(TRUE, nrow(rows)))
  }
  is.na(rows$error) | !nzchar(rows$error)
}

safeFilePart <- function(x) {
  gsub("[^A-Za-z0-9_.-]+", "-", as.character(x))
}

coefficientTable <- function(w, mapping, intercept, preprocessor = NULL) {
  if (!is.numeric(w) || any(!is.finite(w))) {
    stop("Cannot save non-finite or non-numeric coefficients")
  }
  w <- as.numeric(w)
  if (!all(c("covariateId", "columnId") %in% names(mapping)) ||
      anyNA(mapping$covariateId) || anyDuplicated(mapping$covariateId) ||
      !identical(sort(as.integer(mapping$columnId)), seq_len(nrow(mapping)))) {
    stop("Invalid coefficient feature mapping")
  }
  mapping <- mapping[order(mapping$columnId), , drop = FALSE]
  factors <- rep(1, nrow(mapping))
  if (isTRUE(preprocessor$enabled)) {
    if (length(preprocessor$keep) != nrow(mapping) || anyNA(preprocessor$keep) ||
        length(preprocessor$normFactors) != nrow(mapping)) {
      stop("Preprocessing mask does not match the coefficient feature mapping")
    }
    factors <- preprocessor$normFactors[preprocessor$keep]
    mapping <- mapping[preprocessor$keep, , drop = FALSE]
  }
  if (length(w) != nrow(mapping) + as.integer(intercept) ||
      any(!is.finite(factors) | factors == 0)) {
    stop("Coefficient dimensions or normalization factors do not match the feature mapping")
  }
  factors <- c(if (intercept) 1, factors)
  data.frame(
    matrixColumn = seq_along(w),
    covariateId = c(if (intercept) NA_character_, as.character(mapping$covariateId)),
    isIntercept = c(if (intercept) TRUE, rep(FALSE, nrow(mapping))),
    coefficient = as.numeric(w),
    normalizationFactor = factors,
    # Shared matrix scale, including the loader's age/100 transformation.
    coefficientSharedScale = as.numeric(w) / factors,
    selected = w != 0,
    stringsAsFactors = FALSE
  )
}

exactNonzero <- function(w, intercept) {
  if (any(!is.finite(w))) return(NA_integer_)
  if (isTRUE(intercept)) w <- w[-1L]
  sum(w != 0)
}

saveModelArtifact <- function(artifact, rows, modelDirectory) {
  if (is.null(modelDirectory)) return(rows)
  rows$modelFile <- NA_character_
  rows$modelId <- NA_character_
  rows$modelSaveError <- NA_character_
  temporary <- NULL
  on.exit(if (!is.null(temporary)) unlink(temporary), add = TRUE)
  tryCatch({
    if (!dir.exists(modelDirectory) &&
        !dir.create(modelDirectory, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create model directory: ", modelDirectory)
    }
    prefix <- paste(vapply(list(artifact$task, paste0("fold", artifact$fold),
      artifact$featureSet, artifact$method), safeFilePart, character(1)), collapse = "_")
    target <- tempfile(paste0(prefix, "_"), tmpdir = modelDirectory, fileext = ".rds")
    artifact$schemaVersion <- 1L
    artifact$modelId <- basename(target)
    artifact$createdAt <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    savedRows <- rows
    savedRows$modelFile <- file.path(basename(modelDirectory), basename(target))
    savedRows$modelId <- artifact$modelId
    # End-to-end elapsedSeconds is stamped by the runner after this write.
    artifact$results <- savedRows[, setdiff(names(savedRows), "elapsedSeconds"), drop = FALSE]
    artifact$sessionInfo <- utils::sessionInfo()
    temporary <- tempfile(tmpdir = modelDirectory)
    saveRDS(artifact, temporary)
    if (!file.rename(temporary, target)) stop("Could not save model artifact: ", target)
    savedRows
  }, error = function(e) {
    rows$modelSaveError <- conditionMessage(e)
    message("Model artifact was not saved: ", rows$modelSaveError[[1]])
    rows
  })
}

readModelArtifact <- function(rows, resultDirectory, task, fold, featureSet, method) {
  if (is.null(resultDirectory) ||
      !all(c("modelFile", "modelId") %in% names(rows))) return(NULL)
  refs <- unique(rows[, c("modelFile", "modelId"), drop = FALSE])
  if (nrow(refs) != 1L || anyNA(refs) || any(!nzchar(unlist(refs)))) return(NULL)
  path <- file.path(resultDirectory, refs$modelFile)
  if (!file.exists(path)) return(NULL)
  artifact <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(artifact)) return(NULL)
  valid <- identical(artifact$schemaVersion, 1L) &&
    identical(artifact$modelId, refs$modelId[[1]]) &&
    identical(as.character(artifact$task), as.character(task)) &&
    identical(as.integer(artifact$fold), as.integer(fold)) &&
    identical(as.character(artifact$featureSet), as.character(featureSet)) &&
    identical(as.character(artifact$method), as.character(method)) &&
    length(artifact$models) > 0L &&
    all(vapply(artifact$models, function(model) {
      if (!is.list(model)) return(FALSE)
      tab <- model$coefficients
      is.data.frame(tab) && all(c("coefficient", "covariateId", "isIntercept") %in% names(tab)) &&
        nrow(tab) > 0L && is.numeric(tab$coefficient) && all(is.finite(tab$coefficient))
    }, logical(1)))
  if (valid) artifact else NULL
}
