#' Load a saved comparison model
#'
#' @param outputDirectory A [runComparison()] result directory.
#' @param task Task name.
#' @param holdout Name of the held-out site.
#' @param feature Feature-settings label.
#' @param method Method name.
#' @return A `comparisonModel`: the validated model artifact, whose `models`
#'   element holds each component's coefficient table, plus the feature
#'   filter, settings and fit diagnostics.
#' @export
loadModel <- function(outputDirectory, task, holdout, feature, method) {
  results <- readComparisonResults(outputDirectory)
  rows <- results[results$task == task & results$featureSet == feature & results$method == method &
    results$clientId == holdout, , drop = FALSE]
  if (!nrow(rows)) {
    stop("No result for task ", task, ", holdout ", holdout, ", feature ", feature,
      " and method ", method, call. = FALSE)
  }
  fold <- unique(rows$fold)
  if (length(fold) != 1L) stop("Results for this model span several folds", call. = FALSE)
  rows <- results[results$task == task & results$featureSet == feature & results$method == method &
    results$fold == fold, , drop = FALSE]
  artifact <- readModelArtifact(rows, outputDirectory, task, fold, feature, method)
  if (is.null(artifact)) stop("The saved model is missing or invalid", call. = FALSE)
  structure(artifact, class = c("comparisonModel", "list"))
}

#' Score a saved model on a site without refitting
#'
#' Applies the saved coefficients to the site's covariates on the shared
#' matrix scale (age divided by 100). A probability ensemble averages its
#' components' predicted probabilities with the saved weights.
#'
#' @param model From [loadModel()].
#' @param sitePath Site data directory.
#' @param taskSettings From [createTaskSettings()]; its population settings
#'   define the evaluated population and outcome.
#' @return One row with AUC, log loss, calibration intercept and slope, the
#'   share of nonzero coefficients, patient and outcome counts.
#' @export
evaluateModel <- function(model, sitePath, taskSettings) {
  if (!inherits(model, "comparisonModel")) stop("model must come from loadModel()", call. = FALSE)
  if (!inherits(taskSettings, "taskSettings")) stop("taskSettings must come from createTaskSettings()", call. = FALSE)
  plpData <- loadClientData(sitePath, taskSettings$populationSettings)
  population <- plpData$population
  covariates <- as.data.frame(dplyr::collect(plpData$covariateData$covariates))
  covariates <- covariates[covariates$rowId %in% population$rowId, , drop = FALSE]
  row <- match(covariates$rowId, population$rowId)
  covariateId <- as.character(covariates$covariateId)

  linearPredictor <- function(table) {
    slopes <- table[!table$isIntercept, , drop = FALSE]
    coefficient <- slopes$coefficientSharedScale[match(covariateId, slopes$covariateId)]
    present <- !is.na(coefficient)
    eta <- rep(sum(table$coefficientSharedScale[table$isIntercept]), nrow(population))
    contribution <- rowsum(coefficient[present] * covariates$covariateValue[present], row[present])
    eta[as.integer(rownames(contribution))] <- eta[as.integer(rownames(contribution))] + contribution[, 1]
    eta
  }
  density <- function(table) mean(abs(table$coefficient) > 1e-4)
  weights <- if (length(model$models) > 1L) vapply(model$models, `[[`, numeric(1), "weight") else 1
  etas <- lapply(model$models, function(m) linearPredictor(m$coefficients))
  preds <- Reduce(`+`, Map(function(eta, w) w * stats::plogis(eta), etas, weights))
  evaluatePredictions(
    clientData = list(yLabels = as.integer(population$outcomeCount > 0)),
    preds = preds,
    density = sum(weights * vapply(model$models, function(m) density(m$coefficients), numeric(1))),
    clientId = basename(sitePath),
    clientIndex = NA_integer_,
    linearPredictor = if (length(etas) == 1L) etas[[1]] else NULL
  )
}
