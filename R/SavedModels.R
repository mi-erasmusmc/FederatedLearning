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

#' Predict with a saved model on a site
#'
#' Rebuilds the site's matrix the way the fit built it: the saved feature map,
#' covariate combinations (such as a logical OR of indicators), age divided by
#' 100 and an intercept column. The saved coefficients are then applied on
#' that shared scale. A probability ensemble averages its components'
#' predicted probabilities with the saved weights. The predictions are
#' patient-level and must stay where the data are.
#'
#' @param model From [loadModel()].
#' @param sitePath Site data directory.
#' @param taskSettings From [createTaskSettings()]; its population settings
#'   define the evaluated population and outcome.
#' @return One row per patient: `rowId`, `outcome`, `linearPredictor` (`NA`
#'   for a probability ensemble) and `probability`.
#' @export
predictModel <- function(model, sitePath, taskSettings) {
  if (!inherits(model, "comparisonModel")) stop("model must come from loadModel()", call. = FALSE)
  if (!inherits(taskSettings, "taskSettings")) stop("taskSettings must come from createTaskSettings()", call. = FALSE)
  config <- model$config
  if (is.null(config$mapping)) stop("The saved model lacks its feature map", call. = FALSE)
  # Score every patient: no diagnostic downsampling or capture.
  config$diagnosticControlsPerCase <- NULL
  config$surrogateCaptureDirectory <- NULL
  plpData <- loadClientData(sitePath, taskSettings$populationSettings)
  data <- createClientMatrix(plpData, config)
  etas <- lapply(model$models, function(m) {
    coefficients <- m$coefficients[order(m$coefficients$matrixColumn), , drop = FALSE]
    if (nrow(coefficients) != ncol(data$xMatrix)) {
      stop("Saved coefficients do not match the rebuilt matrix", call. = FALSE)
    }
    as.numeric(data$xMatrix %*% coefficients$coefficientSharedScale)
  })
  weights <- if (length(model$models) > 1L) vapply(model$models, `[[`, numeric(1), "weight") else 1
  data.frame(rowId = plpData$population$rowId, outcome = data$yLabels,
    linearPredictor = if (length(etas) == 1L) etas[[1]] else NA_real_,
    probability = Reduce(`+`, Map(function(eta, w) w * stats::plogis(eta), etas, weights)))
}

#' Score a saved model on a site without refitting
#'
#' Scores the predictions of [predictModel()].
#'
#' @inheritParams predictModel
#' @return One row with AUC, log loss, calibration intercept and slope, the
#'   share of nonzero coefficients, patient and outcome counts.
#' @export
evaluateModel <- function(model, sitePath, taskSettings) {
  predictions <- predictModel(model, sitePath, taskSettings)
  density <- function(table) mean(abs(table$coefficient) > 1e-4)
  weights <- if (length(model$models) > 1L) vapply(model$models, `[[`, numeric(1), "weight") else 1
  evaluatePredictions(
    clientData = list(yLabels = predictions$outcome),
    preds = predictions$probability,
    density = sum(weights * vapply(model$models, function(m) density(m$coefficients), numeric(1))),
    clientId = basename(sitePath),
    clientIndex = NA_integer_,
    linearPredictor = if (length(model$models) == 1L) predictions$linearPredictor else NULL
  )
}
