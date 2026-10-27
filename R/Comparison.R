#' Compare methods across held-out sites
#'
#' For each site in `holdouts`, trains every method on the other sites with
#' every feature setting and evaluates it on the held-out site. Results, model
#' artifacts and the shared feature-filter audit are written to
#' `outputDirectory`. Combinations already completed there are reused when
#' `resume` is `TRUE`.
#'
#' Each call adds a row to `run_manifest.csv` in `outputDirectory`, with the
#' framework and study package versions, R, the BLAS, a hash of the settings
#' and start and finish times; `manifest/` keeps the full settings and session.
#' Resuming with other code or settings gives a warning.
#'
#' Methods with identical options run together. Methods with different
#' options run separately, so an option given to one method never reaches
#' another.
#'
#' @param taskSettings From [createTaskSettings()].
#' @param sites Named character vector of site data directories.
#' @param featureSettings Named list of [createFeatureSettings()] objects. The
#'   names label the feature settings in results. All must share `minFraction`.
#' @param methodSettings A [createMethodSettings()] object or a list of them,
#'   with distinct methods.
#' @param outputDirectory Result directory.
#' @param holdouts Names of the sites to hold out, one outer fold each.
#' @param resume Reuse completed combinations in `outputDirectory`.
#' @param runSettings From [createRunSettings()].
#' @return The results table, invisibly. With `runSettings$auditOnly`, the
#'   feature-filter summary instead: candidate, retained and removed
#'   predictors for each task, held-out site and feature setting.
#' @export
runComparison <- function(taskSettings, sites, featureSettings, methodSettings, outputDirectory,
                          holdouts = names(sites), resume = TRUE,
                          runSettings = createRunSettings()) {
  if (!inherits(taskSettings, "taskSettings")) stop("taskSettings must come from createTaskSettings()", call. = FALSE)
  if (!is.character(sites) || is.null(names(sites)) || anyDuplicated(names(sites)) || any(!nzchar(names(sites)))) {
    stop("sites must be a character vector with unique site names", call. = FALSE)
  }
  if (!is.list(featureSettings) || is.null(names(featureSettings)) || anyDuplicated(names(featureSettings)) ||
      !all(vapply(featureSettings, inherits, logical(1), "featureSettings"))) {
    stop("featureSettings must be a named list of createFeatureSettings() objects", call. = FALSE)
  }
  minFraction <- unique(lapply(featureSettings, `[[`, "minFraction"))
  if (length(minFraction) != 1L) stop("All feature settings must share minFraction", call. = FALSE)
  minFraction <- minFraction[[1]]
  if (inherits(methodSettings, "methodSettings")) methodSettings <- list(methodSettings)
  if (!all(vapply(methodSettings, inherits, logical(1), "methodSettings"))) {
    stop("methodSettings must be createMethodSettings() objects", call. = FALSE)
  }
  methods <- vapply(methodSettings, `[[`, "", "method")
  if (anyDuplicated(methods)) stop("Each method can appear only once", call. = FALSE)
  if (!length(holdouts) || !all(holdouts %in% names(sites))) {
    stop("holdouts must name sites in sites", call. = FALSE)
  }
  if (!inherits(runSettings, "runSettings")) {
    stop("runSettings must come from createRunSettings()", call. = FALSE)
  }

  base <- list(
    tasks = taskSettings$name,
    clients = as.character(length(sites)),
    folds = .flagValue(match(holdouts, names(sites))),
    `feature-sets` = paste(names(featureSettings), collapse = ","),
    `result-directory` = outputDirectory,
    resume = .flagValue(resume),
    `federated-preprocess` = if (is.null(minFraction)) "none" else "baseline",
    hosts = .flagValue(runSettings$hosts),
    mirai = .flagValue(runSettings$mirai),
    verbose = .flagValue(runSettings$verbose),
    `debug-diagnostics` = .flagValue(runSettings$debug),
    `preprocessing-audit-only` = .flagValue(runSettings$auditOnly),
    `rerun-errors` = .flagValue(runSettings$rerunErrors),
    `rerun-missing-models` = .flagValue(runSettings$rerunMissingModels),
    `save-models` = .flagValue(runSettings$saveModels)
  )
  if (!is.null(minFraction)) base$`baseline-preprocess-min-fraction` <- .flagValue(minFraction)
  base$`surrogate-capture-directory` <- runSettings$surrogateCaptureDirectory
  base$`preprocessing-reference-directory` <- runSettings$preprocessingReferenceDirectory

  manifestId <- if (!isTRUE(runSettings$auditOnly)) {
    .startRunManifest(outputDirectory, list(taskSettings = taskSettings, sites = sites,
      featureSettings = featureSettings, methodSettings = methodSettings, holdouts = holdouts,
      resume = resume, runSettings = runSettings))
  }

  # One runner call per distinct option set, so options never cross methods.
  optionKeys <- vapply(methodSettings, function(m) paste(deparse(m$options), collapse = ""), "")
  for (key in unique(optionKeys)) {
    group <- methodSettings[optionKeys == key]
    options <- group[[1]]$options
    flags <- stats::setNames(lapply(options, .flagValue), .optionFlag(names(options)))
    args <- c(base, list(methods = paste(vapply(group, `[[`, "", "method"), collapse = ",")), flags)
    args$featureSettings <- featureSettings
    runComparisonFromArgs(args, populationSettings = taskSettings$populationSettings, clientPaths = sites)
  }
  if (isTRUE(runSettings$auditOnly)) {
    return(invisible(utils::read.csv(file.path(outputDirectory, "preprocessing", "summary.csv"),
      stringsAsFactors = FALSE)))
  }
  .finishRunManifest(outputDirectory, manifestId)
  invisible(readComparisonResults(outputDirectory))
}

#' Read comparison results
#'
#' @param outputDirectory A [runComparison()] result directory.
#' @return One row per task, outer fold, feature setting and method, with
#'   held-out metrics, selected penalties and fit status.
#' @export
readComparisonResults <- function(outputDirectory) {
  path <- file.path(outputDirectory, "comparison_results.csv")
  if (!file.exists(path)) stop("No comparison results in ", outputDirectory, call. = FALSE)
  results <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("task", "fold", "featureSet", "method", "auc")
  missing <- setdiff(required, names(results))
  if (length(missing)) stop("Comparison results lack column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  results
}
