#' Define which covariates enter the model
#'
#' Selects covariates among those extracted for every site and sets the shared
#' training-only filter. A covariate is selected when its id is in
#' `covariateIds` or its analysis is in `analysisIds`. With both `NULL`, all
#' extracted covariates are candidates. Selected covariates are aligned across
#' training sites by intersection, and an intercept is always included.
#'
#' Combinations replace groups of binary covariates by one indicator that is 1
#' when any component is nonzero (logical OR, not a sum). They are applied at
#' every site before selection, intersection and filtering. FeatureExtraction
#' omits covariates no patient has, so a component absent from a site's data is
#' zero there. A component that the site's recorded extraction settings show
#' was never requested stops the run instead of being treated as zero.
#'
#' @param covariateIds Covariate ids to include, or `NULL`.
#' @param analysisIds FeatureExtraction analysis ids whose covariates to include,
#'   or `NULL`.
#' @param combinations List of combinations, each a list with `covariateId`
#'   (a new id, distinct from every extracted covariate), `name` and
#'   `components` (the covariate ids to combine). Selection applies to the
#'   combined covariate, which takes its components' analysis id.
#' @param minFraction Covariates with nonzero values in fewer than this fraction
#'   of outer-training entries are removed, together with redundant and
#'   near-constant columns. The filter is estimated from site summaries before
#'   any penalty selection and applied unchanged to every method and the
#'   held-out site. `NULL` disables the shared filter.
#' @return A `featureSettings` object.
#' @export
createFeatureSettings <- function(covariateIds = NULL, analysisIds = NULL, minFraction = 0.001,
                                  combinations = list()) {
  ids <- function(x, name) {
    if (is.null(x)) return(NULL)
    if (!is.numeric(x) || !length(x) || any(!is.finite(x)) || any(x < 0) || any(x != round(x))) {
      stop(name, " must be NULL or non-negative whole numbers", call. = FALSE)
    }
    sort(unique(as.numeric(x)))
  }
  if (!is.null(minFraction) && (!is.numeric(minFraction) || length(minFraction) != 1L ||
      !is.finite(minFraction) || minFraction < 0 || minFraction > 1)) {
    stop("minFraction must be NULL or a single number between 0 and 1", call. = FALSE)
  }
  if (!is.list(combinations)) stop("combinations must be a list", call. = FALSE)
  combinations <- lapply(combinations, function(combination) {
    if (!is.list(combination) || !setequal(names(combination), c("covariateId", "name", "components"))) {
      stop("Each combination must be a list with covariateId, name and components", call. = FALSE)
    }
    if (!is.character(combination$name) || length(combination$name) != 1L || !nzchar(combination$name)) {
      stop("Each combination needs a single non-empty name", call. = FALSE)
    }
    covariateId <- ids(combination$covariateId, "A combination covariateId")
    components <- ids(combination$components, "Combination components")
    if (length(covariateId) != 1L || length(components) < 2L || covariateId %in% components) {
      stop("Each combination needs one new covariateId and at least two other components", call. = FALSE)
    }
    list(covariateId = covariateId, name = combination$name, components = components)
  })
  used <- unlist(lapply(combinations, function(x) c(x$covariateId, x$components)))
  if (anyDuplicated(used)) stop("Combinations must not share covariate ids", call. = FALSE)
  structure(list(
    covariateIds = ids(covariateIds, "covariateIds"),
    analysisIds = ids(analysisIds, "analysisIds"),
    minFraction = minFraction,
    combinations = combinations
  ), class = "featureSettings")
}

.featureCombinations <- function(featureSet) {
  if (inherits(featureSet, "featureSettings")) featureSet$combinations %||% list() else list()
}

# Cohort covariate ids requested by a site's FeatureExtraction settings, with the
# analysis ids they use, or NULL when the settings record no cohort covariates.
.requestedCohortCovariates <- function(covariateSettings) {
  if (inherits(covariateSettings, "covariateSettings")) covariateSettings <- list(covariateSettings)
  cohorts <- Filter(function(s) is.list(s) && !is.null(s$covariateCohorts), covariateSettings %||% list())
  if (!length(cohorts)) return(NULL)
  list(
    covariateIds = unlist(lapply(cohorts, function(s) s$covariateCohorts$cohortId * 1000 + s$analysisId)),
    analysisIds = unique(vapply(cohorts, function(s) as.numeric(s$analysisId), numeric(1)))
  )
}

# Replaces each combination's component rows by one combined covariate row. A
# component absent from the data is zero, unless the site's recorded extraction
# settings show that its cohort covariate was never requested.
.combineCovariateRef <- function(covariateRef, combinations) {
  requested <- attr(covariateRef, "requestedCohortCovariates")
  for (combination in combinations) {
    missing <- setdiff(combination$components, covariateRef$covariateId)
    if (!is.null(requested)) {
      notExtracted <- missing[missing %% 1000 %in% requested$analysisIds &
        !missing %in% requested$covariateIds]
      if (length(notExtracted)) {
        stop("Covariate combination '", combination$name, "' has component(s) ",
          paste(notExtracted, collapse = ", "), " that this site's extraction did not request",
          call. = FALSE)
      }
    }
    if (combination$covariateId %in% covariateRef$covariateId) {
      stop("Combined covariate id ", combination$covariateId, " is already an extracted covariate",
        call. = FALSE)
    }
    parts <- covariateRef$covariateId %in% combination$components
    if (!any(parts)) next
    analysisId <- unique(covariateRef$analysisId[parts])
    combined <- covariateRef[which(parts)[1], , drop = FALSE]
    combined$covariateId <- combination$covariateId
    if ("analysisId" %in% names(combined)) combined$analysisId <- if (length(analysisId) == 1L) analysisId else NA
    if ("covariateName" %in% names(combined)) combined$covariateName <- combination$name
    kept <- rbind(covariateRef[!parts, , drop = FALSE], combined)
    attr(kept, "requestedCohortCovariates") <- requested
    covariateRef <- kept
  }
  covariateRef
}

# Builds combined indicator columns from their components' columns: the mapping
# has the combined ids, so components are mapped to temporary extra columns.
.combinedClientMatrix <- function(plpData, mapping, combinations) {
  combinedIds <- vapply(combinations, `[[`, numeric(1), "covariateId")
  components <- unlist(lapply(combinations, `[[`, "components"))
  extended <- rbind(
    mapping[!mapping$covariateId %in% combinedIds, c("covariateId", "columnId"), drop = FALSE],
    data.frame(covariateId = components, columnId = nrow(mapping) + seq_along(components))
  )
  x <- PatientLevelPrediction::toSparseM(plpData, cohort = plpData$population, map = extended)$dataMatrix
  for (combination in combinations) {
    column <- mapping$columnId[mapping$covariateId == combination$covariateId]
    if (!length(column)) next
    parts <- extended$columnId[extended$covariateId %in% combination$components]
    x[, column] <- as.numeric(Matrix::rowSums(x[, parts, drop = FALSE] != 0) > 0)
  }
  x[, seq_len(nrow(mapping)), drop = FALSE]
}

.selectFeatureSettings <- function(covariateRef, settings) {
  if (is.null(settings$covariateIds) && is.null(settings$analysisIds)) {
    return(rep(TRUE, nrow(covariateRef)))
  }
  covariateRef$covariateId %in% settings$covariateIds |
    covariateRef$analysisId %in% settings$analysisIds
}

#' Define a prediction task
#'
#' Follows PatientLevelPrediction's `createModelDesign()`: the task names its
#' target and outcome cohorts and carries the PLP population settings that both
#' data extraction and fitting use. The horizon therefore has exactly one
#' source. Set `riskWindowEnd` explicitly when creating `populationSettings`,
#' because PatientLevelPrediction defaults it to 365 days.
#'
#' @param name Task name, used in result files and model artifacts.
#' @param targetId Target cohort id.
#' @param outcomeId Outcome cohort id.
#' @param populationSettings A PLP `populationSettings` object from
#'   `PatientLevelPrediction::createStudyPopulationSettings()`.
#' @return A `taskSettings` object.
#' @export
createTaskSettings <- function(name, targetId, outcomeId, populationSettings) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("name must be a single non-empty string", call. = FALSE)
  }
  for (id in list(list(targetId, "targetId"), list(outcomeId, "outcomeId"))) {
    if (!is.numeric(id[[1]]) || length(id[[1]]) != 1L || !is.finite(id[[1]]) || id[[1]] != round(id[[1]])) {
      stop(id[[2]], " must be a single whole number", call. = FALSE)
    }
  }
  if (!inherits(populationSettings, "populationSettings")) {
    stop("populationSettings must come from PatientLevelPrediction::createStudyPopulationSettings()",
      call. = FALSE)
  }
  structure(list(name = name, targetId = targetId, outcomeId = outcomeId,
    populationSettings = populationSettings), class = "taskSettings")
}

#' Define one method to compare
#'
#' Federated methods and centralised or local lasso baselines are defined the
#' same way. Options are the comparison runner's settings in camelCase, for
#' example `createMethodSettings("DualAvg", dualavgLambda = 0.01)` or
#' `createMethodSettings("PooledLasso", cyclopsFolds = 4)`. A vector value
#' defines a grid of configurations. Options a method does not use are ignored
#' by that method.
#'
#' @param method Method name; an error lists the valid names.
#' @param ... Named method options; an error lists the valid names.
#' @return A `methodSettings` object.
#' @export
createMethodSettings <- function(method, ...) {
  valid <- .comparisonMethods()
  if (!is.character(method) || length(method) != 1L || !method %in% valid) {
    stop("Unknown method. Valid methods: ", paste(valid, collapse = ", "), call. = FALSE)
  }
  options <- list(...)
  if (length(options) && (is.null(names(options)) || any(!nzchar(names(options))))) {
    stop("Method options must be named", call. = FALSE)
  }
  unknown <- names(options)[!.optionFlag(names(options)) %in% .methodOptionFlags]
  if (length(unknown)) {
    stop("Unknown method option(s): ", paste(unknown, collapse = ", "), ". Valid options: ",
      paste(sort(.optionName(.methodOptionFlags)), collapse = ", "), call. = FALSE)
  }
  for (name in names(options)) {
    value <- options[[name]]
    if (!(is.numeric(value) || is.logical(value) || is.character(value)) || !length(value) || anyNA(value)) {
      stop("Method option ", name, " must be a non-missing number, logical or string", call. = FALSE)
    }
  }
  structure(list(method = method, options = options), class = "methodSettings")
}

#' Define how a comparison runs
#'
#' These options change how a run executes, not what it estimates.
#'
#' @param hosts Worker hosts, one per site or a single host for all sites.
#' @param mirai Use mirai workers instead of PSOCK workers.
#' @param debug Save debug diagnostics under the output directory.
#' @param surrogateCaptureDirectory Private directory for patient-level
#'   surrogate captures; it must be outside the output directory.
#' @param auditOnly Estimate and audit the shared feature filter without fitting.
#' @param preprocessingReferenceDirectory Earlier results whose saved filter
#'   must match this run's.
#' @param rerunErrors Refit combinations that previously failed.
#' @param rerunMissingModels Refit combinations whose model artifact is missing.
#' @param saveModels Save a model artifact for each fit.
#' @param verbose Print progress messages.
#' @return An `runSettings` object.
#' @export
createRunSettings <- function(hosts = "localhost", mirai = FALSE, debug = FALSE,
                                    surrogateCaptureDirectory = NULL, auditOnly = FALSE,
                                    preprocessingReferenceDirectory = NULL, rerunErrors = FALSE,
                                    rerunMissingModels = FALSE, saveModels = TRUE, verbose = TRUE) {
  flags <- list(mirai = mirai, debug = debug, auditOnly = auditOnly, rerunErrors = rerunErrors,
    rerunMissingModels = rerunMissingModels, saveModels = saveModels, verbose = verbose)
  for (name in names(flags)) {
    if (!is.logical(flags[[name]]) || length(flags[[name]]) != 1L || is.na(flags[[name]])) {
      stop(name, " must be TRUE or FALSE", call. = FALSE)
    }
  }
  if (!is.character(hosts) || !length(hosts) || anyNA(hosts)) stop("hosts must be host names", call. = FALSE)
  structure(c(list(hosts = hosts, surrogateCaptureDirectory = surrogateCaptureDirectory,
    preprocessingReferenceDirectory = preprocessingReferenceDirectory), flags), class = "runSettings")
}

.comparisonMethods <- function() {
  sort(unique(c(baselineMethods, names(.algRegistry))))
}

# Runner options that belong to methods. Run-level options come from the task,
# sites, feature settings and execution settings instead.
.methodOptionFlags <- c(
  "adap-beta-abs-threshold", "adap-condition-numbers", "adap-curvature-action", "adap-curvature-tau",
  "adap-cv-diagnostics",
  "adapdiag-style", "adap-eta-abs-threshold", "adap-final-max-outer", "adap-kkt-tolerance",
  "adap-lambda-cv-global-adjustment", "adap-lambda-cv-max-rows", "adap-lambda-selection-metric",
  "adap-lambda-selection-tie-tolerance", "adap-maxconv-tau", "adap-prox-tau", "adap-trace-diagnostics",
  "baseline-folds", "baseline-maxit", "baseline-seed", "baseline-starting-variance", "baseline-variance",
  "convergence-objective", "cv-seed", "cv-stratify", "cyclops-cv", "cyclops-cv-type", "cyclops-folds",
  "cyclops-lower-limit", "cyclops-max-iterations", "cyclops-noise-level", "cyclops-selector-type",
  "cyclops-starting-variance", "cyclops-threads", "cyclops-tolerance", "cyclops-upper-limit",
  "cyclops-variance", "cyclops-warnings", "diagnostic-controls-per-case", "diagnostic-downsample-seed",
  "dualavg-convergence-objective", "dualavg-kkt-check-every", "dualavg-kkt-tolerance", "dualavg-lambda",
  "dualavg-lambda-default", "dualavg-map-type", "dualavg-reference-fit", "dualavg-reference-tolerance",
  "dualavg-rounds", "dualavg-starting-variance",
  "dualavg-tune-lambda", "dualavg-warm-start-lambda-path", "dualavg-warm-start-round-offset", "epsilon",
  "eta-client", "eta-server", "inner-folds", "k", "lambda", "lambda-cv-global-adjustment",
  "lambda-cv-max-rows", "lambda-grid-len", "lambda-search", "lambda-search-max-evals", "lambda-search-tol",
  "lambda-selection-metric", "lambda-selection-tie-tolerance", "lead-index", "local-average-metric",
  "local-average-threshold", "local-average-thresholds", "local-cv-folds", "local-debias-max-features",
  "local-debias-multiplier", "local-ensemble-weighting", "max-inner", "max-iter", "max-outer",
  "odal-curvature-action", "odal-curvature-tau", "odal-init", "odal-ridge-fallback", "odal-ridge-lambda",
  "pda-rounds", "pooled-diagnostics", "pooled-kkt-tolerance", "profile", "rounds"
)

.optionFlag <- function(name) tolower(gsub("([A-Z])", "-\\1", name))
.optionName <- function(flag) gsub("-([a-z])", "\\U\\1", flag, perl = TRUE)

.flagValue <- function(value) {
  text <- if (is.logical(value)) tolower(as.character(value)) else as.character(value)
  paste(text, collapse = ",")
}
