#' Define which covariates enter the model
#'
#' Selects covariates among those extracted for every site and sets the shared
#' training-only filter. A covariate is selected when its id is in
#' `covariateIds` or its analysis is in `analysisIds`. With both `NULL`, all
#' extracted covariates are candidates. Selected covariates are aligned across
#' training sites by intersection, and an intercept is always included.
#'
#' @param covariateIds Covariate ids to include, or `NULL`.
#' @param analysisIds FeatureExtraction analysis ids whose covariates to include,
#'   or `NULL`.
#' @param minFraction Covariates with nonzero values in fewer than this fraction
#'   of outer-training entries are removed, together with redundant and
#'   near-constant columns. The filter is estimated from site summaries before
#'   any penalty selection and applied unchanged to every method and the
#'   held-out site. `NULL` disables the shared filter.
#' @return A `featureSettings` object.
#' @export
createFeatureSettings <- function(covariateIds = NULL, analysisIds = NULL, minFraction = 0.001) {
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
  structure(list(
    covariateIds = ids(covariateIds, "covariateIds"),
    analysisIds = ids(analysisIds, "analysisIds"),
    minFraction = minFraction
  ), class = "featureSettings")
}

.selectFeatureSettings <- function(covariateRef, settings) {
  if (is.null(settings$covariateIds) && is.null(settings$analysisIds)) {
    return(rep(TRUE, nrow(covariateRef)))
  }
  covariateRef$covariateId %in% settings$covariateIds |
    covariateRef$analysisId %in% settings$analysisIds
}
