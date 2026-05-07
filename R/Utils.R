# Null coalescing helper used in algorithm configs
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Compute density = fraction of |w|>threshold
#' @param w numeric model coefficient vector
#' @param threshold absolute value threshold for nonzero coefficients
#' @return fraction of coefficients above the threshold
computeDensity <- function(w, threshold = 1e-4) {
  mean(abs(w) > threshold)
}

# Cross-entropy loss
logLoss <- function(y, pHat) {
  eps <- 1e-15
  pHat <- pmin(pmax(pHat, eps), 1 - eps)
  -mean(y * log(pHat) + (1 - y) * log(1 - pHat))
}

# Accuracy
accuracy <- function(y, pHat, cutoff = 0.5) mean((pHat > cutoff) == y)

# AUC
auc <- function(y, pHat) {
  if (length(unique(y)) == 2) {
    as.numeric(pROC::roc(y, pHat, quiet = TRUE)$auc)
  } else {
    warning("AUC is not defined for non-binary outcomes")
    NA_real_
  }
}

reportWeights <- function(clientReports, aggregation = "sampleSize") {
  aggregation <- match.arg(aggregation, c("sampleSize", "equalClient"))
  if (identical(aggregation, "equalClient")) {
    return(rep(1 / length(clientReports), length(clientReports)))
  }
  ns <- vapply(clientReports, function(x) x$n %||% NA_real_, numeric(1))
  if (any(!is.finite(ns)) || any(ns <= 0)) {
    stop("sampleSize aggregation requires each client report to include positive finite n")
  }
  ns / sum(ns)
}

weightedReportAverage <- function(clientReports, name, aggregation = "sampleSize") {
  weights <- reportWeights(clientReports, aggregation = aggregation)
  values <- lapply(clientReports, `[[`, name)
  Reduce(`+`, Map(function(value, weight) value * weight, values, weights))
}
