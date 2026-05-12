# Null coalescing helper used in algorithm configs
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Cross-entropy loss
logLoss <- function(y, pHat) {
  eps <- 1e-15
  pHat <- pmin(pmax(pHat, eps), 1 - eps)
  -mean(y * log(pHat) + (1 - y) * log(1 - pHat))
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
