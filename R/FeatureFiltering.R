baselineFeatureSelection <- function(n, nnz, xMeans, x2Means, settings) {
  if (length(n) != 1L || !is.finite(n) || n <= 0 ||
      length(nnz) != length(xMeans) || length(nnz) != length(x2Means) ||
      any(!is.finite(c(nnz, xMeans, x2Means))) || any(nnz < 0 | nnz > n)) {
    stop("Invalid training summaries for preprocessing")
  }
  variance <- pmax(x2Means - xMeans^2, 0)
  minCount <- floor(settings$minFraction * n)
  rare <- if (is.finite(settings$minFraction) && settings$minFraction > 0)
    nnz < minCount else rep(FALSE, length(nnz))
  constant <- isTRUE(settings$removeRedundancy) & variance <= sqrt(.Machine$double.eps)
  list(keep = !(rare | constant), rare = rare, nearConstant = constant,
    variance = variance, minCount = minCount)
}

preprocessingFingerprint <- function(ids) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  saveRDS(as.character(ids), path, version = 2, compress = FALSE)
  unname(tools::md5sum(path))
}

trainingFeatureSummary <- function(clientData, intercept) {
  x <- clientData$xMatrix
  if (isTRUE(intercept)) x <- x[, -1L, drop = FALSE]
  if (nrow(x) != clientData$n || nrow(x) != length(clientData$yLabels)) {
    stop("Training row count differs from client metadata")
  }
  list(n = nrow(x), nnz = as.numeric(Matrix::colSums(x != 0)),
    sum = as.numeric(Matrix::colSums(x)), sumSquares = as.numeric(Matrix::colSums(x^2)))
}

preprocessorFromSummaries <- function(summaries, mapping, settings) {
  p <- nrow(mapping)
  if (!length(summaries) || anyDuplicated(mapping$covariateId) ||
      anyNA(mapping$covariateId) || !identical(as.integer(mapping$columnId), seq_len(p))) {
    stop("Invalid preprocessing feature map or empty training summaries")
  }
  for (s in summaries) {
    if (!all(lengths(s[c("nnz", "sum", "sumSquares")]) == p)) {
      stop("Training summary dimensions differ from the feature map")
    }
    baselineFeatureSelection(s$n, s$nnz, s$sum / s$n, s$sumSquares / s$n, settings)
  }
  n <- sum(vapply(summaries, `[[`, numeric(1), "n"))
  total <- function(name) Reduce(`+`, lapply(summaries, `[[`, name))
  nnz <- total("nnz")
  means <- total("sum") / n
  second <- total("sumSquares") / n
  selected <- baselineFeatureSelection(n, nnz, means, second, settings)
  audit <- data.frame(covariateId = as.character(mapping$covariateId),
    originalColumn = mapping$columnId, trainingRows = n, nonzeroRows = nnz,
    mean = means, secondMoment = second, variance = selected$variance,
    minCount = selected$minCount, removedRare = selected$rare,
    removedNearConstant = selected$nearConstant, retained = selected$keep)
  retained <- mapping[selected$keep, , drop = FALSE]
  retained$columnId <- seq_len(nrow(retained))
  rownames(retained) <- NULL
  list(scope = "outer-training", settings = settings, originalMapping = mapping,
    mapping = retained, audit = audit,
    trainingSampleSizes = vapply(summaries, `[[`, numeric(1), "n"),
    fingerprint = preprocessingFingerprint(retained$covariateId))
}

collectTrainingFeatureFilter <- function(cl, config, settings) {
  start <- Sys.time()
  config$mapping <- FederatedLearning::clusterCollectCovRefs(cl,
    type = config$mapType, featureSet = config$featureSet,
    covariateIds = config$covariateIds, analysisIds = config$analysisIds)
  config$p <- nrow(config$mapping)
  if (!config$p) stop("Global feature map is empty for preprocessing")
  FederatedLearning::clusterCreateMatrices(cl, config)
  summaries <- parallel::clusterCall(cl, function(summarize, intercept) {
    summarize(get("clientData", envir = .GlobalEnv), intercept)
  }, summarize = trainingFeatureSummary, intercept = config$intercept)
  out <- preprocessorFromSummaries(summaries, config$mapping, settings)
  # Counts cover the new summary replies, not cluster setup or feature-map transport.
  out$communication <- list(messages = length(summaries),
    numbers = length(summaries) * (1 + 3 * config$p))
  out$elapsedSeconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  out
}

preprocessingMatchesModel <- function(preprocessor, model, trainClientIds) {
  original <- model$originalMapping[order(model$originalMapping$columnId), , drop = FALSE]
  referenceSizes <- model$trainingSampleSizes[match(trainClientIds, trimws(names(model$trainingSampleSizes)))]
  identical(as.character(original$covariateId), preprocessor$audit$covariateId) &&
    identical(as.logical(model$preprocessing$keep), preprocessor$audit$retained) &&
    identical(as.numeric(referenceSizes), as.numeric(preprocessor$trainingSampleSizes)) &&
    isTRUE(sum(model$trainingSampleSizes) == preprocessor$audit$trainingRows[[1]])
}
