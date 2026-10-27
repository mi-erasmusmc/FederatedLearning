# Comparison runner: fits and evaluates federated and centralised methods across
# tasks, outer folds and feature sets, with resume and model artifacts. The
# command-line entry point is extras/runComparisonMatrix.R.
parseArgs <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list()
  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }
    kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- kv[[1]]
    value <- if (length(kv) > 1L) paste(kv[-1], collapse = "=") else "true"
    out[[key]] <- value
  }
  out
}

argValue <- function(args, name) {
  args[[name, exact = TRUE]]
}

csvArg <- function(x, default = character()) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

intArg <- function(x, default) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  as.integer(x)
}

numArg <- function(x, default) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  as.numeric(x)
}

numCsvArg <- function(x, default) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  as.numeric(csvArg(x))
}

intCsvArg <- function(x, default) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  as.integer(csvArg(x))
}

charCsvArg <- function(x, default) {
  vals <- csvArg(x, default)
  vals[nzchar(vals)]
}

firstValue <- function(x) {
  if (length(x) == 0L) {
    return(NULL)
  }
  x[[1]]
}

logicalArg <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  tolower(x) %in% c("true", "t", "1", "yes", "y")
}

maxOrNa <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

foldArg <- function(x, nClients) {
  if (is.null(x) || !nzchar(x)) {
    return(seq_len(nClients))
  }
  if (grepl(":", x, fixed = TRUE)) {
    parts <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
    return(seq.int(parts[[1]], parts[[2]]))
  }
  as.integer(csvArg(x))
}

taskRiskWindow <- function(task) {
  switch(task,
    dementia = 5 * 365,
    dementiaPhenotypes = 5 * 365,
    readmission = 30,
    lungCancer = 5 * 365,
    lungCancerPhenotypes = 5 * 365,
    30
  )
}

methodRounds <- function(method, args) {
  switch(method,
    DualAvg = firstValue(intCsvArg(argValue(args, "dualavg-rounds"), 10000L)),
    DualAvgCpp = firstValue(intCsvArg(argValue(args, "dualavg-rounds"), 10000L)),
    DualAvgR = firstValue(intCsvArg(argValue(args, "dualavg-rounds"), 10000L)),
    ODAL = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ODAL1 = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ODAL2 = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ADAP = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ADAP2 = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ADAP_PDA = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ADAP1 = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    ADAPDiag = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    `Prox-ADAP` = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    `C-ADAP` = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    `MaxConv-ADAP` = firstValue(intCsvArg(argValue(args, "pda-rounds"), 3L)),
    firstValue(intCsvArg(argValue(args, "rounds"), 1000L))
  )
}

baselineMethods <- c(
  "PooledLasso",
  "LocalAvgLasso",
  "SparseLocalAvgLasso",
  "DebiasedLocalAvgLasso",
  "BiggestSiteLasso",
  "LocalEnsembleLasso",
  "LocalBestLasso",
  "LocalSiteLasso"
)
dualAvgMethods <- c("DualAvg", "DualAvgCpp", "DualAvgR")
sparseAverageMethods <- c("SparseLocalAvgLasso", "DebiasedLocalAvgLasso")

readCsvIfExists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

nonEmptyRows <- function(rows) {
  !is.null(rows) && nrow(rows) > 0L
}

isCompletedCombination <- function(rows, task, fold, featureSet, method,
                                   rerunErrors = FALSE, rerunMissingModels = FALSE,
                                   resultDirectory = NULL) {
  idx <- matchingCombination(rows, task, fold, featureSet, method)
  if (!any(idx)) {
    return(FALSE)
  }
  successful <- rows[idx & successfulRows(rows), , drop = FALSE]
  if (isTRUE(rerunErrors) && nrow(successful) == 0L) {
    return(FALSE)
  }
  if (isTRUE(rerunMissingModels) && nrow(successful) > 0L) {
    return(!is.null(readModelArtifact(successful, resultDirectory, task, fold, featureSet, method)))
  }
  TRUE
}

guardAdapInitializerResume <- function(rows, task, fold, featureSet, method, args, resultDirectory) {
  adapMethods <- c("ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")
  if (!method %in% adapMethods) return(invisible(NULL))
  requested <- methodConfig(method, featureSet, args)
  requestedBackend <- requested$adapInitBackend %||% "glmnet"
  foldSettings <- function(config) list(
    folds = config$adapInitFolds %||% 10L,
    stratify = config$adapInitStratify %||% FALSE,
    seed = config$adapInitSeed %||% config$cvSeed %||% 42L
  )
  selected <- rows[matchingCombination(rows, task, fold, featureSet, method) & successfulRows(rows), , drop = FALSE]
  artifact <- readModelArtifact(selected, resultDirectory, task, fold, featureSet, method)
  if (is.null(artifact)) {
    if (!identical(requestedBackend, "glmnet") ||
        !identical(foldSettings(requested), list(folds = 10L, stratify = FALSE, seed = 42L))) {
      stop("Cannot verify the ADAP initializer for an existing result; use a new result directory")
    }
    return(invisible(NULL))
  }
  saved <- artifact$config %||% list()
  savedBackend <- saved$adapInitBackend %||% "glmnet"
  if (!identical(savedBackend, requestedBackend)) {
    stop("The existing ADAP result uses a different initializer; use a new result directory")
  }
  if (!identical(foldSettings(saved), foldSettings(requested))) {
    stop("The existing ADAP result uses different initializer folds or seed; use a new result directory")
  }
  if (identical(requestedBackend, "cyclops")) {
    settings <- function(config) list(
      seed = config$adapInitSeed %||% config$cvSeed %||% 42L,
      maxEvals = config$adapInitMaxEvals %||% 25L,
      tolerance = config$adapInitTolerance %||% 1e-12,
      kktTolerance = config$adapInitKktTolerance %||% 1e-6,
      maxIterations = config$adapInitMaxIterations %||% 3000L
    )
    if (!isTRUE(all.equal(settings(saved), settings(requested)))) {
      stop("The existing ADAP result uses different initializer settings; use a new result directory")
    }
  }
  invisible(NULL)
}

binaryAuc <- function(y, preds) {
  if (length(unique(y)) != 2L) {
    return(NA_real_)
  }
  if (length(preds) != length(y) || any(!is.finite(preds))) {
    return(NA_real_)
  }
  y01 <- as.integer(y == max(y))
  nCase <- as.numeric(sum(y01 == 1L))
  nControl <- as.numeric(sum(y01 == 0L))
  if (nCase == 0L || nControl == 0L) {
    return(NA_real_)
  }
  ranks <- rank(preds, ties.method = "average")
  (sum(ranks[y01 == 1L]) - nCase * (nCase + 1) / 2) / (nCase * nControl)
}

matrixMoments <- function(xMatrix, intercept = FALSE) {
  xMeans <- Matrix::colMeans(xMatrix)
  x2Means <- Matrix::colMeans(xMatrix^2)
  if (isTRUE(intercept) && length(xMeans) > 0L) {
    xMeans[[1]] <- 0
    x2Means[[1]] <- 0
  }
  list(xMeans = xMeans, x2Means = x2Means)
}

dropCombinationRows <- function(rows, task, fold, featureSet, method) {
  if (!nonEmptyRows(rows)) {
    return(rows)
  }
  idx <- matchingCombination(rows, task, fold, featureSet, method)
  rows[!idx, , drop = FALSE]
}

appendCombinationRows <- function(rows, newRows, task, fold, featureSet, method) {
  rows <- dropCombinationRows(rows, task, fold, featureSet, method)
  if (!nonEmptyRows(rows)) {
    return(newRows)
  }
  dplyr::bind_rows(rows, newRows)
}

stampMethodElapsed <- function(rows, startTime) {
  elapsed <- as.numeric(difftime(Sys.time(), startTime, units = "secs"))
  if (!"fitElapsedSeconds" %in% names(rows)) {
    rows$fitElapsedSeconds <- rows$elapsedSeconds %||% NA_real_
  }
  rows$elapsedSeconds <- elapsed
  rows
}

# Peak memory so far, in MB, of the main R process (whole session) and of the
# largest site worker (workers are started afresh for each fold).
stampPeakMemory <- function(rows, clusters) {
  workers <- unlist(lapply(Filter(Negate(is.null), clusters), function(cl) tryCatch(
    unlist(parallel::clusterCall(cl, peakMemoryMbCpp)), error = function(e) NA_real_)))
  rows$peakMemoryMbMain <- peakMemoryMbCpp()
  rows$peakMemoryMbWorkers <- if (any(is.finite(workers))) max(workers, na.rm = TRUE) else NA_real_
  rows
}

# "site+server" seconds per round, for fits with a handful of rounds such as
# ODAL and ADAP; NA for iterative fits.
roundSecondsLabel <- function(siteSeconds, serverSeconds, maxRounds = 10L) {
  if (!length(siteSeconds) || length(siteSeconds) > maxRounds) return(NA_character_)
  paste(sprintf("%.1f+%.1f", siteSeconds, serverSeconds), collapse = "; ")
}

formatDuration <- function(seconds) {
  if (!is.finite(seconds)) return("NA")
  if (seconds < 60) return(sprintf("%.0f s", seconds))
  if (seconds < 3600) return(sprintf("%.1f min", seconds / 60))
  sprintf("%.2f h", seconds / 3600)
}

debugPath <- function(debugDirectory, task, fold, featureSet, method = NULL,
                      suffix = "debug", extension = "rds") {
  parts <- c(
    safeFilePart(task),
    paste0("fold", safeFilePart(fold)),
    safeFilePart(featureSet),
    if (!is.null(method)) safeFilePart(method),
    safeFilePart(suffix)
  )
  file.path(debugDirectory, paste0(paste(parts, collapse = "_"), ".", extension))
}

writeDebugObject <- function(x, debugDirectory, task, fold, featureSet,
                             method = NULL, suffix = "debug") {
  if (is.null(debugDirectory)) {
    return(invisible(NULL))
  }
  dir.create(debugDirectory, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    x,
    debugPath(debugDirectory, task, fold, featureSet, method, suffix, "rds")
  )
  invisible(NULL)
}

writeDebugCsv <- function(x, debugDirectory, task, fold, featureSet,
                          method = NULL, suffix = "debug") {
  if (is.null(debugDirectory)) {
    return(invisible(NULL))
  }
  dir.create(debugDirectory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    x,
    debugPath(debugDirectory, task, fold, featureSet, method, suffix, "csv"),
    row.names = FALSE
  )
  invisible(NULL)
}

isCompletedDiagnostic <- function(rows, task, fold, featureSet) {
  if (!nonEmptyRows(rows)) {
    return(FALSE)
  }
  required <- c("task", "fold", "featureSet")
  if (!all(required %in% names(rows))) {
    return(FALSE)
  }
  any(rows$task == task & rows$fold == fold & rows$featureSet == featureSet)
}

validateSurrogateCaptureArgs <- function(args, resultDirectory, methods, hosts) {
  root <- argValue(args, "surrogate-capture-directory")
  if (is.null(root)) return(args)
  if (any(!methods %in% .surrogateCaptureMethods)) {
    stop("Private capture requires an ODAL1/2 or exact ADAP1/2/Diag-only diagnostic run")
  }
  if (any(!hosts %in% c("localhost", "127.0.0.1", "::1"))) {
    stop("Private surrogate capture requires local workers")
  }
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    stop("--surrogate-capture-directory requires a directory path")
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  results <- normalizePath(resultDirectory, winslash = "/", mustWork = TRUE)
  compareRoot <- if (.Platform$OS.type == "windows") tolower(root) else root
  compareResults <- if (.Platform$OS.type == "windows") tolower(results) else results
  if (compareRoot == compareResults || startsWith(compareRoot, paste0(compareResults, "/"))) {
    stop("Private surrogate capture must be outside the shareable result directory")
  }
  args[["surrogate-capture-directory"]] <- root
  args
}

# The covariate selection behind a feature label: a featureSettings object
# supplied through the API, otherwise the named feature set itself.
featureSelection <- function(featureSet, args) {
  args[["featureSettings"]][[featureSet]] %||% featureSet
}

methodConfig <- function(method, featureSet, args) {
  adapMethods <- c("ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")
  defaultLambdaSearch <- if (method %in% adapMethods) "optimize" else "grid"
  defaultLambdaMetric <- "deviance"
  cfg <- list(
    mapType = firstValue(charCsvArg(argValue(args, "map-type"), "intersection")),
    featureSet = featureSelection(featureSet, args),
    intercept = logicalArg(argValue(args, "intercept"), TRUE),
    profile = logicalArg(argValue(args, "profile"), FALSE),
    epsilon = firstValue(numCsvArg(argValue(args, "epsilon"), 1e-6)),
    clientFrac = 1,
    rounds = methodRounds(method, args),
    # ADAP lead-site CV: three outcome-stratified folds, as for local lasso.
    foldsK = firstValue(intCsvArg(argValue(args, "inner-folds"), 3L)),
    cvStratify = logicalArg(argValue(args, "cv-stratify"), TRUE),
    cvSeed = intArg(argValue(args, "cv-seed"), 42L),
    maxIter = firstValue(intCsvArg(argValue(args, "max-iter"), 1000L)),
    maxOuter = firstValue(intCsvArg(argValue(args, "max-outer"), 500L)),
    maxInner = firstValue(intCsvArg(argValue(args, "max-inner"), 100L)),
    lambdaGridLen = firstValue(intCsvArg(argValue(args, "lambda-grid-len"), 100L)),
    lambdaSearch = firstValue(charCsvArg(argValue(args, "lambda-search"), defaultLambdaSearch)),
    lambdaSearchTol = firstValue(numCsvArg(argValue(args, "lambda-search-tol"), log(1.5))),
    lambdaSearchMaxEvals = firstValue(intCsvArg(argValue(args, "lambda-search-max-evals"), 25L)),
    lambdaSelectionMetric = firstValue(charCsvArg(
      argValue(args, "adap-lambda-selection-metric") %||% argValue(args, "lambda-selection-metric"),
      defaultLambdaMetric
    )),
    lambdaSelectionTieTolerance = firstValue(numCsvArg(
      argValue(args, "adap-lambda-selection-tie-tolerance") %||% argValue(args, "lambda-selection-tie-tolerance"),
      1e-8
    )),
    lambdaCvMaxRows = firstValue(numCsvArg(
      argValue(args, "adap-lambda-cv-max-rows") %||% argValue(args, "lambda-cv-max-rows"),
      Inf
    )),
    lambdaCvGlobalAdjustment = firstValue(charCsvArg(
      argValue(args, "adap-lambda-cv-global-adjustment") %||% argValue(args, "lambda-cv-global-adjustment"),
      "leaveValOut"
    )),
    diagnosticControlsPerCase = firstValue(numCsvArg(
      argValue(args, "diagnostic-controls-per-case"),
      Inf
    )),
    diagnosticDownsampleSeed = intArg(argValue(args, "diagnostic-downsample-seed"), 42L),
    adapCvDiagnostics = logicalArg(argValue(args, "adap-cv-diagnostics"), TRUE),
    # Condition-number estimates of dense Hessians are slow at thousands of columns.
    adapConditionNumbers = logicalArg(argValue(args, "adap-condition-numbers"), FALSE),
    adapTraceDiagnostics = logicalArg(argValue(args, "adap-trace-diagnostics"), FALSE),
    surrogateCaptureDirectory = argValue(args, "surrogate-capture-directory"),
    adapProxTau = firstValue(numCsvArg(argValue(args, "adap-prox-tau"), 1e-8)),
    adapCurvatureTau = firstValue(numCsvArg(argValue(args, "adap-curvature-tau"), 1e-10)),
    adapMaxConvTau = firstValue(numCsvArg(argValue(args, "adap-maxconv-tau"), 1e-10)),
    adapFinalMaxOuter = firstValue(intCsvArg(argValue(args, "adap-final-max-outer"), 1000L)),
    adapKktTolerance = firstValue(numCsvArg(argValue(args, "adap-kkt-tolerance"), 1e-4)),
    adapBetaAbsThreshold = firstValue(numCsvArg(argValue(args, "adap-beta-abs-threshold"), 1e4)),
    adapEtaAbsThreshold = firstValue(numCsvArg(argValue(args, "adap-eta-abs-threshold"), 1e4)),
    pooledDiagnostics = logicalArg(argValue(args, "pooled-diagnostics"), TRUE),
    pooledKktTolerance = firstValue(numCsvArg(argValue(args, "pooled-kkt-tolerance"), 1e-4)),
    convergenceObjective = firstValue(charCsvArg(argValue(args, "convergence-objective"), "negLogLikelihood"))
  )

  if (method %in% dualAvgMethods) {
    kktTolerance <- argValue(args, "dualavg-kkt-tolerance")
    if (!is.null(kktTolerance)) {
      cfg$dualAvgKktTolerance <- numArg(kktTolerance, NA_real_)
      cfg$dualAvgKktCheckEvery <- numArg(argValue(args, "dualavg-kkt-check-every"), 100L)
    }
    # Exact pooled optimum at the final penalty, for the distance of the fit from it.
    cfg$dualavgReferenceFit <- logicalArg(argValue(args, "dualavg-reference-fit"), TRUE)
    cfg$etaClient <- firstValue(numCsvArg(argValue(args, "eta-client"), 1))
    cfg$etaServer <- firstValue(numCsvArg(argValue(args, "eta-server"), 1))
    cfg$k <- firstValue(intCsvArg(argValue(args, "k"), 10L))
    cfg$lambda <- firstValue(numCsvArg(argValue(args, "dualavg-lambda"), 2.09e-4))
    cfg$mapType <- firstValue(charCsvArg(argValue(args, "dualavg-map-type"), cfg$mapType))
    cfg$convergenceObjective <- firstValue(charCsvArg(
      argValue(args, "dualavg-convergence-objective"),
      cfg$convergenceObjective
    ))
  } else {
    lambdaArg <- argValue(args, "lambda")
    cfg$lambda <- if (!is.null(lambdaArg)) firstValue(numCsvArg(lambdaArg, NA_real_)) else NULL
  }

  if (method %in% c("ODAL", "ODAL1", "ODAL2")) {
    cfg$odalVariant <- if (identical(method, "ODAL1")) "first" else "second"
    cfg$odalCurvatureAction <- firstValue(charCsvArg(argValue(args, "odal-curvature-action"), "report"))
    cfg$odalCurvatureTau <- firstValue(numCsvArg(argValue(args, "odal-curvature-tau"), 1e-10))
    cfg$odalInit <- firstValue(charCsvArg(
      argValue(args, "odal-init"),
      if (logicalArg(argValue(args, "odal-ridge-fallback"), FALSE)) "ridgeFallback" else "pda"
    ))
    cfg$odalRidgeLambda <- firstValue(numCsvArg(argValue(args, "odal-ridge-lambda"), 1e-8))
  }
  if (method %in% c("ADAP", "ADAP2") && !is.null(argValue(args, "adap-curvature-action"))) {
    cfg$adapCurvatureAction <- match.arg(argValue(args, "adap-curvature-action"), c("fail", "report"))
  }
  if (method %in% adapMethods && !is.null(argValue(args, "adap-init-backend"))) {
    cfg$adapInitBackend <- match.arg(argValue(args, "adap-init-backend"), c("glmnet", "glmnet_sparse", "cyclops"))
  }
  if (method %in% adapMethods) {
    if (!is.null(argValue(args, "adap-init-folds")))
      cfg$adapInitFolds <- intArg(argValue(args, "adap-init-folds"), NA_integer_)
    if (!is.null(argValue(args, "adap-init-stratify")))
      cfg$adapInitStratify <- logicalArg(argValue(args, "adap-init-stratify"), FALSE)
    if (!is.null(argValue(args, "adap-init-max-evals")))
      cfg$adapInitMaxEvals <- numArg(argValue(args, "adap-init-max-evals"), NA_real_)
    if (!is.null(argValue(args, "adap-init-max-iterations")))
      cfg$adapInitMaxIterations <- numArg(argValue(args, "adap-init-max-iterations"), NA_real_)
    if (!is.null(argValue(args, "adap-init-seed")))
      cfg$adapInitSeed <- intArg(argValue(args, "adap-init-seed"), NA_integer_)
    if (!is.null(argValue(args, "adap-init-tolerance")))
      cfg$adapInitTolerance <- numArg(argValue(args, "adap-init-tolerance"), NA_real_)
    if (!is.null(argValue(args, "adap-init-kkt-tolerance")))
      cfg$adapInitKktTolerance <- numArg(argValue(args, "adap-init-kkt-tolerance"), NA_real_)
  }

  if (identical(method, "ADAPDiag") && !is.null(argValue(args, "adapdiag-style"))) {
    cfg$adapDiagStyle <- firstValue(charCsvArg(argValue(args, "adapdiag-style"), "remote"))
  }
  if (method %in% c("ODAL", "ODAL1", "ODAL2", adapMethods) && !is.null(argValue(args, "lead-index"))) {
    cfg$leadIndex <- firstValue(intCsvArg(argValue(args, "lead-index"), NA_integer_))
  }
  cfg
}

methodConfigGridValues <- function(method, base, args) {
  values <- list(
    mapType = charCsvArg(argValue(args, "map-type"), base$mapType),
    epsilon = numCsvArg(argValue(args, "epsilon"), base$epsilon),
    rounds = if (method %in% dualAvgMethods) {
      intCsvArg(argValue(args, "dualavg-rounds"), base$rounds)
    } else if (method %in% c("ODAL", "ODAL1", "ODAL2", "ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")) {
      intCsvArg(argValue(args, "pda-rounds"), base$rounds)
    } else {
      intCsvArg(argValue(args, "rounds"), base$rounds)
    },
    foldsK = intCsvArg(argValue(args, "inner-folds"), base$foldsK),
    maxIter = intCsvArg(argValue(args, "max-iter"), base$maxIter),
    maxOuter = intCsvArg(argValue(args, "max-outer"), base$maxOuter),
    maxInner = intCsvArg(argValue(args, "max-inner"), base$maxInner),
    lambdaGridLen = intCsvArg(argValue(args, "lambda-grid-len"), base$lambdaGridLen),
    lambdaSearch = charCsvArg(argValue(args, "lambda-search"), base$lambdaSearch),
    lambdaSearchTol = numCsvArg(argValue(args, "lambda-search-tol"), base$lambdaSearchTol),
    lambdaSearchMaxEvals = intCsvArg(argValue(args, "lambda-search-max-evals"), base$lambdaSearchMaxEvals),
    lambdaSelectionMetric = charCsvArg(
      argValue(args, "adap-lambda-selection-metric") %||% argValue(args, "lambda-selection-metric"),
      base$lambdaSelectionMetric
    ),
    lambdaSelectionTieTolerance = numCsvArg(
      argValue(args, "adap-lambda-selection-tie-tolerance") %||% argValue(args, "lambda-selection-tie-tolerance"),
      base$lambdaSelectionTieTolerance
    ),
    lambdaCvMaxRows = numCsvArg(
      argValue(args, "adap-lambda-cv-max-rows") %||% argValue(args, "lambda-cv-max-rows"),
      base$lambdaCvMaxRows
    ),
    lambdaCvGlobalAdjustment = charCsvArg(
      argValue(args, "adap-lambda-cv-global-adjustment") %||% argValue(args, "lambda-cv-global-adjustment"),
      base$lambdaCvGlobalAdjustment
    ),
    diagnosticControlsPerCase = numCsvArg(
      argValue(args, "diagnostic-controls-per-case"),
      base$diagnosticControlsPerCase
    ),
    adapProxTau = numCsvArg(
      argValue(args, "adap-prox-tau"),
      base$adapProxTau
    ),
    adapCurvatureTau = numCsvArg(
      argValue(args, "adap-curvature-tau"),
      base$adapCurvatureTau
    ),
    adapMaxConvTau = numCsvArg(
      argValue(args, "adap-maxconv-tau"),
      base$adapMaxConvTau
    ),
    adapFinalMaxOuter = intCsvArg(
      argValue(args, "adap-final-max-outer"),
      base$adapFinalMaxOuter
    ),
    adapKktTolerance = numCsvArg(
      argValue(args, "adap-kkt-tolerance"),
      base$adapKktTolerance
    ),
    adapBetaAbsThreshold = numCsvArg(
      argValue(args, "adap-beta-abs-threshold"),
      base$adapBetaAbsThreshold
    ),
    adapEtaAbsThreshold = numCsvArg(
      argValue(args, "adap-eta-abs-threshold"),
      base$adapEtaAbsThreshold
    ),
    pooledKktTolerance = numCsvArg(
      argValue(args, "pooled-kkt-tolerance"),
      base$pooledKktTolerance
    )
  )
  if (method %in% c("ODAL", "ODAL1", "ODAL2")) {
    values$odalInit <- charCsvArg(argValue(args, "odal-init"), base$odalInit)
    values$odalRidgeLambda <- numCsvArg(argValue(args, "odal-ridge-lambda"), base$odalRidgeLambda)
    values$odalCurvatureAction <- charCsvArg(argValue(args, "odal-curvature-action"), base$odalCurvatureAction)
    values$odalCurvatureTau <- numCsvArg(argValue(args, "odal-curvature-tau"), base$odalCurvatureTau)
  }
  if (method %in% dualAvgMethods) {
    values$etaClient <- numCsvArg(argValue(args, "eta-client"), base$etaClient)
    values$etaServer <- numCsvArg(argValue(args, "eta-server"), base$etaServer)
    values$k <- intCsvArg(argValue(args, "k"), base$k)
    values$lambda <- numCsvArg(argValue(args, "dualavg-lambda"), base$lambda)
    values$mapType <- charCsvArg(argValue(args, "dualavg-map-type"), base$mapType)
    values$convergenceObjective <- charCsvArg(
      argValue(args, "dualavg-convergence-objective"),
      base$convergenceObjective
    )
  } else if (!is.null(base$lambda) || !is.null(argValue(args, "lambda"))) {
    values$lambda <- numCsvArg(argValue(args, "lambda"), base$lambda %||% NA_real_)
  }
  if (identical(method, "ADAPDiag")) {
    values$adapDiagStyle <- charCsvArg(argValue(args, "adapdiag-style"), base$adapDiagStyle %||% "remote")
  }
  if (method %in% c("ODAL", "ODAL1", "ODAL2", "ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP") &&
      !is.null(argValue(args, "lead-index"))) {
    values$leadIndex <- intCsvArg(argValue(args, "lead-index"), base$leadIndex %||% NA_integer_)
  }
  values
}

configLabel <- function(grid, i) {
  parts <- vapply(names(grid), function(nm) {
    sprintf("%s=%s", nm, grid[[nm]][[i]])
  }, character(1))
  paste(parts, collapse = ";")
}

methodConfigGrid <- function(method, featureSet, args) {
  base <- methodConfig(method, featureSet, args)
  grid <- expand.grid(
    methodConfigGridValues(method, base, args),
    stringsAsFactors = FALSE
  )
  configs <- lapply(seq_len(nrow(grid)), function(i) {
    cfg <- base
    for (nm in names(grid)) {
      cfg[[nm]] <- grid[[nm]][[i]]
    }
    cfg$configLabel <- if (nrow(grid) == 1L) "default" else configLabel(grid, i)
    cfg
  })
  configs
}

shouldTuneDualAvg <- function(args) {
  tuneArg <- argValue(args, "dualavg-tune-lambda")
  if (!is.null(tuneArg)) {
    return(logicalArg(tuneArg, TRUE))
  }
  is.null(argValue(args, "dualavg-lambda"))
}

dualAvgStartingVariance <- function(args) {
  numArg(
    argValue(args, "dualavg-starting-variance"),
    numArg(argValue(args, "dualavg-lambda-default"), 0.01)
  )
}

diagnosticMatrixSizes <- function(cl, config) {
  if (!is.finite(config$diagnosticControlsPerCase %||% Inf)) {
    return(NULL)
  }
  parallel::clusterCall(
    cl,
    function(mapping, config) {
      .assertWorkerState(
        "plpData",
        action = "Run clusterLoadData() before computing diagnostic matrix sizes."
      )
      config <- within(config, {
        mapping <- mapping
        p <- nrow(mapping)
      })
      clientData <- FederatedLearning::createClientMatrix(
        plpData,
        config = config
      )
      length(clientData$yLabels)
    },
    mapping = config$mapping,
    config = config
  )
}

collectDebugDiagnostics <- function(cl, config, debugDirectory, task, fold,
                                    featureSet, role, clientIds, clientIndexes,
                                    verbose = TRUE) {
  if (is.null(debugDirectory)) {
    return(NULL)
  }
  out <- tryCatch(
    {
      FederatedLearning::clusterCreateMatrices(cl, config)
      diagRows <- FederatedLearning::clusterDiagnostics(cl, config)
      diagRows$task <- task
      diagRows$fold <- fold
      diagRows$featureSet <- featureSet
      diagRows$role <- role
      diagRows$clientIndex <- clientIndexes[diagRows$client]
      diagRows$clientId <- clientIds[diagRows$client]
      writeDebugCsv(
        diagRows,
        debugDirectory = debugDirectory,
        task = task,
        fold = fold,
        featureSet = featureSet,
        method = role,
        suffix = "matrix-diagnostics"
      )
      diagRows
    },
    error = function(e) {
      writeDebugObject(
        list(
          role = role,
          error = conditionMessage(e),
          class = class(e),
          call = if (!is.null(conditionCall(e))) deparse(conditionCall(e)) else NULL
        ),
        debugDirectory = debugDirectory,
        task = task,
        fold = fold,
        featureSet = featureSet,
        method = role,
        suffix = "matrix-diagnostics-error"
      )
      if (isTRUE(verbose)) {
        message("Unable to write debug diagnostics for ", role, ": ", conditionMessage(e))
      }
      NULL
    }
  )
  out
}

tuneDualAvgForFold <- function(method, clTrain, config, trainPopSizes, args, verbose) {
  if (!method %in% dualAvgMethods || !shouldTuneDualAvg(args)) {
    return(config)
  }
  trainPopSizes <- as.numeric(unlist(trainPopSizes, use.names = FALSE))
  if (length(trainPopSizes) < 2L) {
    stop("DualAvg lambda tuning requires at least two training clients")
  }

  algo <- .getAlgorithm(method)
  if (is.null(algo)) {
    stop("Algorithm '", method, "' is not registered")
  }
  lambdaStrategy <- algo$lambdaStrategy %||% .lambdaStrategyDefault()
  globalMap <- config$mapping %||% FederatedLearning::clusterCollectCovRefs(
    clTrain,
    type = config$mapType,
    featureSet = config$featureSet,
    covariateIds = config$covariateIds,
    analysisIds = config$analysisIds
  )
  totalPopSize <- sum(trainPopSizes)
  diagnosticSizes <- diagnosticMatrixSizes(clTrain, c(
    config,
    list(mapping = globalMap, p = nrow(globalMap))
  ))
  if (!is.null(diagnosticSizes)) {
    trainPopSizes <- as.numeric(unlist(diagnosticSizes, use.names = FALSE))
    totalPopSize <- sum(trainPopSizes)
    if (isTRUE(verbose)) {
      message(
        "Using diagnostic downsampled matrix sizes for DualAvg lambda scaling: ",
        paste(trainPopSizes, collapse = ", ")
      )
    }
  }
  lambdaDefault <- dualAvgStartingVariance(args)

  configBase <- config
  configBase$lambda <- NULL
  configBase$mapping <- globalMap
  configBase$p <- nrow(globalMap)
  configBase$warmStartLambdaPath <- logicalArg(
    argValue(args, "dualavg-warm-start-lambda-path"),
    TRUE
  )
  configBase$warmStartRoundOffset <- logicalArg(
    argValue(args, "dualavg-warm-start-round-offset"),
    FALSE
  )

  if (isTRUE(verbose)) {
    message(sprintf(
      "Tuning %s lambda by federated inner CV; starting variance = %.5g; warm starts = %s; warm-start round offset = %s",
      method,
      lambdaDefault,
      configBase$warmStartLambdaPath,
      configBase$warmStartRoundOffset
    ))
  }
  tuned <- tuneLambda(
    cl = clTrain,
    algorithm = method,
    configBase = configBase,
    trainIds = seq_along(trainPopSizes),
    rounds = config$rounds,
    clientFrac = config$clientFrac,
    epsilon = config$epsilon,
    lambdaStrategy = lambdaStrategy,
    lambdaDefault = lambdaDefault,
    totalPopSize = totalPopSize,
    globalMap = globalMap,
    trainPopSizes = trainPopSizes,
    verbose = verbose
  )

  config$mapping <- globalMap
  config$p <- nrow(globalMap)
  config$lambda <- tuned$bestLambda
  config$selectedVariance <- tuned$bestSearchValue
  config$lambdaSearchTrace <- tuned$trace
  config$lambdaSearchStopReason <- tuned$stopReason
  config$innerCvScore <- tuned$perf %||% NA_real_
  config$tuningCommunication <- .combineCommunication(list(config$tuningCommunication,
    tuned$communication))
  if (isTRUE(verbose)) {
    message(sprintf(
      "Selected %s penalty: prior variance = %.5g, mean-loss lambda = %.5g, inner-CV log likelihood = %.5g",
      method,
      config$selectedVariance,
      config$lambda,
      config$innerCvScore
    ))
  }
  config
}

scoreFederatedConfigInnerCv <- function(method, clTrain, config, trainIds, verbose) {
  if (length(trainIds) < 2L) {
    stop("Config selection requires at least two training clients")
  }
  globalMap <- config$mapping %||% FederatedLearning::clusterCollectCovRefs(
    clTrain,
    type = config$mapType,
    featureSet = config$featureSet,
    covariateIds = config$covariateIds,
    analysisIds = config$analysisIds
  )
  config$mapping <- globalMap
  config$p <- nrow(globalMap)

  communication <- list(config$tuningCommunication)
  scores <- vapply(trainIds, function(valId) {
    train2 <- setdiff(trainIds, valId)
    trainCluster <- subsetCluster(clTrain, train2)
    valCluster <- subsetCluster(clTrain, valId)
    if (isTRUE(verbose)) {
      message(
        "Scoring ", method, " config on folds ", paste(train2, collapse = ""),
        " validating on fold ", valId
      )
    }
    fit <- FederatedLearning::fitFederated(
      cl = trainCluster,
      algorithm = method,
      config = config,
      verbose = verbose
    )
    .requireDualAvgConvergence(fit, config,
      paste0("config CV (validation client ", valId, ")"))
    valConfig <- fit$config
    valConfig$mapping <- fit$config$mapping
    valConfig$p <- nrow(fit$config$mapping)
    FederatedLearning::clusterCreateMatrices(valCluster, valConfig)
    ev <- FederatedLearning::clusterEvaluateModel(valCluster, fit$w)
    communication[[length(communication) + 1L]] <<- .asTuningCommunication(fit$communication)
    communication[[length(communication) + 1L]] <<- .validationCommunication(fit$w)
    .innerCvScoreFromEvaluation(ev)
  }, numeric(1))
  config$tuningCommunication <- .combineCommunication(communication)

  score <- .innerCvScoreMean(scores)
  if (!is.finite(score)) {
    stop("Unable to compute a finite inner-CV log likelihood for method ", method)
  }
  config$innerCvScore <- score
  config$innerCvScoreSd <- .innerCvScoreSd(scores)
  config
}

needsInnerCvConfigSelection <- function(method, configs, args) {
  if (length(configs) <= 1L) {
    return(FALSE)
  }
  if (!method %in% dualAvgMethods) {
    return(TRUE)
  }
  !shouldTuneDualAvg(args)
}

selectMethodConfigForFold <- function(method, clTrain, configs, trainPopSizes, args, verbose) {
  if (length(configs) == 1L) {
    config <- tuneDualAvgForFold(method, clTrain, configs[[1]], trainPopSizes, args, verbose)
    return(config)
  }
  tunedConfigs <- lapply(configs, function(config) {
    config <- tuneDualAvgForFold(method, clTrain, config, trainPopSizes, args, verbose)
    if (needsInnerCvConfigSelection(method, configs, args)) {
      config <- scoreFederatedConfigInnerCv(
        method = method,
        clTrain = clTrain,
        config = config,
        trainIds = seq_along(trainPopSizes),
        verbose = verbose
      )
    }
    config
  })
  scores <- vapply(tunedConfigs, function(config) {
    config$innerCvScore %||% NA_real_
  }, numeric(1))
  if (all(!is.finite(scores))) {
    best <- 1L
  } else {
    best <- which.max(scores)
  }
  # Selection pays for tuning every candidate configuration.
  tunedConfigs[[best]]$tuningCommunication <- .combineCommunication(
    lapply(tunedConfigs, `[[`, "tuningCommunication"))
  if (isTRUE(verbose)) {
    message(sprintf(
      "Selected %s config by inner CV: %s (inner-CV log likelihood = %.5g)",
      method,
      tunedConfigs[[best]]$configLabel %||% "default",
      scores[[best]]
    ))
  }
  tunedConfigs[[best]]
}

evaluateWeights <- function(clientData, w, clientId, clientIndex) {
  assertConformableWeights(
    w,
    clientData$xMatrix,
    context = "baseline evaluation"
  )
  lp <- as.numeric(clientData$xMatrix %*% w)
  evaluatePredictions(
    clientData = clientData,
    preds = stats::plogis(lp),
    density = mean(abs(w) > 1e-4),
    clientId = clientId,
    clientIndex = clientIndex,
    linearPredictor = lp
  )
}

# Held-out metrics of predicted probabilities. Single logistic models pass their
# linear predictor; a probability ensemble has none, so its probabilities are
# bounded away from 0 and 1 before taking logits for calibration.
evaluatePredictions <- function(clientData, preds, density, clientId, clientIndex,
                                linearPredictor = NULL) {
  y <- clientData$yLabels
  if (length(preds) != length(y)) {
    stop("Prediction length mismatch: got ", length(preds), " predictions for ", length(y), " labels")
  }
  auc <- binaryAuc(y, preds)
  eps <- 1e-15
  lp <- linearPredictor %||% stats::qlogis(pmin(pmax(preds, eps), 1 - eps))
  calibration <- .calibrationMetrics(y, lp)
  curve <- .calibrationCurve(y, lp)
  out <- data.frame(
    client = clientIndex,
    auc = auc,
    logLoss = if (is.null(linearPredictor)) logLoss(y, preds) else binaryLogLoss(linearPredictor, y),
    calibrationIci = curve$ici,
    calibrationInTheLarge = calibration[["calibrationInTheLarge"]],
    calibrationIntercept = calibration[["calibrationIntercept"]],
    calibrationSlope = calibration[["calibrationSlope"]],
    density = density,
    n = length(y),
    outcomes = sum(y),
    clientId = clientId,
    stringsAsFactors = FALSE
  )
  attr(out, "calibrationCurve") <- if (is.null(curve$curve)) NULL else cbind(clientId = clientId, curve$curve)
  out
}

# rbind() drops attributes, so evaluation rows keep their calibration curves explicitly.
bindEvaluations <- function(rows) {
  curves <- do.call(rbind, lapply(rows, attr, "calibrationCurve"))
  out <- do.call(rbind, rows)
  attr(out, "calibrationCurve") <- curves
  out
}

predictWithWeights <- function(clientData, w) {
  assertConformableWeights(
    w,
    clientData$xMatrix,
    context = "baseline prediction"
  )
  stats::plogis(as.numeric(clientData$xMatrix %*% w))
}

localModelWeights <- function(trainN, args) {
  weighting <- argValue(args, "local-ensemble-weighting") %||% "sampleSize"
  if (identical(weighting, "sampleSize")) {
    return(trainN / sum(trainN))
  }
  if (identical(weighting, "equalClient")) {
    return(rep(1 / length(trainN), length(trainN)))
  }
  stop("--local-ensemble-weighting must be one of: sampleSize, equalClient")
}

evaluateLocalEnsemble <- function(clientData, localFits, modelWeights,
                                  clientId, clientIndex) {
  predMat <- do.call(cbind, lapply(localFits, function(fit) {
    predictWithWeights(clientData, fit$w)
  }))
  preds <- as.numeric(predMat %*% modelWeights)
  density <- sum(vapply(localFits, function(fit) mean(abs(fit$w) > 1e-4), numeric(1)) * modelWeights)
  evaluatePredictions(
    clientData = clientData,
    preds = preds,
    density = density,
    clientId = clientId,
    clientIndex = clientIndex
  )
}

scoreLocalModelOnTrainingSites <- function(fit, trainData, validationIndexes) {
  scores <- vapply(validationIndexes, function(i) {
    ev <- evaluateWeights(
      clientData = trainData[[i]],
      w = fit$w,
      clientId = as.character(i),
      clientIndex = i
    )
    ev$auc[[1]]
  }, numeric(1))
  .innerCvScoreMean(scores)
}

pooledFitDiagnostics <- function(clientDataList, w, lambda = NA_real_,
                                 intercept = TRUE, tolerance = 1e-4) {
  ns <- vapply(clientDataList, function(x) nrow(x$xMatrix), numeric(1))
  totalN <- sum(ns)
  losses <- vapply(clientDataList, function(x) {
    logisticNegLogLik(w, x$xMatrix, x$yLabels, meanLoss = FALSE)
  }, numeric(1))
  grads <- do.call(cbind, lapply(clientDataList, function(x) {
    FederatedLearning::gradLogistic(w, x$xMatrix, x$yLabels)
  }))
  globalGradient <- as.numeric(grads %*% (ns / totalN))
  penalize <- rep(TRUE, length(w))
  if (length(penalize) > 0L && isTRUE(intercept)) {
    penalize[1] <- FALSE
  }
  active <- abs(w) > 1e-8
  kktViolation <- abs(globalGradient)
  if (length(lambda) == 1L && is.finite(lambda) && lambda >= 0) {
    for (j in seq_along(globalGradient)) {
      if (isTRUE(penalize[j])) {
        kktViolation[j] <- if (isTRUE(active[j])) {
          abs(globalGradient[j] + lambda * sign(w[j]))
        } else {
          max(abs(globalGradient[j]) - lambda, 0)
        }
      }
    }
  }
  finiteKkt <- is.finite(kktViolation)
  list(
    pooledNegLogLik = sum(losses),
    pooledMeanLogLoss = sum(losses) / totalN,
    pooledGradientMaxAbs = max(abs(globalGradient), na.rm = TRUE),
    pooledKktMaxAbs = if (any(finiteKkt)) max(kktViolation[finiteKkt]) else NA_real_,
    pooledKktViolating = if (any(finiteKkt)) sum(kktViolation[finiteKkt] > tolerance) else NA_integer_,
    pooledKktMaxCoordinate = if (any(finiteKkt)) which.max(kktViolation) else NA_integer_
  )
}

baselinePreprocessSettings <- function(args) {
  enabled <- logicalArg(argValue(args, "baseline-preprocess"), TRUE)
  list(
    enabled = enabled,
    minFraction = numArg(
      argValue(args, "baseline-preprocess-min-fraction"),
      numArg(argValue(args, "preprocess-min-fraction"), 0.001)
    ),
    normalize = logicalArg(
      argValue(args, "baseline-preprocess-normalize") %||% argValue(args, "preprocess-normalize"),
      FALSE
    ),
    removeRedundancy = logicalArg(
      argValue(args, "baseline-preprocess-remove-redundancy") %||% argValue(args, "preprocess-remove-redundancy"),
      TRUE
    )
  )
}

federatedPreprocessMode <- function(args) {
  mode <- argValue(args, "federated-preprocess") %||% "none"
  if (!mode %in% c("none", "baseline")) {
    stop("--federated-preprocess must be none or baseline")
  }
  if (mode == "baseline") {
    settings <- baselinePreprocessSettings(args)
    if (!isTRUE(settings$enabled) || isTRUE(settings$normalize)) {
      stop("Matched filtering requires baseline preprocessing enabled and normalization disabled")
    }
    if (!is.finite(settings$minFraction) || settings$minFraction < 0 || settings$minFraction > 1) {
      stop("Matched preprocessing min-fraction must be between 0 and 1")
    }
  }
  mode
}

guardPreprocessingSettings <- function(args, directory) {
  mode <- federatedPreprocessMode(args)
  contract <- list(version = 1L, mode = mode, scope = "outer-training",
    settings = baselinePreprocessSettings(args))
  if (mode == "baseline") contract$matrixSettings <- preprocessingMatrixSettings(
    methodConfig("PooledLasso", "all", args))
  path <- file.path(directory, "preprocessing_settings.rds")
  if (file.exists(path)) {
    if (!identical(readRDS(path), contract)) {
      stop("Preprocessing settings differ from this result directory. Use a new result directory.")
    }
  } else {
    if (mode != "none" && file.exists(file.path(directory, "comparison_results.csv"))) {
      stop("Cannot mix matched preprocessing with unverified existing results. Use a new result directory.")
    }
    saveRDS(contract, path)
  }
  invisible(contract)
}

preprocessingMatrixSettings <- function(config) {
  fields <- c("mapType", "intercept", "covariateIds", "analysisIds",
    "diagnosticControlsPerCase", "diagnosticDownsampleSeed")
  stats::setNames(lapply(fields, function(name) config[[name, exact = TRUE]]), fields)
}

fitFederatedPreprocessor <- function(cl, config, args) {
  collectTrainingFeatureFilter(cl, config, baselinePreprocessSettings(args))
}

checkPreprocessingReference <- function(preprocessor, directory, task, fold, featureSet,
                                        trainClientIds, testClientIds, popSettings, config) {
  if (is.null(directory)) return("not_requested")
  rows <- readCsvIfExists(file.path(directory, "comparison_results.csv"))
  if (!nonEmptyRows(rows)) stop("Missing pooled results for preprocessing comparison")
  rows <- rows[matchingCombination(rows, task, fold, featureSet, "PooledLasso"), , drop = FALSE]
  model <- readModelArtifact(rows, directory, task, fold, featureSet, "PooledLasso")
  if (is.null(model)) stop("Missing saved pooled reference for preprocessing comparison")
  # Numeric and integer storage represent the same settings, but values must match exactly.
  if (!isTRUE(all.equal(model$populationSettings, popSettings, tolerance = 0)) ||
      !setequal(trimws(model$trainClientIds), trainClientIds) ||
      !setequal(trimws(model$testClientIds), testClientIds) ||
      !identical(model$config$mapType, config$mapType) ||
      !identical(model$config$intercept, config$intercept) ||
      !identical(model$preprocessing$settings, preprocessor$settings)) {
    stop("Pooled preprocessing reference has different population, sites, or settings")
  }
  if (!preprocessingMatchesModel(preprocessor, model, trainClientIds)) {
    stop("Matched filter differs from the saved pooled feature mask or training row count")
  }
  "matched"
}

savePreprocessingAudit <- function(preprocessor, directory, task, fold, featureSet,
                                   trainClientIds, testClientIds, popSettings) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  stem <- paste(safeFilePart(task), paste0("fold", fold), safeFilePart(featureSet), sep = "_")
  path <- file.path(directory, paste0(stem, ".rds"))
  preprocessor$trainClientIds <- trainClientIds
  names(preprocessor$trainingSampleSizes) <- trainClientIds
  preprocessor$testClientIds <- testClientIds
  preprocessor$populationSettings <- popSettings
  if (file.exists(path)) {
    old <- readRDS(path)
    fields <- c("audit", "settings", "trainClientIds", "testClientIds", "populationSettings", "trainingSampleSizes")
    if (!isTRUE(all.equal(old[fields], preprocessor[fields], tolerance = 1e-12))) {
      stop("Preprocessing audit changed for this fold. Use a new result directory.")
    }
  }
  saveRDS(preprocessor, path)
  audit <- cbind(task = task, fold = fold, featureSet = featureSet,
    fingerprint = preprocessor$fingerprint, referenceStatus = preprocessor$referenceStatus,
    preprocessor$audit)
  utils::write.csv(audit, file.path(directory, paste0(stem, ".csv")), row.names = FALSE)
  summaryPath <- file.path(directory, "summary.csv")
  summaries <- readCsvIfExists(summaryPath)
  current <- data.frame(task = task, fold = fold, featureSet = featureSet,
    candidatePredictors = nrow(audit), retainedPredictors = sum(audit$retained),
    removedRare = sum(audit$removedRare), removedNearConstant = sum(audit$removedNearConstant),
    requiresRefit = any(!audit$retained), fingerprint = preprocessor$fingerprint,
    referenceStatus = preprocessor$referenceStatus)
  if (nonEmptyRows(summaries)) {
    same <- summaries$task == task & summaries$fold == fold & summaries$featureSet == featureSet
    current <- rbind(summaries[!same, , drop = FALSE], current)
  }
  utils::write.csv(current, summaryPath, row.names = FALSE)
  message(sprintf("Preprocessing %s fold %s %s: retained %s/%s, rare %s, near-constant %s, pooled reference: %s",
    task, fold, featureSet, sum(audit$retained), nrow(audit), sum(audit$removedRare),
    sum(audit$removedNearConstant), preprocessor$referenceStatus))
  preprocessor
}

sparseColumnMax <- function(x) {
  if (!inherits(x, "sparseMatrix")) {
    return(apply(as.matrix(x), 2, max, na.rm = TRUE))
  }
  x <- methods::as(x, "dgCMatrix")
  out <- numeric(ncol(x))
  for (j in seq_len(ncol(x))) {
    start <- x@p[[j]] + 1L
    end <- x@p[[j + 1L]]
    out[[j]] <- if (start <= end) max(0, x@x[start:end], na.rm = TRUE) else 0
  }
  out
}

fitBaselinePreprocessor <- function(trainData, intercept = TRUE, args) {
  settings <- baselinePreprocessSettings(args)
  p <- ncol(trainData[[1]]$xMatrix)
  featureCols <- if (isTRUE(intercept)) seq.int(2L, p) else seq_len(p)
  if (!isTRUE(settings$enabled) || length(featureCols) == 0L) {
    return(list(
      enabled = FALSE,
      featureCols = featureCols,
      keep = rep(TRUE, length(featureCols)),
      normFactors = rep(1, length(featureCols)),
      settings = settings
    ))
  }

  xTrain <- do.call(rbind, lapply(trainData, `[[`, "xMatrix"))
  xFeatures <- xTrain[, featureCols, drop = FALSE]
  nTrain <- nrow(xFeatures)
  nnz <- Matrix::colSums(xFeatures != 0)
  selection <- baselineFeatureSelection(nTrain, nnz, Matrix::colMeans(xFeatures),
    Matrix::colMeans(xFeatures^2), settings)
  keep <- selection$keep

  if (!any(keep)) {
    stop("Baseline preprocessing removed all non-intercept covariates")
  }

  normFactors <- rep(1, length(featureCols))
  if (isTRUE(settings$normalize)) {
    maxValues <- sparseColumnMax(xFeatures[, keep, drop = FALSE])
    maxValues[!is.finite(maxValues) | maxValues == 0] <- 1
    normFactors[keep] <- as.numeric(maxValues)
  }

  list(
    enabled = TRUE,
    featureCols = featureCols,
    keep = keep,
    normFactors = normFactors,
    removedRare = sum(nnz < floor(settings$minFraction * nTrain)),
    removed = sum(!keep),
    settings = settings
  )
}

applyBaselinePreprocessor <- function(clientData, preprocessor, intercept = TRUE) {
  if (!isTRUE(preprocessor$enabled)) {
    return(clientData)
  }
  x <- clientData$xMatrix
  p <- ncol(x)
  featureCols <- preprocessor$featureCols
  keep <- preprocessor$keep
  normFactors <- preprocessor$normFactors[keep]
  retained <- featureCols[keep]

  if (isTRUE(intercept)) {
    xOut <- x[, 1L, drop = FALSE]
    if (length(retained) > 0L) {
      xFeatures <- x[, retained, drop = FALSE]
      xFeatures <- xFeatures %*% Matrix::Diagonal(x = 1 / normFactors)
      xOut <- cbind(xOut, xFeatures)
    }
  } else {
    xOut <- x[, retained, drop = FALSE]
    if (length(retained) > 0L) {
      xOut <- xOut %*% Matrix::Diagonal(x = 1 / normFactors)
    }
  }

  moments <- matrixMoments(xOut, intercept = intercept)
  clientData$xMatrix <- xOut
  clientData$xMeans <- moments$xMeans
  clientData$x2Means <- moments$x2Means
  clientData$n <- nrow(xOut)
  clientData
}

preprocessBaselineData <- function(trainData, testData = NULL, config, args) {
  preprocessor <- fitBaselinePreprocessor(
    trainData = trainData,
    intercept = isTRUE(config$intercept),
    args = args
  )
  trainData <- lapply(trainData, applyBaselinePreprocessor,
    preprocessor = preprocessor,
    intercept = isTRUE(config$intercept)
  )
  if (!is.null(testData)) {
    testData <- lapply(testData, applyBaselinePreprocessor,
      preprocessor = preprocessor,
      intercept = isTRUE(config$intercept)
    )
  }
  list(trainData = trainData, testData = testData, preprocessor = preprocessor)
}

# Fits a Cyclops lasso to the stacked rows of clientDataList. With a fixed
# variance it skips Cyclops cross-validation and uses that prior variance.
fitCyclopsWeights <- function(clientDataList, args, seed, intercept = TRUE, variance = NULL) {
  if (!requireNamespace("Cyclops", quietly = TRUE)) {
    stop("Cyclops is required for pooled/local baseline models")
  }
  x <- do.call(rbind, lapply(clientDataList, `[[`, "xMatrix"))
  y <- unlist(lapply(clientDataList, `[[`, "yLabels"), use.names = FALSE)
  if (length(unique(y)) < 2) {
    stop("Cannot fit Cyclops logistic model: training data has only one outcome class")
  }
  if (isTRUE(intercept) && (ncol(x) < 1L || anyNA(x[, 1L]) || any(x[, 1L] != 1))) {
    stop("Configured intercept must be the first matrix column, containing only ones")
  }

  start <- Sys.time()

  cyclopsData <- Cyclops::createCyclopsData(y = y, sx = x, modelType = "lr")
  # A constant sparse column is not automatically recognized as an intercept.
  excluded <- if (isTRUE(intercept)) as.numeric(Cyclops::getCovariateIds(cyclopsData)[1L]) else NULL
  useCv <- is.null(variance) && logicalArg(argValue(args, "cyclops-cv"), TRUE)
  variance <- variance %||%
    numArg(argValue(args, "cyclops-variance"), numArg(argValue(args, "baseline-variance"), 0.01))
  startingVariance <- numArg(
    argValue(args, "cyclops-starting-variance"),
    numArg(argValue(args, "baseline-starting-variance"), 0.01)
  )
  prior <- Cyclops::createPrior(
    "laplace",
    variance = variance,
    exclude = excluded,
    useCrossValidation = useCv
  )
  control <- Cyclops::createControl(
    maxIterations = intArg(argValue(args, "cyclops-max-iterations"), intArg(argValue(args, "baseline-maxit"), 3000L)),
    tolerance = numArg(argValue(args, "cyclops-tolerance"), 2e-6),
    cvType = argValue(args, "cyclops-cv-type") %||% "auto",
    fold = intArg(argValue(args, "cyclops-folds"), intArg(argValue(args, "baseline-folds"), 10L)),
    lowerLimit = numArg(argValue(args, "cyclops-lower-limit"), 0.01),
    upperLimit = numArg(argValue(args, "cyclops-upper-limit"), 20),
    noiseLevel = argValue(args, "cyclops-noise-level") %||% "silent",
    threads = intArg(argValue(args, "cyclops-threads"), 1L),
    seed = seed,
    selectorType = argValue(args, "cyclops-selector-type") %||% "auto",
    startingVariance = startingVariance
  )

  fit <- Cyclops::fitCyclopsModel(
    cyclopsData,
    prior = prior,
    control = control,
    warnings = logicalArg(argValue(args, "cyclops-warnings"), TRUE)
  )
  w <- as.numeric(stats::coef(fit))
  if (length(w) != ncol(x)) {
    stop(sprintf(
      "Cyclops coefficient dimension mismatch: expected %s coefficients but got %s",
      ncol(x),
      length(w)
    ))
  }

  selectedVariance <- if (!is.null(fit$variance)) {
    as.numeric(fit$variance)[[1]]
  } else {
    variance
  }
  list(
    w = w,
    selectedLambda = selectedVariance,
    fittingSettings = list(
      penaltyScale = "Cyclops Laplace prior variance",
      intercept = isTRUE(intercept),
      unpenalizedCovariates = excluded,
      useCrossValidation = useCv,
      requestedControl = control,
      seed = seed,
      selectedVariance = selectedVariance,
      returnFlag = fit$return_flag,
      logLikelihood = fit$log_likelihood,
      logPrior = fit$log_prior,
      priorInfo = fit$prior_info
    ),
    elapsedSeconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}

fitBaselineWeights <- function(clientDataList, args, seed, intercept = TRUE) {
  if (logicalArg(argValue(args, "cyclops-cv"), TRUE)) {
    if (length(clientDataList) == 1L) return(tuneLocalCyclopsFit(clientDataList[[1]], args, seed, intercept))
    return(tunePooledCyclopsFit(clientDataList, args, seed, intercept))
  }
  fitCyclopsWeights(
    clientDataList = clientDataList,
    args = args,
    seed = seed,
    intercept = intercept
  )
}

clientRows <- function(clientData, rows) {
  list(xMatrix = clientData$xMatrix[rows, , drop = FALSE], yLabels = clientData$yLabels[rows],
    n = length(rows))
}

# Local lasso: the prior variance maximises the validation log likelihood of
# outcome-stratified folds within the site, pooled over held-out patients, and
# the model is then refitted on all of the site's training data.
tuneLocalCyclopsFit <- function(clientData, args, seed, intercept = TRUE) {
  start <- Sys.time()
  k <- intArg(argValue(args, "local-cv-folds"), 3L)
  y <- clientData$yLabels
  folds <- .stratifiedFoldId(y, k, seed)
  for (fold in seq_len(k)) {
    if (length(unique(y[folds != fold])) < 2L || !any(folds == fold)) {
      stop("Local lasso CV needs both outcome classes in every training fold and nonempty folds")
    }
  }
  scoreVariance <- function(variance) {
    loss <- vapply(seq_len(k), function(fold) {
      fit <- fitCyclopsWeights(list(clientRows(clientData, which(folds != fold))), args, seed,
        intercept, variance = variance)
      held <- which(folds == fold)
      binaryLogLoss(as.numeric(clientData$xMatrix[held, , drop = FALSE] %*% fit$w), y[held],
        meanLoss = FALSE)
    }, numeric(1))
    sizes <- tabulate(folds, k)
    c(-sum(loss) / sum(sizes), stats::sd(-loss / sizes))
  }
  search <- .searchPriorVariance(scoreVariance,
    startingVariance = numArg(argValue(args, "cyclops-starting-variance"),
      numArg(argValue(args, "baseline-starting-variance"), 0.01)),
    maxEvals = intArg(argValue(args, "lambda-search-max-evals"), 25L))
  fit <- fitCyclopsWeights(list(clientData), args, seed, intercept, variance = search$variance)
  fit$fittingSettings$tuning <- list(
    validation = "stratifiedFolds", folds = k, seed = seed,
    foldSizes = tabulate(folds, k), foldOutcomes = tabulate(folds[y == 1], k),
    score = "pooled validation log likelihood per patient",
    trace = search$trace, stopReason = search$stopReason)
  fit$elapsedSeconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  fit
}

# Pooled lasso: the prior variance maximises the mean validation log likelihood
# when each training database is held out in turn, weighting databases equally,
# matching DualAvg tuning. The model is then refitted on all training databases.
tunePooledCyclopsFit <- function(clientDataList, args, seed, intercept = TRUE) {
  start <- Sys.time()
  databases <- seq_along(clientDataList)
  for (held in databases) {
    if (length(unique(unlist(lapply(clientDataList[-held], `[[`, "yLabels")))) < 2L) {
      stop("Pooled lasso tuning needs both outcome classes when each training database is held out")
    }
  }
  scoreVariance <- function(variance) {
    scores <- vapply(databases, function(held) {
      fit <- fitCyclopsWeights(clientDataList[-held], args, seed, intercept, variance = variance)
      data <- clientDataList[[held]]
      -binaryLogLoss(as.numeric(data$xMatrix %*% fit$w), data$yLabels)
    }, numeric(1))
    c(mean(scores), stats::sd(scores))
  }
  search <- .searchPriorVariance(scoreVariance,
    startingVariance = numArg(argValue(args, "cyclops-starting-variance"),
      numArg(argValue(args, "baseline-starting-variance"), 0.01)),
    maxEvals = intArg(argValue(args, "lambda-search-max-evals"), 25L))
  fit <- fitCyclopsWeights(clientDataList, args, seed, intercept, variance = search$variance)
  fit$fittingSettings$tuning <- list(
    validation = "trainingDatabases", databases = length(databases),
    databaseSizes = vapply(clientDataList, function(x) length(x$yLabels), integer(1)),
    databaseOutcomes = vapply(clientDataList, function(x) as.integer(sum(x$yLabels == 1)), integer(1)),
    score = "mean over databases of validation log likelihood per patient",
    trace = search$trace, stopReason = search$stopReason)
  fit$elapsedSeconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  fit
}

fitLocalBaselineSafely <- function(trainData, localIndex, trainClientId, args, seed, intercept = TRUE) {
  start <- Sys.time()
  tryCatch({
    fit <- fitBaselineWeights(
      list(trainData[[localIndex]]),
      args = args,
      seed = seed,
      intercept = intercept
    )
    list(
      ok = TRUE,
      fit = fit,
      localIndex = localIndex,
      trainClientId = trainClientId,
      elapsedSeconds = fit$elapsedSeconds %||% as.numeric(difftime(Sys.time(), start, units = "secs")),
      error = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      fit = NULL,
      localIndex = localIndex,
      trainClientId = trainClientId,
      elapsedSeconds = as.numeric(difftime(Sys.time(), start, units = "secs")),
      error = conditionMessage(e)
    )
  })
}

assertCyclopsMethod <- function(method) {
  if (!method %in% baselineMethods) {
    return(invisible(NULL))
  }
  if (!requireNamespace("Cyclops", quietly = TRUE)) {
    stop(
      "Cyclops is required for baseline method '",
      method,
      "'. Install Cyclops or remove the baseline from --methods."
    )
  }
  invisible(NULL)
}

sparseAverageSettings <- function(args) {
  fixed <- argValue(args, "local-average-threshold")
  grid <- c(0, numCsvArg(argValue(args, "local-average-thresholds"),
    c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)))
  if (!is.null(fixed)) grid <- numArg(fixed, NA_real_)
  if (!length(grid) || any(!is.finite(grid) | grid < 0)) stop("Invalid averaging threshold(s)")
  grid <- sort(unique(grid))
  metric <- argValue(args, "local-average-metric") %||% "auc"
  if (!metric %in% c("auc", "logLoss")) stop("--local-average-metric must be auc or logLoss")
  list(grid = grid, fixed = !is.null(fixed), metric = metric,
    multiplier = numArg(argValue(args, "local-debias-multiplier"), 1),
    maxFeatures = intArg(argValue(args, "local-debias-max-features"), 512L))
}

averageSparseLocalFits <- function(fits, trainData, method, settings) {
  sizes <- vapply(trainData, function(x) nrow(x$xMatrix), numeric(1))
  if (length(fits) != length(trainData) || !length(fits) || any(sizes <= 0)) {
    stop("Sparse averaging requires matching fits and nonempty training clients")
  }
  for (i in seq_along(fits)) assertConformableWeights(
    fits[[i]]$w, trainData[[i]]$xMatrix, context = "sparse averaging")
  debias <- if (method == "DebiasedLocalAvgLasso") {
    Map(function(fit, data) debiasLocalLasso(data, fit$w,
      settings$multiplier, settings$maxFeatures), fits, trainData)
  } else NULL
  coefficients <- if (is.null(debias)) lapply(fits, `[[`, "w") else lapply(debias, `[[`, "w")
  weights <- sizes / sum(sizes)
  list(w = Reduce("+", Map(`*`, coefficients, weights)), weights = weights, debias = debias)
}

sparseAverageScales <- function(preprocessor, p, intercept) {
  scales <- rep(1, p)
  if (isTRUE(preprocessor$enabled)) {
    slopes <- seq_len(p)
    if (isTRUE(intercept)) slopes <- slopes[-1L]
    scales[slopes] <- preprocessor$normFactors[preprocessor$keep]
  }
  scales
}

tuneSparseAverage <- function(trainData, method, args, config, seed) {
  settings <- sparseAverageSettings(args)
  if (settings$fixed) return(list(tau = settings$grid, metric = settings$metric,
    score = NA_real_, trace = data.frame(), splits = list(), settings = settings))
  if (length(trainData) < 2L) stop("Threshold tuning requires at least two training sites")
  trace <- splits <- vector("list", length(trainData))
  for (validation in seq_along(trainData)) {
    training <- setdiff(seq_along(trainData), validation)
    processed <- preprocessBaselineData(trainData[training], trainData[validation], config, args)
    attempts <- lapply(seq_along(training), function(i) fitLocalBaselineSafely(
      processed$trainData, i, as.character(training[i]), args, seed + training[i], config$intercept))
    ok <- vapply(attempts, `[[`, logical(1), "ok")
    if (!any(ok)) stop("No local fits succeeded in threshold validation split ", validation)
    aggregate <- averageSparseLocalFits(lapply(attempts[ok], `[[`, "fit"),
      processed$trainData[ok], method, settings)
    scales <- sparseAverageScales(processed$preprocessor, length(aggregate$w), config$intercept)
    data <- processed$testData[[1]]
    scores <- vapply(settings$grid, function(tau) {
      w <- softThresholdAverage(aggregate$w, tau, config$intercept, scales)
      if (settings$metric == "auc") {
        binaryAuc(data$yLabels, stats::plogis(as.numeric(data$xMatrix %*% w)))
      } else {
        logisticNegLogLik(w, data$xMatrix, data$yLabels, meanLoss = TRUE)
      }
    }, numeric(1))
    if (any(!is.finite(scores))) stop("Non-finite threshold validation score at training site ", validation)
    trace[[validation]] <- data.frame(validationSite = validation, tau = settings$grid,
      score = scores, metric = settings$metric, successfulLocalFits = sum(ok))
    splits[[validation]] <- list(trainingSites = training, validationSite = validation,
      preprocessing = processed$preprocessor, failures = attempts[!ok],
      contributingSites = training[ok], weights = aggregate$weights, debias = aggregate$debias,
      localFits = lapply(attempts[ok], `[[`, "fit"))
  }
  trace <- do.call(rbind, trace)
  scores <- vapply(settings$grid, function(tau) mean(trace$score[trace$tau == tau]), numeric(1))
  # The sorted grid deliberately prefers the smallest threshold on exact ties.
  best <- if (settings$metric == "auc") which.max(scores) else which.min(scores)
  list(tau = settings$grid[best], metric = settings$metric, score = scores[best],
    trace = trace, splits = splits, settings = settings)
}

# Baselines run centrally, so their transfers are modeled: each site sends its
# coefficients once, and LocalBestLasso scores each model at the other training
# sites. Pooled data and a tuned sparse-averaging threshold are not counted.
baselineCommunication <- function(method, p, nModels, nTrainSites, thresholdTuned = FALSE) {
  if (identical(method, "PooledLasso")) {
    return(list(fit = NA, tuning = NA, basis = "pooledData"))
  }
  if (identical(method, "LocalSiteLasso")) {
    return(list(fit = NULL, tuning = NULL, basis = "none"))
  }
  model <- numeric(p)
  log <- .newCommunicationLog()
  if (identical(method, "BiggestSiteLasso")) {
    .recordExchange(log, "site sizes", vector("list", nTrainSites), rep(list(0), nTrainSites))
    .recordExchange(log, "local models", list(NULL), list(model))
  } else {
    .recordExchange(log, "local models", vector("list", nModels), rep(list(model), nModels))
  }
  tuning <- NULL
  if (identical(method, "LocalBestLasso")) {
    tuning <- .validationCommunication(model, nModels * (nTrainSites - 1L))
  } else if (thresholdTuned) {
    tuning <- NA
  }
  list(fit = .communicationTable(log), tuning = tuning, basis = "modeled")
}

fitBaselineFold <- function(method, trainPaths, testPaths, popSettings, config,
                            args, task, featureSet, fold, trainClientIds,
                            testClientIds, testClientIndexes, modelDirectory = NULL) {
  assertCyclopsMethod(method)

  trainPlp <- lapply(trainPaths, FederatedLearning::loadClientData, popSettings = popSettings)
  trainMap <- FederatedLearning::createGlobalMap(
    lapply(trainPlp, FederatedLearning::getClientFeatures),
    type = config$mapType,
    featureSet = featureSelection(featureSet, args),
    covariateIds = config$covariateIds,
    analysisIds = config$analysisIds
  )
  if (nrow(trainMap) == 0L) {
    stop("Global feature map is empty for the requested feature set")
  }
  matrixConfig <- config
  matrixConfig$mapping <- trainMap
  matrixConfig$p <- nrow(trainMap)
  trainData <- lapply(trainPlp, FederatedLearning::createClientMatrix, config = matrixConfig)

  thresholdStart <- Sys.time()
  thresholdTuning <- if (method %in% sparseAverageMethods) {
    tuneSparseAverage(trainData, method, args, config, intArg(args[["baseline-seed"]], 42L) + fold)
  } else NULL
  thresholdElapsed <- as.numeric(difftime(Sys.time(), thresholdStart, units = "secs"))

  testPlp <- lapply(testPaths, FederatedLearning::loadClientData, popSettings = popSettings)
  testData <- lapply(testPlp, FederatedLearning::createClientMatrix, config = matrixConfig)

  processed <- preprocessBaselineData(
    trainData = trainData,
    testData = testData,
    config = config,
    args = args
  )
  if (!is.null(config$featureFiltering) &&
      (!identical(as.character(trainMap$covariateId), config$featureFiltering$audit$covariateId) ||
       !identical(as.logical(processed$preprocessor$keep), config$featureFiltering$audit$retained))) {
    stop("Centralized baseline filter differs from the shared training mask")
  }
  trainData <- processed$trainData
  testData <- processed$testData
  localFits <- list()
  localFailures <- list()
  localIndexesOk <- integer()
  weights <- numeric()

  if (identical(method, "PooledLasso")) {
    fits <- list(fitBaselineWeights(
      trainData,
      args = args,
      seed = intArg(args[["baseline-seed"]], 42L) + fold,
      intercept = isTRUE(config$intercept)
    ))
    fits[[1]]$leadIndex <- NA_integer_
    fits[[1]]$innerCvScore <- NA_real_
  } else {
    eligible <- vapply(trainData, function(x) length(unique(x$yLabels)) == 2, logical(1))
    if (!any(eligible)) {
      stop("Cannot fit local Cyclops baseline: no training client has both outcome classes")
    }
    localIndexes <- which(eligible)
    localAttempts <- lapply(localIndexes, function(i) {
      fitLocalBaselineSafely(
        trainData = trainData,
        localIndex = i,
        trainClientId = trainClientIds[[i]],
        args = args,
        seed = intArg(args[["baseline-seed"]], 42L) + fold + i,
        intercept = isTRUE(config$intercept)
      )
    })
    localOk <- vapply(localAttempts, `[[`, logical(1), "ok")
    localFailures <- localAttempts[!localOk]
    if (!any(localOk)) {
      failureSummary <- paste(
        vapply(localFailures, function(x) paste0(x$trainClientId, ": ", x$error), character(1)),
        collapse = "; "
      )
      stop("Cannot fit local Cyclops baseline: no eligible training client converged. ", failureSummary)
    }
    localFits <- lapply(localAttempts[localOk], `[[`, "fit")
    localIndexesOk <- vapply(localAttempts[localOk], `[[`, integer(1), "localIndex")
    localFailuresForRows <- if (identical(method, "LocalSiteLasso")) localFailures else list()
    trainN <- vapply(trainData[localIndexesOk], `[[`, numeric(1), "n")
    failureLabel <- if (length(localFailures)) paste0(";failedLocalFits=", length(localFailures)) else ""
    if (identical(method, "BiggestSiteLasso")) {
      fits <- list(localFits[[which.max(trainN)]])
      fits[[1]]$leadIndex <- localIndexesOk[[which.max(trainN)]]
      fits[[1]]$innerCvScore <- NA_real_
      fits[[1]]$configLabel <- paste0("source=", trainClientIds[[fits[[1]]$leadIndex]], failureLabel)
    } else if (method %in% c("LocalAvgLasso", sparseAverageMethods)) {
      weights <- trainN / sum(trainN)
      fits <- list(list(
        w = Reduce("+", Map(function(localFit, wi) localFit$w * wi, localFits, weights)),
        selectedLambda = NA_real_,
        elapsedSeconds = sum(vapply(localFits, `[[`, numeric(1), "elapsedSeconds")),
        leadIndex = NA_integer_,
        innerCvScore = NA_real_,
        configLabel = paste0("coefficientAverage=sampleSize", failureLabel)
      ))
      if (method %in% sparseAverageMethods) {
        aggregateStart <- Sys.time()
        aggregate <- averageSparseLocalFits(localFits, trainData[localIndexesOk], method, thresholdTuning$settings)
        scales <- sparseAverageScales(processed$preprocessor, length(aggregate$w), config$intercept)
        fits[[1]]$w <- softThresholdAverage(aggregate$w,
          thresholdTuning$tau, config$intercept, scales)
        fits[[1]]$aggregationThreshold <- thresholdTuning$tau
        fits[[1]]$innerCvScore <- thresholdTuning$score
        fits[[1]]$fittingSettings <- list(thresholdTuning = thresholdTuning,
          unthresholdedAverage = aggregate$w, debias = aggregate$debias,
          thresholdScale = "shared coefficient units before baseline normalization")
        fits[[1]]$elapsedSeconds <- thresholdElapsed +
          sum(vapply(localAttempts, `[[`, numeric(1), "elapsedSeconds")) +
          as.numeric(difftime(Sys.time(), aggregateStart, units = "secs"))
        fits[[1]]$configLabel <- paste0("coefficientAverage=sampleSize;tau=", thresholdTuning$tau,
          ";thresholdMetric=", thresholdTuning$metric, failureLabel)
      }
    } else if (identical(method, "LocalEnsembleLasso")) {
      weighting <- argValue(args, "local-ensemble-weighting") %||% "sampleSize"
      weights <- localModelWeights(trainN, args)
      fits <- list(list(
        localFits = localFits,
        localWeights = weights,
        w = Reduce("+", Map(function(localFit, wi) localFit$w * wi, localFits, weights)),
        selectedLambda = NA_real_,
        elapsedSeconds = sum(vapply(localFits, `[[`, numeric(1), "elapsedSeconds")),
        leadIndex = NA_integer_,
        innerCvScore = NA_real_,
        configLabel = paste0("predictionAverage=", weighting, failureLabel)
      ))
    } else if (identical(method, "LocalBestLasso")) {
      scores <- vapply(seq_along(localFits), function(j) {
        scoreLocalModelOnTrainingSites(
          fit = localFits[[j]],
          trainData = trainData,
          validationIndexes = setdiff(seq_along(trainData), localIndexesOk[[j]])
        )
      }, numeric(1))
      best <- if (all(!is.finite(scores))) which.max(trainN) else which.max(scores)
      fits <- list(localFits[[best]])
      fits[[1]]$leadIndex <- localIndexesOk[[best]]
      fits[[1]]$innerCvScore <- scores[[best]]
      fits[[1]]$configLabel <- paste0("selectedBy=innerCv;source=", trainClientIds[[fits[[1]]$leadIndex]], failureLabel)
    } else if (identical(method, "LocalSiteLasso")) {
      fits <- Map(function(localFit, localIndex) {
        localFit$leadIndex <- localIndex
        localFit$innerCvScore <- NA_real_
        localFit$configLabel <- paste0("source=", trainClientIds[[localIndex]])
        localFit
      }, localFits, localIndexesOk)
    } else {
      stop("Unknown baseline method: ", method)
    }
  }

  communication <- baselineCommunication(method, length(fits[[1]]$w),
    if (method %in% c("PooledLasso", "BiggestSiteLasso")) 1L else length(localFits),
    length(trainData), method %in% sparseAverageMethods && !isTRUE(thresholdTuning$settings$fixed))
  calibrationCurves <- list()
  baselineRows <- lapply(fits, function(fit) {
    pooledDiag <- if (isTRUE(config$pooledDiagnostics %||% TRUE) && is.null(fit$localFits)) {
      # Pooled lasso is checked against its own penalty; the other baselines,
      # which have no single pooled penalty, against the unpenalized likelihood.
      pooledLambda <- if (identical(method, "PooledLasso")) {
        .laplaceVarianceToLambda(fit$fittingSettings$selectedVariance,
          sum(vapply(trainData, function(x) nrow(x$xMatrix), numeric(1))))
      } else {
        NA_real_
      }
      pooledFitDiagnostics(
        trainData,
        fit$w,
        lambda = pooledLambda,
        intercept = config$intercept,
        tolerance = config$pooledKktTolerance %||% 1e-4
      )
    } else {
      list()
    }

    evalRows <- if (!is.null(fit$localFits)) {
      bindEvaluations(Map(
        evaluateLocalEnsemble,
        clientData = testData,
        clientId = testClientIds,
        clientIndex = testClientIndexes,
        MoreArgs = list(localFits = fit$localFits, modelWeights = fit$localWeights)
      ))
    } else {
      bindEvaluations(Map(
        evaluateWeights,
        clientData = testData,
        clientId = testClientIds,
        clientIndex = testClientIndexes,
        MoreArgs = list(w = fit$w)
      ))
    }
    lead <- fit$leadIndex %||% NA_integer_
    fitCurves <- attr(evalRows, "calibrationCurve")
    if (!is.null(fitCurves)) {
      calibrationCurves[[length(calibrationCurves) + 1L]] <<- cbind(
        sourceClientId = if (length(lead) == 1L && !is.na(lead)) trainClientIds[[lead]] else NA_character_,
        fitCurves)
    }

    cbind(
      data.frame(
        method = method,
        featureSet = featureSet,
        fold = fold,
        p = length(fit$w),
        roundsCompleted = NA_integer_,
        nonzeroPredictors = if (is.null(fit$localFits)) exactNonzero(fit$w, config$intercept) else NA_integer_,
        penaltyScale = "Cyclops Laplace prior variance",
        selectedLambda = fit$selectedLambda %||% NA_real_,
        aggregationThreshold = fit$aggregationThreshold %||% NA_real_,
        lambdaPathFile = NA_character_,
        leadIndex = fit$leadIndex %||% NA_integer_,
        leadWeight = NA_real_,
        leadWeightMin = NA_real_,
        trainObjective = fit$innerCvScore %||% NA_real_,
        hessianDim = NA_character_,
        hessianDiagMin = NA_real_,
        hessianDiagMax = NA_real_,
        hessianCondition = NA_real_,
        odalVariant = NA_character_,
        curvatureStatus = NA_character_,
        correctionEigenMin = NA_real_,
        correctionEigenMax = NA_real_,
        correctionEigenNegative = NA_real_,
        correctionDiagMin = NA_real_,
        correctionDiagMax = NA_real_,
        correctionDiagNegative = NA_real_,
        globalHessianEigenMin = NA_real_,
        leadHessianEigenMin = NA_real_,
        siteHessianEigenMin = NA_real_,
        siteHessianNegative = NA_real_,
        maxConvAlpha = NA_real_,
        maxConvAlphaStatus = NA_character_,
        optimConvergence = NA_integer_,
        adapFinalRho = NA_real_,
        adapFinalRhoOverGlobalEigenMax = NA_real_,
        adapFinalLeadWeight = NA_real_,
        adapFinalProxAnchorsIntercept = NA,
        pooledNegLogLik = pooledDiag$pooledNegLogLik %||% NA_real_,
        pooledMeanLogLoss = pooledDiag$pooledMeanLogLoss %||% NA_real_,
        pooledGradientMaxAbs = pooledDiag$pooledGradientMaxAbs %||% NA_real_,
        pooledKktMaxAbs = pooledDiag$pooledKktMaxAbs %||% NA_real_,
        pooledKktViolating = pooledDiag$pooledKktViolating %||% NA_real_,
        pooledKktMaxCoordinate = pooledDiag$pooledKktMaxCoordinate %||% NA_real_,
        pooledObjectiveGap = NA_real_,
        fitElapsedSeconds = fit$elapsedSeconds,
        elapsedSeconds = fit$elapsedSeconds,
        configLabel = fit$configLabel %||% config$configLabel %||% "default",
        stringsAsFactors = FALSE
      ),
      evalRows,
      communicationColumns(communication$fit, communication$tuning, communication$basis),
      data.frame(
        task = task,
        error = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  })
  baselineRows <- do.call(rbind, baselineRows)

  if (exists("localFailuresForRows", inherits = FALSE) && length(localFailuresForRows)) {
    failedRows <- do.call(rbind, lapply(localFailuresForRows, function(failure) {
      evalRows <- do.call(rbind, Map(function(clientData, clientId, clientIndex) {
        y <- clientData$yLabels
        data.frame(
          client = clientIndex,
          auc = NA_real_,
          logLoss = NA_real_,
          calibrationIci = NA_real_,
          calibrationInTheLarge = NA_real_,
          calibrationIntercept = NA_real_,
          calibrationSlope = NA_real_,
          density = NA_real_,
          n = length(y),
          outcomes = sum(y),
          clientId = clientId,
          stringsAsFactors = FALSE
        )
      }, clientData = testData, clientId = testClientIds, clientIndex = testClientIndexes))

      cbind(
        data.frame(
          method = method,
          featureSet = featureSet,
          fold = fold,
          p = ncol(testData[[1]]$xMatrix),
          selectedLambda = NA_real_,
          lambdaPathFile = NA_character_,
          leadIndex = failure$localIndex,
          leadWeight = NA_real_,
          leadWeightMin = NA_real_,
          trainObjective = NA_real_,
          hessianDim = NA_character_,
          hessianDiagMin = NA_real_,
          hessianDiagMax = NA_real_,
          hessianCondition = NA_real_,
          odalVariant = NA_character_,
          curvatureStatus = NA_character_,
          correctionEigenMin = NA_real_,
          correctionEigenMax = NA_real_,
          correctionEigenNegative = NA_real_,
          correctionDiagMin = NA_real_,
          correctionDiagMax = NA_real_,
          correctionDiagNegative = NA_real_,
          globalHessianEigenMin = NA_real_,
          leadHessianEigenMin = NA_real_,
          siteHessianEigenMin = NA_real_,
          siteHessianNegative = NA_real_,
          maxConvAlpha = NA_real_,
          maxConvAlphaStatus = NA_character_,
          optimConvergence = NA_integer_,
          adapFinalRho = NA_real_,
          adapFinalRhoOverGlobalEigenMax = NA_real_,
          adapFinalLeadWeight = NA_real_,
          adapFinalProxAnchorsIntercept = NA,
          pooledNegLogLik = NA_real_,
          pooledMeanLogLoss = NA_real_,
          pooledGradientMaxAbs = NA_real_,
          pooledKktMaxAbs = NA_real_,
          pooledKktViolating = NA_real_,
          pooledKktMaxCoordinate = NA_real_,
          pooledObjectiveGap = NA_real_,
          fitElapsedSeconds = failure$elapsedSeconds,
          elapsedSeconds = failure$elapsedSeconds,
          configLabel = paste0("source=", failure$trainClientId),
          stringsAsFactors = FALSE
        ),
        evalRows,
        communicationColumns(NULL, NULL, "none"),
        data.frame(
          task = task,
          error = paste0("local baseline fit failed for ", failure$trainClientId, ": ", failure$error),
          stringsAsFactors = FALSE
        )
      )
    }))
    baselineRows <- dplyr::bind_rows(baselineRows, failedRows)
  }

  if (!is.null(modelDirectory)) {
    modelRecord <- function(fit, sourceClientId = NA_character_, weight = NA_real_) {
      list(
        sourceClientId = sourceClientId,
        weight = weight,
        selectedLambda = fit$selectedLambda,
        fittingSettings = fit$fittingSettings,
        coefficients = coefficientTable(fit$w, trainMap, config$intercept, processed$preprocessor)
      )
    }
    components <- lapply(seq_along(localFits), function(j) {
      modelRecord(localFits[[j]], trainClientIds[[localIndexesOk[[j]]]],
        if (length(weights)) weights[[j]] else NA_real_)
    })
    models <- if (identical(method, "LocalEnsembleLasso")) components else lapply(fits, function(fit) {
      lead <- fit$leadIndex
      modelRecord(fit, if (length(lead) == 1L && !is.na(lead)) trainClientIds[[lead]] else NA_character_)
    })
    baselineRows <- saveModelArtifact(list(
      task = task, fold = fold, featureSet = featureSet, method = method,
      models = models,
      components = components,
      aggregation = switch(method, LocalAvgLasso = "coefficientAverage=sampleSize",
        SparseLocalAvgLasso = "softThresholdedCoefficientAverage=sampleSize",
        DebiasedLocalAvgLasso = "nodewiseDebiasedThresholdedAverage=sampleSize",
        LocalEnsembleLasso = paste0("predictionAverage=", argValue(args, "local-ensemble-weighting") %||% "sampleSize"),
        "singleModel"),
      localFailures = localFailures,
      ineligibleClientIds = if (length(localFits)) trainClientIds[!eligible] else character(),
      trainingSampleSizes = stats::setNames(vapply(trainData, `[[`, numeric(1), "n"), trainClientIds),
      preprocessing = processed$preprocessor,
      originalMapping = trainMap,
      config = config,
      args = args,
      populationSettings = popSettings,
      trainPaths = trainPaths, testPaths = testPaths,
      trainClientIds = trainClientIds, testClientIds = testClientIds,
      calibrationCurves = if (length(calibrationCurves)) do.call(rbind, calibrationCurves) else NULL,
      communication = .combineCommunication(list(.asTuningCommunication(
        if (is.data.frame(communication$tuning)) communication$tuning), communication$fit))
    ), baselineRows, modelDirectory)
  }
  baselineRows
}

summarizeResults <- function(rows) {
  if (nrow(rows) == 0L) {
    return(rows)
  }
  splitRows <- split(rows, list(rows$task, rows$featureSet, rows$method), drop = TRUE)
  summaries <- lapply(splitRows, function(x) {
    data.frame(
      task = x$task[[1]],
      featureSet = x$featureSet[[1]],
      method = x$method[[1]],
      folds = length(unique(x$fold)),
      meanAuc = mean(x$auc, na.rm = TRUE),
      sdAuc = stats::sd(x$auc, na.rm = TRUE),
      meanLogLoss = mean(x$logLoss, na.rm = TRUE),
      sdLogLoss = stats::sd(x$logLoss, na.rm = TRUE),
      meanCalIci = mean(x$calibrationIci, na.rm = TRUE),
      meanCalInTheLarge = mean(x$calibrationInTheLarge, na.rm = TRUE),
      meanCalIntercept = mean(x$calibrationIntercept, na.rm = TRUE),
      meanCalSlope = mean(x$calibrationSlope, na.rm = TRUE),
      meanElapsed = mean(x$elapsedSeconds, na.rm = TRUE),
      messages = maxOrNa(x$messages),
      numbers = maxOrNa(x$numbers),
      bytes = maxOrNa(x$bytes),
      tuningNumbers = maxOrNa(x$tuningNumbers),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summaries)
}

# Cyclops fitted to the pooled training rows at DualAvg's own penalty minimizes
# the same penalized mean log loss, so its solution shows how far a round-limited
# DualAvg fit stopped from the optimum.
dualAvgReferenceDiagnostics <- function(clTrain, w, lambda, intercept, args) {
  start <- Sys.time()
  trainData <- parallel::clusterEvalQ(clTrain,
    list(xMatrix = clientData$xMatrix, yLabels = clientData$yLabels))
  n <- sum(vapply(trainData, function(x) nrow(x$xMatrix), numeric(1)))
  referenceArgs <- args
  referenceArgs[["cyclops-tolerance"]] <- argValue(args, "dualavg-reference-tolerance") %||% "1e-8"
  reference <- fitCyclopsWeights(trainData, referenceArgs, seed = 42L, intercept = intercept,
    variance = 2 / (lambda * n)^2)$w
  penalized <- seq_along(w)
  if (isTRUE(intercept)) penalized <- penalized[-1L]
  objective <- function(beta) {
    pooledFitDiagnostics(trainData, beta, intercept = intercept)$pooledMeanLogLoss +
      lambda * sum(abs(beta[penalized]))
  }
  list(
    dualAvgReferenceObjectiveGap = objective(w) - objective(reference),
    dualAvgReferenceCoefficientMaxDiff = max(abs(w - reference)),
    dualAvgReferenceSupportMismatch = sum((abs(w[penalized]) > 1e-8) != (abs(reference[penalized]) > 1e-8)),
    dualAvgReferenceKktMaxAbs = pooledFitDiagnostics(trainData, reference, lambda = lambda,
      intercept = intercept)$pooledKktMaxAbs,
    dualAvgReferenceElapsedSeconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}

addPooledObjectiveGap <- function(rows) {
  if (!nonEmptyRows(rows) || !"pooledMeanLogLoss" %in% names(rows)) {
    return(rows)
  }
  rows$pooledObjectiveGap <- NA_real_
  pooled <- rows[rows$method == "PooledLasso" & is.finite(rows$pooledMeanLogLoss), , drop = FALSE]
  if (!nrow(pooled)) {
    return(rows)
  }
  keys <- paste(pooled$task, pooled$fold, pooled$featureSet, sep = "\r")
  reference <- stats::setNames(pooled$pooledMeanLogLoss, keys)
  rowKeys <- paste(rows$task, rows$fold, rows$featureSet, sep = "\r")
  rows$pooledObjectiveGap <- rows$pooledMeanLogLoss - as.numeric(reference[rowKeys])
  rows
}

# Result columns for the final fit and penalty tuning; `basis` says how counts
# were obtained. NA marks communication that is not counted.
communicationColumns <- function(fit, tuning, basis) {
  missing <- list(exchanges = NA_real_, messages = NA_real_, values = NA_real_, bytes = NA_real_)
  final <- if (identical(fit, NA)) missing else .communicationTotals(fit)
  tuned <- if (identical(tuning, NA)) missing else .communicationTotals(tuning)
  data.frame(communicationBasis = basis, exchanges = final$exchanges,
    messages = final$messages, numbers = final$values, bytes = final$bytes,
    tuningExchanges = tuned$exchanges, tuningMessages = tuned$messages,
    tuningNumbers = tuned$values, tuningBytes = tuned$bytes, stringsAsFactors = FALSE)
}

adapFinalDiagnostic <- function(fit, name, default = NA_real_) {
  diagnostics <- fit$adapFinalDiagnostics
  if (is.null(diagnostics) || is.null(diagnostics[[name]])) {
    return(default)
  }
  value <- diagnostics[[name]]
  if (length(value) == 0L) {
    return(default)
  }
  value[[1]]
}

fitFederatedFold <- function(method, clTrain, clTest, config, resultDirectory,
                             task, featureSet, fold, testClientIds, verbose,
                             debugDirectory = NULL, modelDirectory = NULL,
                             provenance = list()) {
  if (isTRUE(config$adapTraceDiagnostics) && !is.null(debugDirectory) &&
      method %in% c("ADAP", "ADAP2", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")) {
    config$adapTraceFile <- debugPath(
      debugDirectory,
      task,
      fold,
      featureSet,
      method,
      suffix = "adap-trace",
      extension = "rds"
    )
    config$adapTraceContext <- list(
      task = task,
      fold = fold,
      featureSet = featureSet,
      method = method,
      trainClientPaths = config$trainClientPaths %||% NULL,
      testClientIds = testClientIds
    )
  }
  start <- Sys.time()
  fit <- FederatedLearning::fitFederated(
    cl = clTrain,
    algorithm = method,
    config = config,
    verbose = verbose
  )
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  .requireDualAvgConvergence(fit, config, "final fit", config$lambdaSearchTrace)
  reference <- if (method %in% dualAvgMethods && isTRUE(config$dualavgReferenceFit)) {
    dualAvgReferenceDiagnostics(clTrain, fit$w, config[["lambda", exact = TRUE]],
      isTRUE(config$intercept), provenance$args %||% list())
  }

  testConfig <- config
  testConfig$mapping <- fit$config$mapping
  testConfig$p <- nrow(fit$config$mapping)
  FederatedLearning::clusterCreateMatrices(clTest, testConfig)
  evalRows <- FederatedLearning::clusterEvaluateModel(clTest, fit$w)
  calibrationCurves <- attr(evalRows, "calibrationCurve")
  if (!is.null(calibrationCurves)) {
    calibrationCurves <- cbind(clientId = testClientIds[calibrationCurves$client], calibrationCurves)
  }
  evalRows$clientId <- testClientIds[evalRows$client]

  if (!is.null(debugDirectory)) {
    writeDebugObject(
      list(
        task = task,
        fold = fold,
        featureSet = featureSet,
        method = method,
        config = fit$config,
        roundsCompleted = fit$roundsCompleted %||% NA_integer_,
        dualAvgConverged = fit$converged %||% NA,
        dualAvgStopReason = fit$stopReason %||% NA_character_,
        dualAvgKktMaxAbs = fit$kktMaxAbs %||% NA_real_,
        dualAvgKktChecks = fit$kktChecks %||% NA_integer_,
        dualAvgKktHistory = fit$kktHistory,
        selectedLambda = fit$selectedLambda %||% config[["lambda", exact = TRUE]] %||% NA_real_,
        leadIndex = fit$leadIndex %||% NA_integer_,
        leadWeight = fit$leadWeight %||% NA_real_,
        leadWeightMin = fit$leadWeightMin %||% NA_real_,
        betaBarMaxAbs = fit$betaBarMaxAbs %||% NA_real_,
        betaBarL2 = fit$betaBarL2 %||% NA_real_,
        odalInitDiagnostics = fit$odalInitDiagnostics %||% NULL,
        adapInitDiagnostics = fit$adapInitDiagnostics %||% NULL,
        hessianDim = fit$hessianDim %||% NA_character_,
        hessianDiagMin = fit$hessianDiagMin %||% NA_real_,
        hessianDiagMax = fit$hessianDiagMax %||% NA_real_,
        hessianCondition = fit$hessianCondition %||% NA_real_,
        odalVariant = fit$odalVariant %||% NA_character_,
        curvatureStatus = fit$curvatureStatus %||% NA_character_,
        correctionEigenMin = fit$correctionEigenMin %||% NA_real_,
        correctionEigenMax = fit$correctionEigenMax %||% NA_real_,
        correctionEigenNegative = fit$correctionEigenNegative %||% NA_real_,
        correctionDiagMin = fit$correctionDiagMin %||% NA_real_,
        correctionDiagMax = fit$correctionDiagMax %||% NA_real_,
        correctionDiagNegative = fit$correctionDiagNegative %||% NA_real_,
        globalHessianEigenMin = fit$globalHessianEigenMin %||% NA_real_,
        leadHessianEigenMin = fit$leadHessianEigenMin %||% NA_real_,
        siteHessianEigenMin = fit$siteHessianEigenMin %||% NA_real_,
        siteHessianNegative = fit$siteHessianNegative %||% NA_real_,
        maxConvAlpha = fit$maxConvAlpha %||% NA_real_,
        maxConvAlphaStatus = fit$maxConvAlphaStatus %||% NA_character_,
        optimConvergence = fit$optimConvergence %||% NA_integer_,
        odalGradientMaxAbs = fit$odalGradientMaxAbs %||% NA_real_,
        odalCompleteHessianEigenMin = fit$odalCompleteHessianEigenMin %||% NA_real_,
        odalBetaMaxAbs = fit$odalBetaMaxAbs %||% NA_real_,
        odalEtaMaxAbs = fit$odalEtaMaxAbs %||% NA_real_,
        pooledNegLogLik = fit$pooledNegLogLik %||% NA_real_,
        pooledMeanLogLoss = fit$pooledMeanLogLoss %||% NA_real_,
        pooledGradientMaxAbs = fit$pooledGradientMaxAbs %||% NA_real_,
        pooledKktMaxAbs = fit$pooledKktMaxAbs %||% NA_real_,
        pooledKktViolating = fit$pooledKktViolating %||% NA_real_,
        pooledKktMaxCoordinate = fit$pooledKktMaxCoordinate %||% NA_real_,
        lambdaSeq = fit$lambdaSeq %||% NULL,
        cvScores = fit$cvScores %||% NULL,
        cvValid = fit$cvValid %||% NULL,
        lambdaSelectionMetric = fit$lambdaSelectionMetric %||% NA_character_,
        adapCvDiagnostics = fit$adapCvDiagnostics %||% NULL,
        adapTrace = fit$adapTrace %||% NULL,
        adapFinalTrace = fit$adapFinalTrace %||% NULL,
        adapFinalDiagnostics = fit$adapFinalDiagnostics %||% NULL,
        coefficients = fit$w,
        coefficientSummary = c(
          length = length(fit$w),
          nonFinite = sum(!is.finite(fit$w)),
          min = suppressWarnings(min(fit$w, na.rm = TRUE)),
          max = suppressWarnings(max(fit$w, na.rm = TRUE)),
          l1 = sum(abs(fit$w), na.rm = TRUE),
          nonzero = sum(abs(fit$w) > 1e-8, na.rm = TRUE)
        ),
        evaluation = evalRows
      ),
      debugDirectory = debugDirectory,
      task = task,
      fold = fold,
      featureSet = featureSet,
      method = method,
      suffix = "fit"
    )
    if (!is.null(fit$adapCvDiagnostics)) {
      writeDebugCsv(
        fit$adapCvDiagnostics,
        debugDirectory = debugDirectory,
        task = task,
        fold = fold,
        featureSet = featureSet,
        method = method,
        suffix = "adap-cv"
      )
    }
    if (isTRUE(config$adapTraceDiagnostics) &&
        (!is.null(fit$adapTrace) || !is.null(fit$adapFinalTrace))) {
      writeDebugObject(
        list(
          task = task,
          fold = fold,
          featureSet = featureSet,
          method = method,
          cvTrace = fit$adapTrace %||% NULL,
          finalTrace = fit$adapFinalTrace %||% NULL
        ),
        debugDirectory = debugDirectory,
        task = task,
        fold = fold,
        featureSet = featureSet,
        method = method,
        suffix = "adap-trace"
      )
    }
  }

  lambdaPathFile <- NA_character_
  if (!is.null(fit$lambdaSeq) && length(fit$lambdaSeq) > 0L) {
    lambdaPathFile <- file.path(
      resultDirectory,
      sprintf("lambda_%s_%s_%s_fold%s.csv", safeFilePart(task), safeFilePart(method),
        safeFilePart(featureSet), fold)
    )
    lambdaDf <- data.frame(
      lambda = fit$lambdaSeq,
      cvScore = fit$cvScores %||% NA_real_,
      cvValid = fit$cvValid %||% NA,
      cvMetric = fit$lambdaSelectionMetric %||% NA_character_
    )
    utils::write.csv(lambdaDf, lambdaPathFile, row.names = FALSE)
  }
  resultRows <- cbind(
    data.frame(
      dualAvgConverged = fit$converged %||% NA,
      dualAvgStopReason = fit$stopReason %||% NA_character_,
      dualAvgKktMaxAbs = fit$kktMaxAbs %||% NA_real_,
      dualAvgKktChecks = fit$kktChecks %||% NA_integer_,
      method = method,
      featureSet = featureSet,
      fold = fold,
      p = length(fit$w),
      roundsCompleted = fit$roundsCompleted %||% NA_integer_,
      siteElapsedSeconds = sum(fit$siteSeconds %||% NA_real_),
      serverElapsedSeconds = sum(fit$serverSeconds %||% NA_real_),
      roundElapsedSeconds = roundSecondsLabel(fit$siteSeconds, fit$serverSeconds),
      nonzeroPredictors = exactNonzero(fit$w, config$intercept),
      penaltyScale = if (method %in% c("ODAL", "ODAL1", "ODAL2")) "none (unpenalized surrogate)" else "mean negative log-likelihood L1 multiplier",
      selectedLambda = fit$selectedLambda %||% config[["lambda", exact = TRUE]] %||% NA_real_,
      lambdaPathFile = lambdaPathFile,
      leadIndex = fit$leadIndex %||% NA_integer_,
      leadWeight = fit$leadWeight %||% NA_real_,
      leadWeightMin = fit$leadWeightMin %||% NA_real_,
      trainObjective = fit$globalObjective %||% NA_real_,
      hessianDim = fit$hessianDim %||% NA_character_,
      hessianDiagMin = fit$hessianDiagMin %||% NA_real_,
      hessianDiagMax = fit$hessianDiagMax %||% NA_real_,
      hessianCondition = fit$hessianCondition %||% NA_real_,
      odalVariant = fit$odalVariant %||% NA_character_,
      curvatureStatus = fit$curvatureStatus %||% NA_character_,
      correctionEigenMin = fit$correctionEigenMin %||% NA_real_,
      correctionEigenMax = fit$correctionEigenMax %||% NA_real_,
      correctionEigenNegative = fit$correctionEigenNegative %||% NA_real_,
      correctionDiagMin = fit$correctionDiagMin %||% NA_real_,
      correctionDiagMax = fit$correctionDiagMax %||% NA_real_,
      correctionDiagNegative = fit$correctionDiagNegative %||% NA_real_,
      globalHessianEigenMin = fit$globalHessianEigenMin %||% NA_real_,
      leadHessianEigenMin = fit$leadHessianEigenMin %||% NA_real_,
      siteHessianEigenMin = fit$siteHessianEigenMin %||% NA_real_,
      siteHessianNegative = fit$siteHessianNegative %||% NA_real_,
      maxConvAlpha = fit$maxConvAlpha %||% NA_real_,
      maxConvAlphaStatus = fit$maxConvAlphaStatus %||% NA_character_,
      optimConvergence = fit$optimConvergence %||% NA_integer_,
      odalGradientMaxAbs = fit$odalGradientMaxAbs %||% NA_real_,
      odalCompleteHessianEigenMin = fit$odalCompleteHessianEigenMin %||% NA_real_,
      odalBetaMaxAbs = fit$odalBetaMaxAbs %||% NA_real_,
      odalEtaMaxAbs = fit$odalEtaMaxAbs %||% NA_real_,
      adapCvMaxOuter = config$maxOuter %||% NA_integer_,
      adapFinalMaxOuter = config$adapFinalMaxOuter %||% NA_integer_,
      adapFinalOuterIterations = adapFinalDiagnostic(fit, "outerIterations", NA_real_),
      adapFinalConverged = adapFinalDiagnostic(fit, "converged", NA),
      adapFinalKktMaxAbs = adapFinalDiagnostic(fit, "kktMaxAbs", NA_real_),
      adapFinalKktViolating = adapFinalDiagnostic(fit, "kktViolating", NA_real_),
      adapFinalBetaMaxAbs = adapFinalDiagnostic(fit, "betaMaxAbs", NA_real_),
      adapFinalEtaMaxAbs = adapFinalDiagnostic(fit, "etaMaxAbs", NA_real_),
      adapFinalCompleteHessianEigenMin = adapFinalDiagnostic(fit, "completeHessianEigenMin", NA_real_),
      adapFinalRho = adapFinalDiagnostic(fit, "rho", NA_real_),
      adapFinalRhoOverGlobalEigenMax = adapFinalDiagnostic(fit, "rhoOverGlobalEigenMax", NA_real_),
      adapFinalLeadWeight = adapFinalDiagnostic(fit, "leadWeight", NA_real_),
      adapFinalProxAnchorsIntercept = adapFinalDiagnostic(fit, "proxAnchorsIntercept", NA),
      adapFinalFailureReason = adapFinalDiagnostic(fit, "failureReason", NA_character_),
      pooledNegLogLik = fit$pooledNegLogLik %||% NA_real_,
      pooledMeanLogLoss = fit$pooledMeanLogLoss %||% NA_real_,
      pooledGradientMaxAbs = fit$pooledGradientMaxAbs %||% NA_real_,
      pooledKktMaxAbs = fit$pooledKktMaxAbs %||% NA_real_,
      pooledKktViolating = fit$pooledKktViolating %||% NA_real_,
      pooledKktMaxCoordinate = fit$pooledKktMaxCoordinate %||% NA_real_,
      pooledObjectiveGap = NA_real_,
      fitElapsedSeconds = elapsed,
      elapsedSeconds = elapsed,
      configLabel = config$configLabel %||% "default",
      stringsAsFactors = FALSE
    ),
    evalRows,
    communicationColumns(fit$communication, config$tuningCommunication,
      fit$communicationBasis %||% NA_character_),
    data.frame(
      task = task,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  for (name in names(reference)) {
    resultRows[[name]] <- reference[[name]]
  }
  filtering <- fit$config$featureFiltering
  if (!is.null(filtering)) {
    resultRows$preprocessFingerprint <- filtering$fingerprint
    preprocessing <- .communicationTotals(filtering$communication)
    resultRows$preprocessMessages <- preprocessing$messages
    resultRows$preprocessNumbers <- preprocessing$values
    resultRows$preprocessBytes <- preprocessing$bytes
    resultRows$preprocessElapsedSeconds <- filtering$elapsedSeconds
  }
  artifact <- list(
    task = task, fold = fold, featureSet = featureSet, method = method,
    models = list(list(
      sourceClientId = NA_character_,
      coefficients = coefficientTable(fit$w, fit$config$mapping, fit$config$intercept),
      selectedLambda = fit$selectedLambda %||% fit$config[["lambda", exact = TRUE]]
    )),
    config = fit$config,
    featureFiltering = filtering,
    provenance = provenance,
    testClientIds = testClientIds,
    roundsCompleted = fit$roundsCompleted,
    dualAvgConverged = fit$converged %||% NA,
    dualAvgStopReason = fit$stopReason %||% NA_character_,
    dualAvgKktMaxAbs = fit$kktMaxAbs %||% NA_real_,
    dualAvgKktChecks = fit$kktChecks %||% NA_integer_,
    dualAvgKktHistory = fit$kktHistory,
    lambdaSeq = fit$lambdaSeq,
    cvScores = fit$cvScores,
    cvValid = fit$cvValid,
    lambdaSelectionMetric = fit$lambdaSelectionMetric,
    adapCvDiagnostics = fit$adapCvDiagnostics,
    adapFinalDiagnostics = fit$adapFinalDiagnostics,
    calibrationCurves = calibrationCurves,
    communication = .combineCommunication(list(filtering$communication,
      config$tuningCommunication, fit$communication))
  )
  if (!is.null(fit$adapInitDiagnostics)) artifact$adapInitDiagnostics <- fit$adapInitDiagnostics
  saveModelArtifact(artifact, resultRows, modelDirectory)
}

safeStopCluster <- function(cl) {
  if (is.null(cl)) {
    return(invisible(NULL))
  }
  try(stopCluster(cl), silent = TRUE)
  invisible(NULL)
}

# Population settings saved with a site's data must equal the task's settings,
# so a run cannot silently use a different horizon from the extraction.
checkSavedPopulationSettings <- function(clientPaths, populationSettings) {
  for (path in clientPaths) {
    saved <- file.path(path, "populationSettings.rds")
    if (file.exists(saved) &&
        !isTRUE(all.equal(readRDS(saved), populationSettings, tolerance = 0))) {
      stop("Population settings saved in ", path, " differ from the task's population settings")
    }
  }
  invisible(TRUE)
}

# Runs the comparison described by command-line style arguments. The public
# runComparison() supplies explicit site paths and PLP population settings;
# command-line runs build both from --data-root, --client-ids and flags.
runComparisonFromArgs <- function(args, populationSettings = NULL, clientPaths = NULL) {
  if (!requireNamespace("FederatedLearning", quietly = TRUE)) {
    stop("FederatedLearning must be installed or loaded before running this script")
  }
  if (!requireNamespace("PatientLevelPrediction", quietly = TRUE)) {
    stop("PatientLevelPrediction is required to load PLP data")
  }

  runStart <- Sys.time()
  dataRoot <- args[["data-root"]] %||% "data"
  resultDirectory <- args[["result-directory"]] %||% "results/comparisonMatrix"
  dir.create(resultDirectory, recursive = TRUE, showWarnings = FALSE)
  preprocessMode <- federatedPreprocessMode(args)
  auditOnly <- logicalArg(argValue(args, "preprocessing-audit-only"), FALSE)
  referenceDirectory <- argValue(args, "preprocessing-reference-directory")
  if ((auditOnly || !is.null(referenceDirectory)) && preprocessMode != "baseline") {
    stop("Preprocessing audit/reference requires --federated-preprocess=baseline")
  }
  guardPreprocessingSettings(args, resultDirectory)
  resultFile <- file.path(resultDirectory, "comparison_results.csv")
  summaryFile <- file.path(resultDirectory, "summary_by_method.csv")
  diagnosticsFile <- file.path(resultDirectory, "diagnostics.csv")

  tasks <- csvArg(args[["tasks"]], c("dementia", "readmission", "lungCancer"))
  featureSets <- csvArg(args[["feature-sets"]], c("ageSex", "ageSexPhenotypes"))
  methods <- csvArg(args[["methods"]], c(
    "DualAvg", "ODAL", "ODAL1", "ADAP2", "Prox-ADAP", "C-ADAP",
    "MaxConv-ADAP", "ADAP1", "ADAPDiag"
  ))
  clientIds <- if (is.null(clientPaths)) csvArg(args[["client-ids"]], character()) else names(clientPaths)
  nClients <- intArg(args[["clients"]], if (length(clientIds) > 0L) length(clientIds) else 5L)
  if (length(clientIds) == 0L) {
    clientIds <- paste0("client", seq_len(nClients))
  }
  stopifnot(length(clientIds) == nClients)
  folds <- foldArg(args[["folds"]], nClients)
  hosts <- csvArg(args[["hosts"]], rep("localhost", nClients))
  if (length(hosts) == 1L) {
    hosts <- rep(hosts, nClients)
  }
  stopifnot(length(hosts) == nClients)
  args <- validateSurrogateCaptureArgs(args, resultDirectory, methods, hosts)
  mirai <- logicalArg(args[["mirai"]], FALSE)
  verbose <- logicalArg(args[["verbose"]], TRUE)
  resume <- logicalArg(args[["resume"]], TRUE)
  rerunErrors <- logicalArg(args[["rerun-errors"]], FALSE)
  rerunMissingModels <- logicalArg(args[["rerun-missing-models"]], FALSE)
  saveModels <- logicalArg(args[["save-models"]], TRUE)
  if (rerunMissingModels && !saveModels) {
    stop("--rerun-missing-models=true requires --save-models=true")
  }
  modelDirectory <- if (saveModels) file.path(resultDirectory, "models") else NULL
  debugDirectory <- if (logicalArg(args[["debug-diagnostics"]], FALSE)) {
    file.path(resultDirectory, "debug")
  } else {
    NULL
  }
  if (!is.null(debugDirectory)) {
    dir.create(debugDirectory, recursive = TRUE, showWarnings = FALSE)
    message("Writing debug diagnostics to ", debugDirectory)
  }

  rows <- if (isTRUE(resume)) readCsvIfExists(resultFile) else NULL
  diagnostics <- if (isTRUE(resume)) readCsvIfExists(diagnosticsFile) else NULL
  if (nonEmptyRows(rows)) {
    message(sprintf(
      "Loaded %s existing result rows from %s",
      nrow(rows),
      resultFile
    ))
  }
  if (nonEmptyRows(diagnostics)) {
    message(sprintf(
      "Loaded %s existing diagnostic rows from %s",
      nrow(diagnostics),
      diagnosticsFile
    ))
  }

  if (!is.null(clientPaths) && length(tasks) != 1L) {
    stop("Explicit site paths require exactly one task")
  }
  sitePaths <- clientPaths
  for (task in tasks) {
    clientPaths <- unname(sitePaths) %||% file.path(dataRoot, task, clientIds)
    if (!all(dir.exists(clientPaths))) {
      stop("Missing client paths for task ", task, ": ",
           paste(clientPaths[!dir.exists(clientPaths)], collapse = ", "))
    }
    if (!is.null(populationSettings)) checkSavedPopulationSettings(clientPaths, populationSettings)
    popSettings <- populationSettings %||% PatientLevelPrediction::createStudyPopulationSettings(
      requireTimeAtRisk = logicalArg(args[["require-time-at-risk"]], FALSE),
      minTimeAtRisk = intArg(args[["min-time-at-risk"]], 1L),
      riskWindowStart = intArg(args[[paste0(task, "-risk-window-start")]],
                               intArg(args[["risk-window-start"]], 1L)),
      riskWindowEnd = intArg(args[[paste0(task, "-risk-window-end")]], taskRiskWindow(task)),
      removeSubjectsWithPriorOutcome = logicalArg(args[["remove-prior-outcomes"]], TRUE),
      priorOutcomeLookback = intArg(args[["prior-outcome-lookback"]], 99999L)
    )

    for (fold in folds) {
      pending <- expand.grid(
        featureSet = featureSets,
        method = methods,
        stringsAsFactors = FALSE
      )
      pending$done <- mapply(
        function(featureSet, method) {
          isCompletedCombination(
            rows = rows,
            task = task,
            fold = fold,
            featureSet = featureSet,
            method = method,
            rerunErrors = rerunErrors,
            rerunMissingModels = rerunMissingModels,
            resultDirectory = resultDirectory
          )
        },
        pending$featureSet,
        pending$method
      )
      diagnosticsDone <- vapply(
        featureSets,
        function(featureSet) isCompletedDiagnostic(diagnostics, task, fold, featureSet),
        logical(1)
      )
      if (!auditOnly && all(pending$done) && all(diagnosticsDone)) {
        message(sprintf(
          "[%s] skip task=%s fold=%s: all requested methods and diagnostics are already complete",
          format(Sys.time(), "%H:%M:%S"), task, fold
        ))
        next
      }

      trainIds <- setdiff(seq_len(nClients), fold)
      testIds <- fold
      trainPaths <- clientPaths[trainIds]
      testPaths <- clientPaths[testIds]
      trainHosts <- hosts[trainIds]
      testHosts <- hosts[testIds]

      clTrain <- NULL
      clTest <- NULL
      tryCatch(
        {
          clTrain <- FederatedLearning::clusterInit(trainHosts, trainPaths, mirai = mirai)
          trainPopSizes <- FederatedLearning::clusterLoadData(clTrain, trainPaths, popSettings)

          if (!auditOnly) {
            clTest <- FederatedLearning::clusterInit(testHosts, testPaths, mirai = mirai)
            FederatedLearning::clusterLoadData(clTest, testPaths, popSettings)
          }

          for (featureSet in featureSets) {
            filtering <- NULL
            if (preprocessMode == "baseline") {
              filterConfig <- methodConfig("PooledLasso", featureSet, args)
              for (method in methods) for (cfg in methodConfigGrid(method, featureSet, args)) {
                if (!identical(preprocessingMatrixSettings(cfg), preprocessingMatrixSettings(filterConfig))) {
                  stop("Matched preprocessing requires the same matrix settings for all methods/configs")
                }
              }
              filtering <- fitFederatedPreprocessor(clTrain, filterConfig, args)
              referenceError <- NULL
              filtering$referenceStatus <- tryCatch(checkPreprocessingReference(filtering,
                referenceDirectory, task, fold, featureSet, clientIds[trainIds],
                clientIds[testIds], popSettings, filterConfig), error = function(e) {
                  referenceError <<- conditionMessage(e)
                  "mismatch"
                })
              filtering <- savePreprocessingAudit(filtering,
                file.path(resultDirectory, "preprocessing"), task, fold, featureSet,
                clientIds[trainIds], clientIds[testIds], popSettings)
              if (!is.null(referenceError)) stop(referenceError)
              if (auditOnly) next
              if (!nrow(filtering$mapping)) stop("Matched preprocessing removed all non-intercept covariates")
            }
            featureDebugConfig <- NULL
            if (!is.null(debugDirectory)) {
              featureDebugConfig <- methodConfig(methods[[1]], featureSet, args)
              featureDebugConfig$mapping <- FederatedLearning::clusterCollectCovRefs(
                clTrain,
                type = featureDebugConfig$mapType,
                featureSet = featureDebugConfig$featureSet,
                covariateIds = featureDebugConfig$covariateIds,
                analysisIds = featureDebugConfig$analysisIds
              )
              featureDebugConfig$p <- nrow(featureDebugConfig$mapping)
              if (!is.null(filtering)) {
                featureDebugConfig$mapping <- filtering$mapping
                featureDebugConfig$p <- nrow(filtering$mapping)
              }
              collectDebugDiagnostics(
                cl = clTrain,
                config = featureDebugConfig,
                debugDirectory = debugDirectory,
                task = task,
                fold = fold,
                featureSet = featureSet,
                role = "train",
                clientIds = clientIds[trainIds],
                clientIndexes = trainIds,
                verbose = verbose
              )
              collectDebugDiagnostics(
                cl = clTest,
                config = featureDebugConfig,
                debugDirectory = debugDirectory,
                task = task,
                fold = fold,
                featureSet = featureSet,
                role = "test",
                clientIds = clientIds[testIds],
                clientIndexes = testIds,
                verbose = verbose
              )
            }

            for (method in methods) {
              if (isCompletedCombination(rows, task, fold, featureSet, method,
                  rerunErrors = rerunErrors, rerunMissingModels = rerunMissingModels,
                  resultDirectory = resultDirectory)) {
                guardAdapInitializerResume(rows, task, fold, featureSet, method, args, resultDirectory)
                message(sprintf(
                  "[%s] skip task=%s fold=%s featureSet=%s method=%s: existing successful result",
                  format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method
                ))
                next
              }
              message(sprintf(
                "[%s] task=%s fold=%s featureSet=%s method=%s",
                format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method
              ))
              methodStart <- Sys.time()
              configs <- methodConfigGrid(method, featureSet, args)
              configs <- lapply(configs, function(config) {
                config$trainClientPaths <- trainPaths
                if (!is.null(config$surrogateCaptureDirectory)) {
                  config$surrogateCaptureContext <- list(task = task, fold = fold, featureSet = featureSet,
                    method = method, populationSettings = popSettings,
                    trainClientIds = clientIds[trainIds], testClientIds = clientIds[testIds])
                }
                if (!is.null(filtering)) {
                  config$mapping <- filtering$mapping
                  config$p <- nrow(filtering$mapping)
                  config$featureFiltering <- filtering
                }
                if (!is.null(debugDirectory) && method %in% c("ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")) {
                  config$adapCvDiagnostics <- TRUE
                }
                config
              })
              selectedConfig <- NULL
              res <- tryCatch(
                if (method %in% baselineMethods) {
                  config <- configs[[1]]
                  selectedConfig <- config
                  fitBaselineFold(
                    method = method,
                    trainPaths = trainPaths,
                    testPaths = testPaths,
                    popSettings = popSettings,
                    config = config,
                    args = args,
                    task = task,
                    featureSet = featureSet,
                    fold = fold,
                    trainClientIds = clientIds[trainIds],
                    testClientIds = clientIds[testIds],
                    testClientIndexes = testIds,
                    modelDirectory = modelDirectory
                  )
                } else {
                  config <- selectMethodConfigForFold(
                    method = method,
                    clTrain = clTrain,
                    configs = configs,
                    trainPopSizes = trainPopSizes,
                    args = args,
                    verbose = verbose
                  )
                  selectedConfig <- config
                  fitFederatedFold(
                    method = method,
                    clTrain = clTrain,
                    clTest = clTest,
                    config = config,
                    resultDirectory = resultDirectory,
                    task = task,
                    featureSet = featureSet,
                    fold = fold,
                    testClientIds = clientIds[testIds],
                    verbose = verbose,
                    debugDirectory = debugDirectory,
                    modelDirectory = modelDirectory,
                    provenance = list(
                      args = args, populationSettings = popSettings,
                      trainClientIds = clientIds[trainIds], testClientIds = clientIds[testIds],
                      trainPaths = trainPaths, testPaths = testPaths
                    )
                  )
                },
                error = function(e) {
                  convergence <- e$dualAvgConvergence
                  message(sprintf(
                    "[%s] ERROR task=%s fold=%s featureSet=%s method=%s: %s",
                    format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method,
                    conditionMessage(e)
                  ))
                  writeDebugObject(
                    list(
                      task = task,
                      fold = fold,
                      featureSet = featureSet,
                      method = method,
                      error = conditionMessage(e),
                      class = class(e),
                      call = if (!is.null(conditionCall(e))) deparse(conditionCall(e)) else NULL,
                      calls = vapply(sys.calls(), function(x) paste(deparse(x), collapse = "\n"), character(1)),
                      configs = configs,
                      selectedConfig = selectedConfig,
                      dualAvgConvergence = convergence,
                      trainClientIds = clientIds[trainIds],
                      testClientIds = clientIds[testIds]
                    ),
                    debugDirectory = debugDirectory,
                    task = task,
                    fold = fold,
                    featureSet = featureSet,
                    method = method,
                    suffix = "error"
                  )
                  data.frame(
                    method = method,
                    featureSet = featureSet,
                    fold = fold,
                    dualAvgConverged = convergence$converged %||% NA,
                    dualAvgStopReason = convergence$stopReason %||% NA_character_,
                    dualAvgKktMaxAbs = convergence$kktMaxAbs %||% NA_real_,
                    dualAvgKktChecks = convergence$kktChecks %||% NA_integer_,
                    dualAvgFailedFitRounds = convergence$roundsCompleted %||% NA_integer_,
                    dualAvgConvergenceStage = convergence$stage %||% NA_character_,
                    p = NA_integer_,
                    selectedLambda = NA_real_,
                    lambdaPathFile = NA_character_,
                    leadIndex = NA_integer_,
                    leadWeight = NA_real_,
                    leadWeightMin = NA_real_,
                    trainObjective = NA_real_,
                    hessianDim = NA_character_,
                    hessianDiagMin = NA_real_,
                    hessianDiagMax = NA_real_,
                    hessianCondition = NA_real_,
                    odalVariant = NA_character_,
                    curvatureStatus = NA_character_,
                    correctionEigenMin = NA_real_,
                    correctionEigenMax = NA_real_,
                    correctionEigenNegative = NA_real_,
                    correctionDiagMin = NA_real_,
                    correctionDiagMax = NA_real_,
                    correctionDiagNegative = NA_real_,
                    globalHessianEigenMin = NA_real_,
                    leadHessianEigenMin = NA_real_,
                    siteHessianEigenMin = NA_real_,
                    siteHessianNegative = NA_real_,
                    maxConvAlpha = NA_real_,
                    maxConvAlphaStatus = NA_character_,
                    optimConvergence = NA_integer_,
                    adapFinalRho = NA_real_,
                    adapFinalRhoOverGlobalEigenMax = NA_real_,
                    adapFinalLeadWeight = NA_real_,
                    adapFinalProxAnchorsIntercept = NA,
                    pooledNegLogLik = NA_real_,
                    pooledMeanLogLoss = NA_real_,
                    pooledGradientMaxAbs = NA_real_,
                    pooledKktMaxAbs = NA_real_,
                    pooledKktViolating = NA_real_,
                    pooledKktMaxCoordinate = NA_real_,
                    pooledObjectiveGap = NA_real_,
                    fitElapsedSeconds = NA_real_,
                    elapsedSeconds = NA_real_,
                    configLabel = NA_character_,
                    client = testIds,
                    clientId = clientIds[testIds],
                    auc = NA_real_,
                    logLoss = NA_real_,
                    calibrationIci = NA_real_,
                    calibrationInTheLarge = NA_real_,
                    calibrationIntercept = NA_real_,
                    calibrationSlope = NA_real_,
                    density = NA_real_,
                    n = NA_integer_,
                    outcomes = NA_integer_,
                    messages = NA_real_,
                    numbers = NA_real_,
                    task = task,
                    error = conditionMessage(e),
                    stringsAsFactors = FALSE
                  )
                }
              )
              res <- stampMethodElapsed(res, methodStart)
              res <- stampPeakMemory(res, list(clTrain, clTest))
              message(sprintf(
                "[%s] done task=%s fold=%s featureSet=%s method=%s in %s; peak memory main %.1f GB, largest worker %.1f GB",
                format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method,
                formatDuration(res$elapsedSeconds[[1]]), res$peakMemoryMbMain[[1]] / 1024,
                res$peakMemoryMbWorkers[[1]] / 1024
              ))
              rows <- appendCombinationRows(rows, res, task, fold, featureSet, method)
              utils::write.csv(
                rows,
                resultFile,
                row.names = FALSE
              )
            }

            if (isCompletedDiagnostic(diagnostics, task, fold, featureSet)) {
              message(sprintf(
                "[%s] skip diagnostics task=%s fold=%s featureSet=%s: existing diagnostics",
                format(Sys.time(), "%H:%M:%S"), task, fold, featureSet
              ))
              next
            }
            diagConfig <- methodConfig(methods[[1]], featureSet, args)
            diagConfig$mapping <- FederatedLearning::clusterCollectCovRefs(
              clTrain,
              type = diagConfig$mapType,
              featureSet = diagConfig$featureSet,
              covariateIds = diagConfig$covariateIds,
              analysisIds = diagConfig$analysisIds
            )
            diagConfig$p <- nrow(diagConfig$mapping)
            if (!is.null(filtering)) {
              diagConfig$mapping <- filtering$mapping
              diagConfig$p <- nrow(filtering$mapping)
            }
            FederatedLearning::clusterCreateMatrices(clTrain, diagConfig)
            diagRows <- FederatedLearning::clusterDiagnostics(clTrain, diagConfig)
            diagRows$task <- task
            diagRows$fold <- fold
            diagRows$featureSet <- featureSet
            diagnostics <- if (nonEmptyRows(diagnostics)) dplyr::bind_rows(diagnostics, diagRows) else diagRows
            utils::write.csv(
              diagnostics,
              diagnosticsFile,
              row.names = FALSE
            )
          }
        },
        finally = {
          safeStopCluster(clTest)
          safeStopCluster(clTrain)
        }
      )
    }
  }

  if (auditOnly) return(invisible(list(preprocessingDirectory = file.path(resultDirectory, "preprocessing"))))
  allRows <- if (nonEmptyRows(rows)) addPooledObjectiveGap(rows) else data.frame()
  summaryRows <- summarizeResults(allRows)
  utils::write.csv(allRows, resultFile, row.names = FALSE)
  utils::write.csv(summaryRows, summaryFile, row.names = FALSE)
  message(sprintf(
    "[%s] finished %s in %s; peak memory main %.1f GB",
    format(Sys.time(), "%H:%M:%S"), resultDirectory,
    formatDuration(as.numeric(difftime(Sys.time(), runStart, units = "secs"))),
    peakMemoryMbCpp() / 1024
  ))
  invisible(list(results = allRows, summary = summaryRows))
}
