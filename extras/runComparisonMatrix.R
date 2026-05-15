# Reproducible comparison runner for federated methods across tasks and feature sets.
#
# Example:
# Rscript extras/runComparisonMatrix.R \
#   --data-root=data \
#   --tasks=taskA,taskB \
#   --feature-sets=ageSex,ageSexPhenotypes \
#   --methods=DualAvg,ODAL,ODAL1,ADAP,ADAP_PDA,ADAP1,ADAPDiag,PooledLasso \
#   --result-directory=results/comparisonMatrix \
#   --clients=5 \
#   --client-ids=databaseA,databaseB,databaseC,databaseD,databaseE \
#   --folds=1:5 \
#   --taskA-risk-window-end=365 \
#   --taskB-risk-window-end=30

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

baselineMethods <- c("PooledLasso", "LocalAvgLasso", "BiggestSiteLasso")
dualAvgMethods <- c("DualAvg", "DualAvgCpp", "DualAvgR")

readCsvIfExists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

nonEmptyRows <- function(rows) {
  !is.null(rows) && nrow(rows) > 0L
}

matchingCombination <- function(rows, task, fold, featureSet, method) {
  if (!nonEmptyRows(rows)) {
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

isCompletedCombination <- function(rows, task, fold, featureSet, method,
                                   rerunErrors = TRUE) {
  idx <- matchingCombination(rows, task, fold, featureSet, method)
  if (!any(idx)) {
    return(FALSE)
  }
  if (isTRUE(rerunErrors)) {
    return(any(successfulRows(rows)[idx]))
  }
  TRUE
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

safeFilePart <- function(x) {
  gsub("[^A-Za-z0-9_.-]+", "-", as.character(x))
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

methodConfig <- function(method, featureSet, args) {
  adapMethods <- c("ADAP", "ADAP2", "ADAP_PDA", "ADAP1", "ADAPDiag", "Prox-ADAP", "C-ADAP", "MaxConv-ADAP")
  defaultLambdaSearch <- if (method %in% adapMethods) "optimize" else "grid"
  defaultLambdaMetric <- "deviance"
  cfg <- list(
    mapType = firstValue(charCsvArg(argValue(args, "map-type"), "intersection")),
    featureSet = featureSet,
    intercept = logicalArg(argValue(args, "intercept"), TRUE),
    profile = logicalArg(argValue(args, "profile"), FALSE),
    epsilon = firstValue(numCsvArg(argValue(args, "epsilon"), 1e-6)),
    clientFrac = 1,
    rounds = methodRounds(method, args),
    foldsK = firstValue(intCsvArg(argValue(args, "inner-folds"), 5L)),
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
    adapCvDiagnostics = logicalArg(argValue(args, "adap-cv-diagnostics"), FALSE),
    adapTraceDiagnostics = logicalArg(argValue(args, "adap-trace-diagnostics"), FALSE),
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
      FederatedLearning:::.assertWorkerState(
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

  algo <- FederatedLearning:::.getAlgorithm(method)
  if (is.null(algo)) {
    stop("Algorithm '", method, "' is not registered")
  }
  lambdaStrategy <- algo$lambdaStrategy %||% FederatedLearning:::.lambdaStrategyDefault()
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
  tuned <- FederatedLearning:::tuneLambda(
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
    verbose = verbose
  )

  contextFinal <- list(
    cl = clTrain,
    configBase = configBase,
    rounds = config$rounds,
    clientFrac = config$clientFrac,
    epsilon = config$epsilon,
    totalPopSize = totalPopSize,
    globalMap = globalMap
  )
  config$mapping <- globalMap
  config$p <- nrow(globalMap)
  config$lambda <- lambdaStrategy$initial(tuned$bestLambda, totalPopSize, contextFinal)
  config$lambdaSearchDefault <- lambdaDefault
  config$lambdaSearchBest <- tuned$bestLambda
  config$lambdaSearchBestTrain <- tuned$bestLambdaTrain %||% NA_real_
  config$lambdaSearchInnerAuc <- tuned$perf %||% NA_real_
  config$innerCvScore <- tuned$perf %||% NA_real_
  if (isTRUE(verbose)) {
    message(sprintf(
      "Selected %s lambda: search scale = %.5g, fit scale = %.5g, inner-CV AUC = %.5g",
      method,
      config$lambdaSearchBest,
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

  scores <- vapply(trainIds, function(valId) {
    train2 <- setdiff(trainIds, valId)
    trainCluster <- FederatedLearning:::subsetCluster(clTrain, train2)
    valCluster <- FederatedLearning:::subsetCluster(clTrain, valId)
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
    valConfig <- fit$config
    valConfig$mapping <- fit$config$mapping
    valConfig$p <- nrow(fit$config$mapping)
    FederatedLearning::clusterCreateMatrices(valCluster, valConfig)
    ev <- FederatedLearning::clusterEvaluateModel(valCluster, fit$w)
    FederatedLearning:::.innerCvScoreFromEvaluation(ev)
  }, numeric(1))

  score <- FederatedLearning:::.innerCvScoreMean(scores)
  if (!is.finite(score)) {
    stop("Unable to compute a finite inner-CV AUC for method ", method)
  }
  config$innerCvScore <- score
  config$innerCvScoreSd <- FederatedLearning:::.innerCvScoreSd(scores)
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
    config$innerCvScore %||% config$lambdaSearchInnerAuc %||% NA_real_
  }, numeric(1))
  if (all(!is.finite(scores))) {
    best <- 1L
  } else {
    best <- which.max(scores)
  }
  if (isTRUE(verbose)) {
    message(sprintf(
      "Selected %s config by inner CV: %s (inner-CV AUC = %.5g)",
      method,
      tunedConfigs[[best]]$configLabel %||% "default",
      scores[[best]]
    ))
  }
  tunedConfigs[[best]]
}

evaluateWeights <- function(clientData, w, clientId, clientIndex) {
  FederatedLearning:::assertConformableWeights(
    w,
    clientData$xMatrix,
    context = "baseline evaluation"
  )
  preds <- stats::plogis(as.numeric(clientData$xMatrix %*% w))
  y <- clientData$yLabels
  auc <- if (length(unique(y)) == 2) {
    as.numeric(pROC::roc(response = y, predictor = preds, quiet = TRUE)$auc)
  } else {
    NA_real_
  }
  eps <- 1e-15
  pClip <- pmin(pmax(preds, eps), 1 - eps)
  calFit <- if (length(unique(y)) == 2) {
    suppressWarnings(tryCatch(
      stats::glm(y ~ stats::qlogis(pClip), family = stats::binomial()),
      error = function(e) NULL
    ))
  } else {
    NULL
  }
  data.frame(
    client = clientIndex,
    auc = auc,
    logLoss = FederatedLearning:::logLoss(y, preds),
    calibrationIntercept = if (!is.null(calFit)) unname(stats::coef(calFit)[[1]]) else NA_real_,
    calibrationSlope = if (!is.null(calFit)) unname(stats::coef(calFit)[[2]]) else NA_real_,
    density = mean(abs(w) > 1e-4),
    n = length(y),
    outcomes = sum(y),
    clientId = clientId,
    stringsAsFactors = FALSE
  )
}

pooledFitDiagnostics <- function(clientDataList, w, lambda = NA_real_,
                                 intercept = TRUE, tolerance = 1e-4) {
  ns <- vapply(clientDataList, function(x) nrow(x$xMatrix), numeric(1))
  totalN <- sum(ns)
  losses <- vapply(clientDataList, function(x) {
    FederatedLearning:::logisticNegLogLik(w, x$xMatrix, x$yLabels, meanLoss = FALSE)
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

fitCyclopsWeights <- function(clientDataList, args, seed) {
  if (!requireNamespace("Cyclops", quietly = TRUE)) {
    stop("Cyclops is required for pooled/local baseline models")
  }
  x <- do.call(rbind, lapply(clientDataList, `[[`, "xMatrix"))
  y <- unlist(lapply(clientDataList, `[[`, "yLabels"), use.names = FALSE)
  if (length(unique(y)) < 2) {
    stop("Cannot fit Cyclops logistic model: training data has only one outcome class")
  }

  start <- Sys.time()

  cyclopsData <- Cyclops::createCyclopsData(y = y, sx = x, modelType = "lr")
  useCv <- logicalArg(argValue(args, "cyclops-cv"), TRUE)
  variance <- numArg(argValue(args, "cyclops-variance"), numArg(argValue(args, "baseline-variance"), 0.01))
  startingVariance <- numArg(
    argValue(args, "cyclops-starting-variance"),
    numArg(argValue(args, "baseline-starting-variance"), 0.01)
  )
  prior <- Cyclops::createPrior(
    "laplace",
    variance = variance,
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
    elapsedSeconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}

fitBaselineWeights <- function(clientDataList, args, seed) {
  fitCyclopsWeights(
    clientDataList = clientDataList,
    args = args,
    seed = seed
  )
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

fitBaselineFold <- function(method, trainPaths, testPaths, popSettings, config,
                            args, task, featureSet, fold, trainClientIds,
                            testClientIds, testClientIndexes) {
  assertCyclopsMethod(method)

  trainPlp <- lapply(trainPaths, FederatedLearning::loadClientData, popSettings = popSettings)
  trainMap <- FederatedLearning::createGlobalMap(
    lapply(trainPlp, FederatedLearning::getClientFeatures),
    type = config$mapType,
    featureSet = featureSet,
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

  if (identical(method, "PooledLasso")) {
    fit <- fitBaselineWeights(
      trainData,
      args = args,
      seed = intArg(args[["baseline-seed"]], 42L) + fold
    )
  } else {
    eligible <- vapply(trainData, function(x) length(unique(x$yLabels)) == 2, logical(1))
    if (!any(eligible)) {
      stop("Cannot fit local Cyclops baseline: no training client has both outcome classes")
    }
    localIndexes <- which(eligible)
    localFits <- lapply(localIndexes, function(i) {
      fitBaselineWeights(
        list(trainData[[i]]),
        args = args,
        seed = intArg(args[["baseline-seed"]], 42L) + fold + i
      )
    })
    trainN <- vapply(trainData[localIndexes], `[[`, numeric(1), "n")
    if (identical(method, "BiggestSiteLasso")) {
      fit <- localFits[[which.max(trainN)]]
      fit$leadIndex <- localIndexes[[which.max(trainN)]]
    } else {
      weights <- trainN / sum(trainN)
      fit <- list(
        w = Reduce("+", Map(function(localFit, wi) localFit$w * wi, localFits, weights)),
        selectedLambda = NA_real_,
        elapsedSeconds = sum(vapply(localFits, `[[`, numeric(1), "elapsedSeconds")),
        leadIndex = NA_integer_
      )
    }
  }
  pooledDiag <- if (isTRUE(config$pooledDiagnostics %||% TRUE)) {
    pooledFitDiagnostics(
      trainData,
      fit$w,
      lambda = NA_real_,
      intercept = config$intercept,
      tolerance = config$pooledKktTolerance %||% 1e-4
    )
  } else {
    list()
  }

  testPlp <- lapply(testPaths, FederatedLearning::loadClientData, popSettings = popSettings)
  testData <- lapply(testPlp, FederatedLearning::createClientMatrix, config = matrixConfig)
  evalRows <- do.call(rbind, Map(
    evaluateWeights,
    clientData = testData,
    clientId = testClientIds,
    clientIndex = testClientIndexes,
    MoreArgs = list(w = fit$w)
  ))

  cbind(
    data.frame(
      method = method,
      featureSet = featureSet,
      fold = fold,
      p = length(fit$w),
      selectedLambda = fit$selectedLambda %||% NA_real_,
      lambdaPathFile = NA_character_,
      leadIndex = fit$leadIndex %||% NA_integer_,
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
      pooledNegLogLik = pooledDiag$pooledNegLogLik %||% NA_real_,
      pooledMeanLogLoss = pooledDiag$pooledMeanLogLoss %||% NA_real_,
      pooledGradientMaxAbs = pooledDiag$pooledGradientMaxAbs %||% NA_real_,
      pooledKktMaxAbs = pooledDiag$pooledKktMaxAbs %||% NA_real_,
      pooledKktViolating = pooledDiag$pooledKktViolating %||% NA_real_,
      pooledKktMaxCoordinate = pooledDiag$pooledKktMaxCoordinate %||% NA_real_,
      pooledObjectiveGap = NA_real_,
      fitElapsedSeconds = fit$elapsedSeconds,
      elapsedSeconds = fit$elapsedSeconds,
      configLabel = config$configLabel %||% "default",
      stringsAsFactors = FALSE
    ),
    evalRows,
    data.frame(
      messages = 0,
      numbers = 0,
      task = task,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  )
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
      meanCalIntercept = mean(x$calibrationIntercept, na.rm = TRUE),
      meanCalSlope = mean(x$calibrationSlope, na.rm = TRUE),
      meanElapsed = mean(x$elapsedSeconds, na.rm = TRUE),
      messages = maxOrNa(x$messages),
      numbers = maxOrNa(x$numbers),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summaries)
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

communicationMessages <- function(fit, config) {
  roundsCompleted <- fit$roundsCompleted %||% config$rounds
  roundsCompleted * length(config$trainClientPaths)
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
                             debugDirectory = NULL) {
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

  testConfig <- config
  testConfig$mapping <- fit$config$mapping
  testConfig$p <- nrow(fit$config$mapping)
  FederatedLearning::clusterCreateMatrices(clTest, testConfig)
  evalRows <- FederatedLearning::clusterEvaluateModel(clTest, fit$w)
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
        selectedLambda = fit$selectedLambda %||% config[["lambda", exact = TRUE]] %||% NA_real_,
        lambdaSearchBest = fit$config$lambdaSearchBest %||% NA_real_,
        lambdaSearchInnerAuc = fit$config$lambdaSearchInnerAuc %||% NA_real_,
        leadIndex = fit$leadIndex %||% NA_integer_,
        leadWeight = fit$leadWeight %||% NA_real_,
        leadWeightMin = fit$leadWeightMin %||% NA_real_,
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
      sprintf("lambda_%s_%s_fold%s.csv", method, featureSet, fold)
    )
    lambdaDf <- data.frame(
      lambda = fit$lambdaSeq,
      cvScore = fit$cvScores %||% NA_real_,
      cvValid = fit$cvValid %||% NA,
      cvMetric = fit$lambdaSelectionMetric %||% NA_character_
    )
    utils::write.csv(lambdaDf, lambdaPathFile, row.names = FALSE)
  }
  cbind(
    data.frame(
      method = method,
      featureSet = featureSet,
      fold = fold,
      p = length(fit$w),
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
      adapCvMaxOuter = config$maxOuter %||% NA_integer_,
      adapFinalMaxOuter = config$adapFinalMaxOuter %||% NA_integer_,
      adapFinalOuterIterations = adapFinalDiagnostic(fit, "outerIterations", NA_real_),
      adapFinalConverged = adapFinalDiagnostic(fit, "converged", NA),
      adapFinalKktMaxAbs = adapFinalDiagnostic(fit, "kktMaxAbs", NA_real_),
      adapFinalKktViolating = adapFinalDiagnostic(fit, "kktViolating", NA_real_),
      adapFinalBetaMaxAbs = adapFinalDiagnostic(fit, "betaMaxAbs", NA_real_),
      adapFinalEtaMaxAbs = adapFinalDiagnostic(fit, "etaMaxAbs", NA_real_),
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
    data.frame(
      messages = communicationMessages(fit, config),
      numbers = fit$communicationNumbers %||% NA_real_,
      task = task,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

safeStopCluster <- function(cl) {
  if (is.null(cl)) {
    return(invisible(NULL))
  }
  try(FederatedLearning:::stopCluster(cl), silent = TRUE)
  invisible(NULL)
}

runComparison <- function(args) {
  if (!requireNamespace("FederatedLearning", quietly = TRUE)) {
    stop("FederatedLearning must be installed or loaded before running this script")
  }
  if (!requireNamespace("PatientLevelPrediction", quietly = TRUE)) {
    stop("PatientLevelPrediction is required to load PLP data")
  }

  dataRoot <- args[["data-root"]] %||% "data"
  resultDirectory <- args[["result-directory"]] %||% "results/comparisonMatrix"
  dir.create(resultDirectory, recursive = TRUE, showWarnings = FALSE)
  resultFile <- file.path(resultDirectory, "comparison_results.csv")
  summaryFile <- file.path(resultDirectory, "summary_by_method.csv")
  diagnosticsFile <- file.path(resultDirectory, "diagnostics.csv")

  tasks <- csvArg(args[["tasks"]], c("dementia", "readmission", "lungCancer"))
  featureSets <- csvArg(args[["feature-sets"]], c("ageSex", "ageSexPhenotypes"))
  methods <- csvArg(args[["methods"]], c(
    "DualAvg", "ODAL", "ODAL1", "ADAP2", "Prox-ADAP", "C-ADAP",
    "MaxConv-ADAP", "ADAP1", "ADAPDiag"
  ))
  clientIds <- csvArg(args[["client-ids"]], character())
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
  mirai <- logicalArg(args[["mirai"]], FALSE)
  verbose <- logicalArg(args[["verbose"]], TRUE)
  resume <- logicalArg(args[["resume"]], TRUE)
  rerunErrors <- logicalArg(args[["rerun-errors"]], TRUE)
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

  for (task in tasks) {
    clientPaths <- file.path(dataRoot, task, clientIds)
    if (!all(dir.exists(clientPaths))) {
      stop("Missing client paths for task ", task, ": ",
           paste(clientPaths[!dir.exists(clientPaths)], collapse = ", "))
    }
    popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
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
            rerunErrors = rerunErrors
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
      if (all(pending$done) && all(diagnosticsDone)) {
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

          clTest <- FederatedLearning::clusterInit(testHosts, testPaths, mirai = mirai)
          FederatedLearning::clusterLoadData(clTest, testPaths, popSettings)

          for (featureSet in featureSets) {
            featureDebugConfig <- NULL
            if (!is.null(debugDirectory)) {
              featureDebugConfig <- methodConfig(methods[[1]], featureSet, args)
              featureDebugConfig$mapping <- FederatedLearning::clusterCollectCovRefs(
                clTrain,
                type = featureDebugConfig$mapType,
                featureSet = featureSet,
                covariateIds = featureDebugConfig$covariateIds,
                analysisIds = featureDebugConfig$analysisIds
              )
              featureDebugConfig$p <- nrow(featureDebugConfig$mapping)
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
              if (isCompletedCombination(rows, task, fold, featureSet, method, rerunErrors = rerunErrors)) {
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
                    testClientIndexes = testIds
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
                    debugDirectory = debugDirectory
                  )
                },
                error = function(e) {
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
              featureSet = featureSet,
              covariateIds = diagConfig$covariateIds,
              analysisIds = diagConfig$analysisIds
            )
            diagConfig$p <- nrow(diagConfig$mapping)
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

  allRows <- if (nonEmptyRows(rows)) addPooledObjectiveGap(rows) else data.frame()
  summaryRows <- summarizeResults(allRows)
  utils::write.csv(allRows, resultFile, row.names = FALSE)
  utils::write.csv(summaryRows, summaryFile, row.names = FALSE)
  invisible(list(results = allRows, summary = summaryRows))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

if (sys.nframe() == 0L) {
  runComparison(parseArgs())
}
