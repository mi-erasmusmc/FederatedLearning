# Reproducible comparison runner for federated methods across tasks and feature sets.
#
# Example:
# Rscript extras/runComparisonMatrix.R \
#   --data-root=data \
#   --tasks=taskA,taskB \
#   --feature-sets=ageSex,ageSexPhenotypes \
#   --methods=DualAvg,ODAL,ADAP,ADAP_PDA,ADAP1,ADAPDiag \
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
    dementia = 30,
    readmission = 30,
    lungCancer = 5 * 365,
    lungCancerPhenotypes = 5 * 365,
    30
  )
}

methodRounds <- function(method, args) {
  switch(method,
    DualAvg = intArg(args[["dualavg-rounds"]], 10000L),
    FastDualAvg = intArg(args[["dualavg-rounds"]], 10000L),
    ADAP2 = intArg(args[["pda-rounds"]], 3L),
    ODAL = intArg(args[["pda-rounds"]], 3L),
    ADAP = intArg(args[["pda-rounds"]], 3L),
    ADAP_PDA = intArg(args[["pda-rounds"]], 3L),
    ADAP1 = intArg(args[["pda-rounds"]], 3L),
    ADAPDiag = intArg(args[["pda-rounds"]], 3L),
    intArg(args[["rounds"]], 1000L)
  )
}

baselineMethods <- c("PooledLasso", "LocalAvgLasso", "BiggestSiteLasso")

methodConfig <- function(method, featureSet, args) {
  cfg <- list(
    mapType = args[["map-type"]] %||% "intersection",
    featureSet = featureSet,
    intercept = logicalArg(args[["intercept"]], TRUE),
    profile = FALSE,
    epsilon = numArg(args[["epsilon"]], 1e-6),
    clientFrac = 1,
    rounds = methodRounds(method, args),
    foldsK = intArg(args[["inner-folds"]], 5L),
    cvSeed = intArg(args[["cv-seed"]], 42L),
    maxIter = intArg(args[["max-iter"]], 1000L),
    maxOuter = intArg(args[["max-outer"]], 100L),
    maxInner = intArg(args[["max-inner"]], 100L),
    lambdaGridLen = intArg(args[["lambda-grid-len"]], 100L)
  )

  if (method %in% c("DualAvg", "FastDualAvg")) {
    cfg$etaClient <- numArg(args[["eta-client"]], 1)
    cfg$etaServer <- numArg(args[["eta-server"]], 1)
    cfg$k <- intArg(args[["k"]], 10L)
    cfg$lambda <- numArg(args[["dualavg-lambda"]], 2.09e-4)
    cfg$mapType <- args[["dualavg-map-type"]] %||% cfg$mapType
  } else {
    cfg$lambda <- if (!is.null(args[["lambda"]])) numArg(args[["lambda"]], NA_real_) else NULL
  }

  if (identical(method, "ADAPDiag") && !is.null(args[["adapdiag-style"]])) {
    cfg$adapDiagStyle <- args[["adapdiag-style"]]
  }
  if (identical(method, "ADAP2")) {
    cfg$hessian <- args[["adap2-hessian"]] %||% "diag"
    cfg$maxFullHessianP <- intArg(args[["max-full-hessian-p"]], 2000L)
  }
  cfg
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

fitGlmnetWeights <- function(clientDataList, args, seed) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("glmnet is required for pooled/local lasso baselines")
  }
  x <- do.call(rbind, lapply(clientDataList, `[[`, "xMatrix"))
  y <- unlist(lapply(clientDataList, `[[`, "yLabels"), use.names = FALSE)
  if (length(unique(y)) < 2) {
    stop("Cannot fit logistic lasso: training data has only one outcome class")
  }
  lambda <- if (!is.null(args[["baseline-lambda"]])) {
    numArg(args[["baseline-lambda"]], NA_real_)
  } else {
    NA_real_
  }
  start <- Sys.time()
  if (is.finite(lambda)) {
    fit <- glmnet::glmnet(
      x = x,
      y = y,
      family = "binomial",
      alpha = 1,
      lambda = lambda,
      intercept = FALSE,
      standardize = FALSE,
      maxit = intArg(args[["baseline-maxit"]], 100000L)
    )
    w <- as.numeric(stats::coef(fit, s = lambda))[-1]
    selectedLambda <- lambda
  } else {
    set.seed(seed)
    fit <- glmnet::cv.glmnet(
      x = x,
      y = y,
      family = "binomial",
      alpha = 1,
      nfolds = intArg(args[["baseline-folds"]], 5L),
      intercept = FALSE,
      standardize = FALSE,
      type.measure = args[["baseline-measure"]] %||% "deviance",
      maxit = intArg(args[["baseline-maxit"]], 100000L)
    )
    selectedLambda <- fit$lambda.min
    w <- as.numeric(stats::coef(fit, s = "lambda.min"))[-1]
  }
  list(
    w = w,
    selectedLambda = selectedLambda,
    elapsedSeconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}

fitBaselineFold <- function(method, trainPaths, testPaths, popSettings, config,
                            args, task, featureSet, fold, trainClientIds,
                            testClientIds, testClientIndexes) {
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
    fit <- fitGlmnetWeights(
      trainData,
      args = args,
      seed = intArg(args[["baseline-seed"]], 42L) + fold
    )
  } else {
    eligible <- vapply(trainData, function(x) length(unique(x$yLabels)) == 2, logical(1))
    if (!any(eligible)) {
      stop("Cannot fit local lasso baseline: no training client has both outcome classes")
    }
    localIndexes <- which(eligible)
    localFits <- lapply(localIndexes, function(i) {
      fitGlmnetWeights(
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
      trainObjective = NA_real_,
      hessianDim = NA_character_,
      hessianDiagMin = NA_real_,
      hessianDiagMax = NA_real_,
      hessianCondition = NA_real_,
      elapsedSeconds = fit$elapsedSeconds,
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

fitFederatedFold <- function(method, clTrain, clTest, config, resultDirectory,
                             task, featureSet, fold, testClientIds, verbose) {
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

  lambdaPathFile <- NA_character_
  if (!is.null(fit$lambdaSeq) && length(fit$lambdaSeq) > 0L) {
    lambdaPathFile <- file.path(
      resultDirectory,
      sprintf("lambda_%s_%s_fold%s.csv", method, featureSet, fold)
    )
    lambdaDf <- data.frame(
      lambda = fit$lambdaSeq,
      cvScore = fit$cvScores %||% NA_real_
    )
    utils::write.csv(lambdaDf, lambdaPathFile, row.names = FALSE)
  }

  cbind(
    data.frame(
      method = method,
      featureSet = featureSet,
      fold = fold,
      p = length(fit$w),
      selectedLambda = fit$selectedLambda %||% config$lambda %||% NA_real_,
      lambdaPathFile = lambdaPathFile,
      leadIndex = fit$leadIndex %||% NA_integer_,
      trainObjective = fit$globalObjective %||% NA_real_,
      hessianDim = fit$hessianDim %||% NA_character_,
      hessianDiagMin = fit$hessianDiagMin %||% NA_real_,
      hessianDiagMax = fit$hessianDiagMax %||% NA_real_,
      hessianCondition = fit$hessianCondition %||% NA_real_,
      elapsedSeconds = elapsed,
      stringsAsFactors = FALSE
    ),
    evalRows,
    data.frame(
      messages = config$rounds * length(config$trainClientPaths),
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

  tasks <- csvArg(args[["tasks"]], c("dementia", "readmission", "lungCancer"))
  featureSets <- csvArg(args[["feature-sets"]], c("ageSex", "ageSexPhenotypes"))
  methods <- csvArg(args[["methods"]], c("DualAvg", "ODAL", "ADAP", "ADAP_PDA", "ADAP1", "ADAPDiag"))
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

  rows <- list()
  diagnostics <- list()
  rowIndex <- 1L
  diagIndex <- 1L

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
          FederatedLearning::clusterLoadData(clTrain, trainPaths, popSettings)

          clTest <- FederatedLearning::clusterInit(testHosts, testPaths, mirai = mirai)
          FederatedLearning::clusterLoadData(clTest, testPaths, popSettings)

          for (featureSet in featureSets) {
            for (method in methods) {
              message(sprintf(
                "[%s] task=%s fold=%s featureSet=%s method=%s",
                format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method
              ))
              config <- methodConfig(method, featureSet, args)
              config$trainClientPaths <- trainPaths
              res <- tryCatch(
                if (method %in% baselineMethods) {
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
                    verbose = verbose
                  )
                },
                error = function(e) {
                  message(sprintf(
                    "[%s] ERROR task=%s fold=%s featureSet=%s method=%s: %s",
                    format(Sys.time(), "%H:%M:%S"), task, fold, featureSet, method,
                    conditionMessage(e)
                  ))
                  data.frame(
                    method = method,
                    featureSet = featureSet,
                    fold = fold,
                    p = NA_integer_,
                    selectedLambda = NA_real_,
                    lambdaPathFile = NA_character_,
                    leadIndex = NA_integer_,
                    trainObjective = NA_real_,
                    hessianDim = NA_character_,
                    hessianDiagMin = NA_real_,
                    hessianDiagMax = NA_real_,
                    hessianCondition = NA_real_,
                    elapsedSeconds = NA_real_,
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
              rows[[rowIndex]] <- res
              rowIndex <- rowIndex + 1L
              utils::write.csv(
                do.call(rbind, rows),
                file.path(resultDirectory, "comparison_results.csv"),
                row.names = FALSE
              )
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
            diagnostics[[diagIndex]] <- diagRows
            diagIndex <- diagIndex + 1L
            utils::write.csv(
              do.call(rbind, diagnostics),
              file.path(resultDirectory, "diagnostics.csv"),
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

  allRows <- do.call(rbind, rows)
  summaryRows <- summarizeResults(allRows)
  utils::write.csv(allRows, file.path(resultDirectory, "comparison_results.csv"), row.names = FALSE)
  utils::write.csv(summaryRows, file.path(resultDirectory, "summary_by_method.csv"), row.names = FALSE)
  invisible(list(results = allRows, summary = summaryRows))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

if (sys.nframe() == 0L) {
  runComparison(parseArgs())
}
