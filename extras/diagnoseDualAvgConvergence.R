# Fixed-penalty optimizer diagnostic on existing PLP extracts, not a CV benchmark.
# Run from this worktree, e.g.:
# Rscript extras/diagnoseDualAvgConvergence.R --data-root=../main/data \
#   --task=lungCancerPhenotypes --fold=1 --max-train-rows-per-site=20000 \
#   --result-directory=../main/results/localDualAvgConvergence/lungCancerPhenotypes

source("extras/runComparisonMatrix.R")

stratifiedRows <- function(y, maxRows, seed) {
  stopifnot(all(y %in% c(0, 1)), length(unique(y)) == 2L, maxRows >= 2)
  if (length(y) <= maxRows) return(seq_along(y))
  set.seed(seed)
  nCase <- max(1L, min(sum(y == 1L), round(maxRows * mean(y))))
  nControl <- min(sum(y == 0L), maxRows - nCase)
  # sample.int avoids sample(x) treating a single row index as a range.
  take <- function(ids, n) ids[sample.int(length(ids), n)]
  sort(c(take(which(y == 1L), nCase), take(which(y == 0L), nControl)))
}

recordedServerRound <- function(baseRound, recorder, every) {
  force(baseRound)
  force(recorder)
  force(every)
  function(serverState, clientReports, config) {
    result <- baseRound(serverState, clientReports, config)
    recorder$previousWeights <- recorder$lastWeights
    recorder$lastWeights <- as.numeric(result$report$w)
    round <- serverState$r + 1L
    if (round == 1L || round %% every == 0L) {
      recorder$weights[[as.character(round)]] <- as.numeric(result$report$w)
      recorder$dualValues[[as.character(round)]] <- as.numeric(result$report$z)
    }
    result
  }
}

recordedFitter <- function(recorder, every) {
  algorithm <- FederatedLearning:::.getAlgorithm("DualAvg")
  algorithm$serverRound <- recordedServerRound(algorithm$serverRound, recorder, every)
  fitter <- FederatedLearning::fitFederated
  scope <- new.env(parent = environment(fitter))
  scope$.getAlgorithm <- function(name) {
    stopifnot(name == "DualAvg")
    algorithm
  }
  environment(fitter) <- scope
  fitter
}

runDiagnostic <- function(args) {
  task <- argValue(args, "task") %||% "lungCancerPhenotypes"
  fold <- intArg(argValue(args, "fold"), 1L)
  featureSet <- argValue(args, "feature-set") %||% "ageSexPhenotypes"
  maxRows <- numArg(argValue(args, "max-train-rows-per-site"), 20000)
  rounds <- intArg(argValue(args, "rounds"), 10000L)
  k1Rounds <- intArg(argValue(args, "k1-rounds"), rounds)
  seed <- intArg(argValue(args, "seed"), 20260915L)
  every <- intArg(argValue(args, "checkpoint-every"), 100L)
  root <- normalizePath(argValue(args, "data-root") %||% "../main/data")
  output <- argValue(args, "result-directory") %||% "../main/results/localDualAvgConvergence"
  stopifnot(fold %in% 1:5, maxRows >= 2, rounds > 0, k1Rounds > 0, every > 0)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(output)
  if (file.exists(file.path(output, "provenance.rds"))) {
    stop("Use a new result directory; this diagnostic does not overwrite prior experiments")
  }
  libraries <- .libPaths()
  paths <- file.path(root, task, paste0("client", 1:5))
  stopifnot(all(dir.exists(paths)))
  trainIds <- setdiff(1:5, fold)
  popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE, minTimeAtRisk = 1L, riskWindowStart = 1L,
    riskWindowEnd = intArg(argValue(args, "risk-window-end"), taskRiskWindow(task)),
    removeSubjectsWithPriorOutcome = TRUE, priorOutcomeLookback = 99999L
  )
  cat("Task:", task, "held out:", fold, "horizon:", popSettings$riskWindowEnd,
      "max training rows/site:", maxRows, "\n")
  cl <- parallel::makePSOCKcluster(length(trainIds), outfile = file.path(output, "workers.log"))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterCall(cl, function(libraries) {
    .libPaths(libraries)
    library(FederatedLearning)
    NULL
  }, libraries)
  FederatedLearning::clusterLoadData(cl, paths[trainIds], popSettings)
  config <- methodConfig("DualAvg", featureSet, list())
  config$mapping <- FederatedLearning::clusterCollectCovRefs(
    cl, type = "intersection", featureSet = featureSet
  )
  config$p <- nrow(config$mapping)
  stopifnot(config$p > 2L || featureSet == "ageSex")
  parallel::clusterExport(cl, "stratifiedRows", envir = environment(stratifiedRows))
  counts <- parallel::clusterApply(cl, trainIds, function(id, maxRows, seed) {
    y <- as.integer(plpData$population$outcomeCount)
    rows <- stratifiedRows(y, maxRows, seed + id)
    data <- plpData
    data$population <- data$population[rows, , drop = FALSE]
    assign("plpData", data, envir = .GlobalEnv)
    data.frame(client = id, originalRows = length(y), originalOutcomes = sum(y),
      trainingRows = length(rows), trainingOutcomes = sum(y[rows]))
  }, maxRows, seed)
  counts <- do.call(rbind, counts)
  write.csv(counts, file.path(output, "sample_counts.csv"), row.names = FALSE)
  print(counts)
  FederatedLearning::clusterCreateMatrices(cl, config)
  trainData <- parallel::clusterEvalQ(cl, clientData)
  testPlp <- FederatedLearning::loadClientData(paths[fold], popSettings)
  testData <- FederatedLearning::createClientMatrix(testPlp, config)
  n <- sum(vapply(trainData, `[[`, numeric(1), "n"))
  variance <- numArg(argValue(args, "variance"), 0.02)
  config$lambda <- numArg(argValue(args, "lambda"),
    FederatedLearning:::.laplaceVarianceToLambda(variance, n))
  variance <- 2 / (n * config$lambda)^2
  config$aggregation <- "sampleSize"
  config$roundOffset <- 0L
  config$initialZ <- NULL
  config$rounds <- rounds
  config$pooledDiagnostics <- TRUE
  config$pooledKktTolerance <- 1e-7
  config$dualAvgGapDiagnostics <- logicalArg(argValue(args, "dualavg-gap-diagnostics"), FALSE)
  config$dualAvgGapCheckEvery <- intArg(argValue(args, "dualavg-gap-check-every"), 100L)
  if (!is.null(argValue(args, "dualavg-kkt-tolerance"))) {
    config$dualAvgKktTolerance <- numArg(argValue(args, "dualavg-kkt-tolerance"), NA_real_)
    config$dualAvgKktCheckEvery <- intArg(argValue(args, "dualavg-kkt-check-every"), 100L)
  }
  stopifnot(is.finite(config$lambda), config$lambda > 0)
  saveRDS(list(args = args, config = config, populationSettings = popSettings,
    task = task, fold = fold, featureSet = featureSet, seed = seed,
    trainIds = trainIds, trainPaths = paths[trainIds], testPath = paths[fold],
    counts = counts, testRows = testData$n, testOutcomes = sum(testData$yLabels),
    variance = variance, sessionInfo = sessionInfo(),
    scriptHash = tools::md5sum("extras/diagnoseDualAvgConvergence.R"),
    revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
    file.path(output, "provenance.rds"))

  # All solvers receive the same columns, age/100 scale and unpenalized intercept.
  # No baseline-specific feature filtering/normalization and no penalty tuning.
  baselineArgs <- list("cyclops-cv" = "false", "cyclops-variance" = as.character(variance),
    "cyclops-tolerance" = "1e-12", "cyclops-max-iterations" = "100000",
    "cyclops-threads" = "1")
  oracle <- fitCyclopsWeights(trainData, baselineArgs, seed, intercept = TRUE)
  stopifnot(all(is.finite(oracle$w)), oracle$fittingSettings$returnFlag == "SUCCESS")
  saveRDS(oracle, file.path(output, "PooledLasso.rds"))
  columnIds <- c("intercept", as.character(config$mapping$covariateId))
  ageColumn <- match("1002", columnIds)
  penalty <- function(w) config$lambda * sum(abs(w[-1L]))
  diagnostics <- function(w) {
    d <- pooledFitDiagnostics(trainData, w, lambda = config$lambda, tolerance = 1e-7)
    data.frame(meanNll = d$pooledMeanLogLoss,
      penalizedObjective = d$pooledMeanLogLoss + penalty(w),
      kktMax = d$pooledKktMaxAbs, kktViolations = d$pooledKktViolating,
      kktMaxCovariate = columnIds[d$pooledKktMaxCoordinate],
      nonzero = sum(w[-1L] != 0), age = w[ageColumn],
      coefMaxError = max(abs(w - oracle$w)), coefL2Error = sqrt(sum((w - oracle$w)^2)))
  }
  oracleDiag <- diagnostics(oracle$w)
  cat("Pooled reference (same penalty):\n")
  print(oracleDiag)
  if (oracleDiag$kktMax > 1e-7) {
    stop("Cyclops reference did not reach the requested KKT accuracy")
  }
  summaryRows <- list()
  trajectories <- list()
  coefficientRows <- list()
  dualAuditRows <- list()
  auditDual <- function(name, w) {
    summaries <- lapply(trainData, function(d) {
      FederatedLearning:::logisticObjectiveGradientCpp(d$xMatrix, w, d$yLabels, dualStats = TRUE)
    })
    request <- FederatedLearning:::.dualGapRequest(summaries, config$lambda, TRUE, "sampleSize", 1L)
    for (i in seq_along(summaries)) {
      e <- FederatedLearning:::logisticDualEntropyCpp(summaries[[i]]$dualResidual,
        trainData[[i]]$yLabels, request$scales)
      e$round <- 1L
      summaries[[i]]$dualEvaluation <- e
    }
    # Independent R/Matrix check using the row-level candidates, available only
    # in this local diagnostic driver, not collected by the federated protocol.
    gradient <- Reduce(`+`, lapply(seq_along(trainData), function(i) {
      d <- trainData[[i]]
      residual <- as.numeric(summaries[[i]]$dualResidual) * request$scales[d$yLabels + 1L]
      stopifnot(all(d$yLabels + residual >= 0), all(d$yLabels + residual <= 1))
      as.numeric(Matrix::crossprod(d$xMatrix, residual)) / n
    }))
    cat(sprintf("Dual audit %s: summary norm %.17g; row-wise norm %.17g; lambda %.17g; balance %.17g\n",
      name, request$gradientMaxAbs, max(abs(gradient[-1])), config$lambda, gradient[1]))
    evaluated <- FederatedLearning:::.dualGapEvaluation(summaries, request,
      config$lambda, TRUE, "sampleSize")
    stopifnot(abs(gradient[1]) < 1e-12, max(abs(gradient[-1])) <= config$lambda * (1 + 1e-12),
      evaluated$objective <= oracleDiag$penalizedObjective + 1e-12)
    d <- diagnostics(w)
    gap <- FederatedLearning:::.checkedDualityGap(d$penalizedObjective, evaluated$objective)
    dualAuditRows[[name]] <<- cbind(data.frame(condition = name, dualLowerBound = evaluated$objective,
      dualityGap = gap, balance = gradient[1], dualGradientMaxAbs = max(abs(gradient[-1]))), d)
    write.csv(do.call(rbind, dualAuditRows), file.path(output, "duality_audit.csv"), row.names = FALSE)
  }
  recordResult <- function(name, w, k, epsilon, roundsDone, elapsed,
                           objective = "gradient", stoppingValue = NA_real_,
                           stopReason = "Cyclops SUCCESS", kktChecks = NA_integer_,
                           kktStopMax = NA_real_, dualityGap = NA_real_, dualLowerBound = NA_real_) {
    d <- diagnostics(w)
    pred <- stats::plogis(as.numeric(testData$xMatrix %*% w))
    auc <- as.numeric(pROC::auc(pROC::roc(testData$yLabels, pred,
      direction = "<", levels = c(0, 1), quiet = TRUE)))
    d$objectiveGap <- d$penalizedObjective - oracleDiag$penalizedObjective
    row <- cbind(data.frame(condition = name, convergenceObjective = objective,
      stoppingValue = stoppingValue, stopReason = stopReason, k = k, epsilon = epsilon,
      kktChecks = kktChecks, kktStopMax = kktStopMax,
      dualityGap = dualityGap, dualLowerBound = dualLowerBound,
      rounds = roundsDone, localGradientSteps = roundsDone * k, fitSeconds = elapsed,
      auc = auc, testLogLoss = FederatedLearning:::logisticNegLogLik(
        w, testData$xMatrix, testData$yLabels, meanLoss = TRUE)), d)
    summaryRows[[name]] <<- row
    coefficientRows[[name]] <<- data.frame(condition = name, covariateId = columnIds,
      isIntercept = seq_along(w) == 1L, coefficient = as.numeric(w), selected = w != 0)
    write.csv(do.call(rbind, summaryRows), file.path(output, "summary.csv"), row.names = FALSE)
    write.csv(do.call(rbind, coefficientRows), file.path(output, "coefficients.csv"), row.names = FALSE)
    print(row)
    if (config$dualAvgGapDiagnostics) auditDual(name, w)
  }
  recordResult("PooledLasso", oracle$w, NA_integer_, 1e-12, NA_integer_, oracle$elapsedSeconds)
  if (logicalArg(argValue(args, "matched-cyclops"), FALSE)) {
    baselineArgs[["cyclops-tolerance"]] <- "2e-6"
    matched <- fitCyclopsWeights(trainData, baselineArgs, seed, intercept = TRUE)
    stopifnot(all(is.finite(matched$w)), matched$fittingSettings$returnFlag == "SUCCESS")
    saveRDS(matched, file.path(output, "PooledLasso_matched.rds"))
    recordResult("PooledLasso_matched", matched$w, NA_integer_, 2e-6,
      NA_integer_, matched$elapsedSeconds)
  }
  for (directory in csvArg(argValue(args, "audit-result-directories"), character())) {
    previous <- readRDS(file.path(directory, "provenance.rds"))
    stopifnot(previous$task == task, previous$fold == fold,
      isTRUE(all.equal(previous$counts, counts)), previous$config$lambda == config$lambda,
      isTRUE(all.equal(previous$config$mapping, config$mapping)),
      isTRUE(all.equal(previous$populationSettings, popSettings)))
    for (file in list.files(directory, pattern = "^k.*\\.rds$", full.names = TRUE)) {
      fit <- readRDS(file)$fit
      auditDual(paste(basename(directory), basename(file), sep = "/"), fit$w)
    }
  }

  # A recording-only wrapper calls the registered production C++ server unchanged.
  # Verify its end-to-end equivalence on these real matrices before longer fits.
  recorder <- new.env(parent = emptyenv())
  recorder$weights <- list()
  recorder$dualValues <- list()
  fitRecorded <- recordedFitter(recorder, every)
  checkConfig <- config
  checkConfig$rounds <- 10L
  checkConfig$epsilon <- 0
  plain <- FederatedLearning::fitFederated(cl, "DualAvg", checkConfig, verbose = FALSE)
  recorded <- fitRecorded(cl, "DualAvg", checkConfig, verbose = FALSE)
  stopifnot(isTRUE(all.equal(plain$w, recorded$w, tolerance = 1e-13)),
    isTRUE(all.equal(plain$z, recorded$z, tolerance = 1e-13)))
  cat("Production/recording parity passed on real PLP data.\n")

  conditions <- data.frame(
    name = c("k10_eps1e6", "k10_eps1e10", "k10_fixed", "k1_fixed",
      "k10_oracle_start", "k1_oracle_start", "k10_null_start", "k1_null_start",
      "k10_cyclops_zero", "k10_cyclops_null"),
    k = c(10L, 10L, 10L, 1L, 10L, 1L, 10L, 1L, 10L, 10L),
    epsilon = c(1e-6, 1e-10, 0, 0, 0, 0, 0, 0, 2e-6, 2e-6),
    rounds = c(rounds, rounds, rounds, k1Rounds, 1000L, 1000L, rounds, k1Rounds,
      rounds, rounds),
    initialization = c("zero", "zero", "zero", "zero", "oracle", "oracle", "null", "null",
      "zero", "null"),
    objective = c(rep("negLogLikelihood", 8), rep("cyclopsGradient", 2))
  )
  requested <- csvArg(argValue(args, "conditions"), conditions$name)
  stopifnot(all(requested %in% conditions$name))
  conditions <- conditions[conditions$name %in% requested, , drop = FALSE]
  write.csv(conditions, file.path(output, "conditions.csv"), row.names = FALSE)
  for (i in seq_len(nrow(conditions))) {
    condition <- conditions[i, ]
    cat("\nCondition:", condition$name, "\n")
    cfg <- config
    cfg$k <- condition$k
    cfg$epsilon <- condition$epsilon
    cfg$rounds <- condition$rounds
    cfg$convergenceObjective <- condition$objective
    # Oracle starts are fixed-point probes only, never federated benchmarks.
    cfg$initialZ <- switch(condition$initialization,
      oracle = oracle$w,
      null = c(stats::qlogis(sum(counts$trainingOutcomes) / n), rep(0, config$p)),
      zero = NULL)
    recorder$weights <- list()
    recorder$dualValues <- list()
    recorder$previousWeights <- NULL
    recorder$lastWeights <- NULL
    start <- proc.time()[["elapsed"]]
    fit <- fitRecorded(cl, "DualAvg", cfg, verbose = TRUE)
    elapsed <- proc.time()[["elapsed"]] - start
    stopifnot(all(is.finite(fit$w)))
    recorder$weights[[as.character(fit$roundsCompleted)]] <- as.numeric(fit$w)
    recorder$dualValues[[as.character(fit$roundsCompleted)]] <- as.numeric(fit$z)
    stopStatistic <- function(w) {
      if (cfg$convergenceObjective == "cyclopsGradient") {
        sum(vapply(trainData, function(d) {
          sum(d$yLabels * as.numeric(d$xMatrix %*% w))
        }, numeric(1)))
      } else {
        sum(vapply(trainData, function(d) {
          FederatedLearning:::logisticNegLogLik(w, d$xMatrix, d$yLabels)
        }, numeric(1)))
      }
    }
    current <- stopStatistic(fit$w)
    stoppingValue <- if (!is.null(recorder$previousWeights)) {
      abs(current - stopStatistic(recorder$previousWeights)) / (abs(current) + 1)
    } else NA_real_
    stopReason <- if (!is.null(fit$stopReason)) fit$stopReason else if (
      is.finite(stoppingValue) && stoppingValue < cfg$epsilon) {
      "criterion met"
    } else "round limit"
    stopifnot(isTRUE(all.equal(current, fit$globalObjective, tolerance = 1e-12)))
    saveRDS(list(fit = fit, coefficientsByRound = recorder$weights,
      dualValuesByRound = recorder$dualValues,
      previousWeights = recorder$previousWeights, stoppingValue = stoppingValue,
      stopReason = stopReason, fitSeconds = elapsed, initialization = condition$initialization),
      file.path(output, paste0(condition$name, ".rds")))
    if (!is.null(fit$dualGapHistory)) {
      write.csv(fit$dualGapHistory, file.path(output, paste0(condition$name, "-dual-gap.csv")),
        row.names = FALSE)
    }
    trace <- do.call(rbind, lapply(names(recorder$weights), function(round) {
      cbind(data.frame(condition = condition$name, round = as.integer(round),
        localGradientSteps = as.integer(round) * cfg$k,
        ageDualValue = recorder$dualValues[[round]][ageColumn],
        l1Threshold = as.integer(round) * cfg$k * cfg$etaClient * cfg$etaServer * cfg$lambda),
        diagnostics(recorder$weights[[round]]))
    }))
    trace$objectiveGap <- trace$penalizedObjective - oracleDiag$penalizedObjective
    trajectories[[condition$name]] <- trace
    write.csv(do.call(rbind, trajectories), file.path(output, "trajectory.csv"), row.names = FALSE)
    recordResult(condition$name, fit$w, cfg$k, cfg$epsilon, fit$roundsCompleted, elapsed,
      cfg$convergenceObjective, stoppingValue, stopReason,
      fit$kktChecks %||% NA_integer_, fit$kktMaxAbs %||% NA_real_,
      fit$dualityGap %||% NA_real_, fit$dualLowerBound %||% NA_real_)
  }
  invisible(do.call(rbind, summaryRows))
}

if (sys.nframe() == 0L) runDiagnostic(parseArgs())
