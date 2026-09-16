# Controlled diagnostic; does not change production fitting or tuning.
# Rscript extras/checkDualAvgLambdaScaling.R --output=results/dualavgLambdaScaling
# Client updates run sequentially in one process to avoid socket overhead.
# Replays the legacy grid handoff, then runs real federated auto-search on four
# PSOCK workers. Only PLP matrix construction is bypassed for synthetic inputs.

runLambdaScalingCheck <- function(output = "results/dualavgLambdaScaling", seed = 914L) {
  stopifnot(requireNamespace("FederatedLearning", quietly = TRUE),
    requireNamespace("Cyclops", quietly = TRUE), requireNamespace("testthat", quietly = TRUE))
  runner <- new.env(parent = globalenv())
  sys.source(file.path("extras", "runComparisonMatrix.R"), runner)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  set.seed(seed)
  p <- 30L
  makeSite <- function(n) {
    x <- cbind(1, matrix(stats::rnorm(n * p), n, p))
    eta <- -1 + x[, 2:4, drop = FALSE] %*% c(1.5, -1, 0.8)
    list(xMatrix = methods::as(Matrix::Matrix(x, sparse = TRUE), "dgCMatrix"),
      yLabels = stats::rbinom(n, 1, stats::plogis(eta)), n = n)
  }
  sites <- replicate(4, makeSite(200L), simplify = FALSE)
  test <- makeSite(4000L)
  mapping <- data.frame(covariateId = seq_len(p), columnId = seq_len(p))
  config <- list(p = p, mapping = mapping, intercept = TRUE, k = 1L,
    etaClient = 1, etaServer = 1, aggregation = "sampleSize", clientFrac = 1,
    rounds = 10000L, epsilon = 0, pooledDiagnostics = FALSE)
  algo <- FederatedLearning:::.getAlgorithm("DualAvg")
  stopifnot(identical(algo$clientUpdate,
    FederatedLearning:::.getAlgorithm("DualAvgCpp")$clientUpdate))

  diagnostics <- function(w, data, lambda) {
    sizes <- vapply(data, `[[`, numeric(1), "n")
    g <- Reduce(`+`, Map(function(site, n) {
      FederatedLearning::gradLogistic(w, site$xMatrix, site$yLabels) * n / sum(sizes)
    }, data, sizes))
    loss <- sum(vapply(data, function(site) {
      FederatedLearning:::logisticNegLogLik(w, site$xMatrix, site$yLabels, meanLoss = FALSE)
    }, numeric(1))) / sum(sizes)
    active <- w != 0
    residual <- pmax(abs(g) - lambda, 0)
    residual[active] <- abs(g[active] + lambda * sign(w[active]))
    residual[[1]] <- abs(g[[1]])
    list(objective = loss + lambda * sum(abs(w[-1L])), loss = loss, kkt = max(residual))
  }

  fitDual <- function(data, lambda, maxRounds = 10000L, kktTolerance = 1e-7) {
    cfg <- config
    cfg$lambda <- lambda
    state <- algo$serverInit(cfg)
    start <- Sys.time()
    for (r in seq_len(maxRounds)) {
      state$r <- r - 1L
      reports <- lapply(data, function(site) algo$clientUpdate(site, state, cfg))
      step <- algo$serverRound(state, reports, cfg)
      state <- step$state
      w <- as.numeric(step$report$w)
      if (r %% 25L == 0L || r == maxRounds) {
        check <- diagnostics(w, data, lambda)
        if (!is.finite(check$kkt)) stop("Non-finite DualAvg KKT residual")
        if (check$kkt < kktTolerance) break
      }
    }
    list(w = w, rounds = r, diagnostics = check,
      elapsed = as.numeric(difftime(Sys.time(), start, units = "secs")))
  }

  # CV sees training sites only, never the independent test sample or pooled solver.
  grid <- c(0.001, 0.003, 0.01, 0.03, 0.1)
  cv <- list()
  for (lambda in grid) {
    for (fold in seq_along(sites)) {
      fit <- fitDual(sites[-fold], lambda)
      score <- runner$binaryAuc(sites[[fold]]$yLabels,
        as.numeric(sites[[fold]]$xMatrix %*% fit$w))
      cv[[length(cv) + 1L]] <- data.frame(lambda = lambda, fold = fold, auc = score,
        rounds = fit$rounds, kkt = fit$diagnostics$kkt)
    }
    message(sprintf("CV lambda=%g mean AUC=%.6f", lambda,
      mean(vapply(tail(cv, 4L), function(x) x$auc, numeric(1)))))
  }
  cv <- do.call(rbind, cv)
  stopifnot(all(is.finite(cv$auc)), all(cv$kkt < 1e-7))
  cvSummary <- stats::aggregate(auc ~ lambda, cv, mean)
  selected <- cvSummary$lambda[[which.max(cvSummary$auc)]]
  n <- sum(vapply(sites, `[[`, numeric(1), "n"))
  # These historical formulas deliberately reproduce the old bug as a control.
  adjusted <- selected * 3 / 4
  legacyLambda <- adjusted / (3 * n / 5)
  cl <- parallel::makePSOCKcluster(length(sites))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    library(Matrix)
    library(FederatedLearning)
  })
  parallel::clusterApply(cl, sites, function(site) {
    assign("clientData", site, envir = .GlobalEnv)
    invisible(NULL)
  })
  stopifnot(all(is.finite(FederatedLearning::clusterEvaluateModel(cl, rep(0, p + 1L))$auc)))
  autoConfig <- config
  autoConfig$epsilon <- 1e-14
  message("Running the production federated auto-search from variance 0.01 with warm starts.")
  runnerConfig <- testthat::with_mocked_bindings(
    runner$tuneDualAvgForFold("DualAvg", clTrain = cl, config = autoConfig,
      trainPopSizes = vapply(sites, `[[`, numeric(1), "n"),
      args = list(), verbose = TRUE),
    clusterCreateMatrices = function(cl, config) invisible(NULL), .package = "FederatedLearning"
  )
  stopifnot(abs(runnerConfig$lambda * n - sqrt(2 / runnerConfig$selectedVariance)) < 1e-12)
  autoFit <- testthat::with_mocked_bindings(
    FederatedLearning::fitFederated(cl, "DualAvg", runnerConfig, verbose = FALSE),
    clusterCreateMatrices = function(cl, config) invisible(NULL), .package = "FederatedLearning"
  )
  lambdas <- c(legacy_double_scaled = legacyLambda,
    grid_size_adjustment = adjusted, unchanged_grid_lambda = selected,
    corrected_auto_search = runnerConfig$lambda)

  pooled <- list(xMatrix = do.call(rbind, lapply(sites, `[[`, "xMatrix")),
    yLabels = unlist(lapply(sites, `[[`, "yLabels")), n = n)
  results <- list()
  weights <- list()
  for (arm in names(lambdas)) {
    lambda <- lambdas[[arm]]
    fit <- fitDual(sites, lambda)
    start <- Sys.time()
    cyclopsData <- Cyclops::createCyclopsData(y = pooled$yLabels, sx = pooled$xMatrix, modelType = "lr")
    oracle <- Cyclops::fitCyclopsModel(cyclopsData,
      prior = Cyclops::createPrior("laplace", variance = 2 / (n * lambda)^2,
        exclude = 1, useCrossValidation = FALSE),
      control = Cyclops::createControl(maxIterations = 10000L, tolerance = 1e-10, threads = 1L, noiseLevel = "silent"))
    wOracle <- as.numeric(stats::coef(oracle))
    elapsedOracle <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    for (solver in c("DualAvg", "Cyclops")) {
      w <- if (solver == "DualAvg") fit$w else wOracle
      check <- diagnostics(w, sites, lambda)
      common <- diagnostics(w, sites, adjusted)
      results[[length(results) + 1L]] <- data.frame(arm = arm, solver = solver,
        lambda = lambda, nonzeroPredictors = sum(w[-1L] != 0),
        nonzeroNoise = sum(w[-(1:4)] != 0),
        testAuc = runner$binaryAuc(test$yLabels, as.numeric(test$xMatrix %*% w)),
        testLogLoss = FederatedLearning:::logisticNegLogLik(w, test$xMatrix, test$yLabels, meanLoss = TRUE),
        objective = check$objective, commonObjectiveAtAdjustedLambda = common$objective,
        kkt = check$kkt, maxAbsDifferenceFromCyclops = max(abs(w - wOracle)),
        rounds = if (solver == "DualAvg") fit$rounds else NA_integer_,
        elapsedSeconds = if (solver == "DualAvg") fit$elapsed else elapsedOracle)
      weights[[paste(arm, solver, sep = "_")]] <- w
    }
  }
  results <- do.call(rbind, results)
  # Confirm that the fast diagnostic loop agrees with the production orchestrator.
  cfg <- config
  cfg$lambda <- adjusted
  cfg$rounds <- results$rounds[results$arm == "grid_size_adjustment" & results$solver == "DualAvg"]
  cfg$convergenceObjective <- "none"
  actual <- testthat::with_mocked_bindings(
    FederatedLearning::fitFederated(cl, "DualAvg", cfg, verbose = FALSE),
    clusterCreateMatrices = function(cl, config) invisible(NULL), .package = "FederatedLearning"
  )
  parity <- data.frame(rounds = actual$roundsCompleted,
    maxAbsDifference = max(abs(actual$w - weights$grid_size_adjustment_DualAvg)))
  stopifnot(parity$maxAbsDifference < 1e-12)
  stopifnot(all(results$kkt < 1e-7),
    all(results$maxAbsDifferenceFromCyclops < 1e-5))

  # Audit the runner's baseline intercept treatment separately from lambda scaling.
  baseline <- runner$fitCyclopsWeights(list(pooled),
    args = list("cyclops-cv" = "false", "cyclops-variance" = as.character(2 / (n * adjusted)^2),
      "cyclops-tolerance" = "1e-10"), seed = seed)
  baselineIntercept <- data.frame(lambda = adjusted, runnerIntercept = baseline$w[[1]],
    unpenalizedIntercept = weights$grid_size_adjustment_Cyclops[[1]],
    runnerInterceptGradient = FederatedLearning::gradLogistic(baseline$w, pooled$xMatrix, pooled$yLabels)[[1]])
  stopifnot(abs(baselineIntercept$runnerInterceptGradient) < 1e-7)
  pooledCv <- runner$fitCyclopsWeights(list(pooled),
    args = list("cyclops-folds" = "4", "cyclops-tolerance" = "1e-10"), seed = seed)
  pooledLambda <- sqrt(2 / pooledCv$selectedLambda) / n
  pooledSummary <- data.frame(priorVariance = pooledCv$selectedLambda,
    lambda = pooledLambda, nonzeroPredictors = sum(pooledCv$w[-1L] != 0),
    kkt = diagnostics(pooledCv$w, sites, pooledLambda)$kkt,
    testAuc = runner$binaryAuc(test$yLabels, as.numeric(test$xMatrix %*% pooledCv$w)))
  stopifnot(pooledSummary$kkt < 1e-6)
  utils::write.csv(pooledSummary, file.path(output, "pooled_auto_cv.csv"), row.names = FALSE)
  autoCheck <- diagnostics(autoFit$w, sites, runnerConfig$lambda)
  autoSummary <- data.frame(lambda = runnerConfig$lambda,
    priorVariance = runnerConfig$selectedVariance,
    innerAuc = runnerConfig$innerCvScore,
    stopReason = runnerConfig$lambdaSearchStopReason,
    rounds = autoFit$roundsCompleted,
    kkt = autoCheck$kkt,
    nonzeroPredictors = sum(autoFit$w[-1L] != 0),
    maxAbsDifferenceFromCyclops = max(abs(autoFit$w - weights$corrected_auto_search_Cyclops)))
  stopifnot(autoSummary$kkt < 1e-6, autoSummary$maxAbsDifferenceFromCyclops < 1e-5)
  utils::write.csv(autoSummary, file.path(output, "auto_search_final.csv"), row.names = FALSE)
  utils::write.csv(runnerConfig$lambdaSearchTrace, file.path(output, "auto_search_folds.csv"), row.names = FALSE)
  utils::write.csv(parity, file.path(output, "production_parity.csv"), row.names = FALSE)
  utils::write.csv(baselineIntercept, file.path(output, "baseline_intercept_audit.csv"), row.names = FALSE)
  utils::write.csv(cv, file.path(output, "cv_folds.csv"), row.names = FALSE)
  utils::write.csv(cvSummary, file.path(output, "cv_summary.csv"), row.names = FALSE)
  utils::write.csv(results, file.path(output, "comparison.csv"), row.names = FALSE)
  utils::write.csv(data.frame(term = c("intercept", paste0("x", seq_len(p))),
    as.data.frame(weights)), file.path(output, "coefficients.csv"), row.names = FALSE)
  saveRDS(list(seed = seed, config = config, sites = sites, test = test,
    selectedCvLambda = selected, runnerConfig = runnerConfig, weights = weights,
    results = results, productionParity = parity, baselineInterceptAudit = baselineIntercept,
    autoSearchFit = autoFit, autoSearchSummary = autoSummary, pooledAutoCv = pooledCv,
    sessionInfo = utils::sessionInfo()), file.path(output, "experiment.rds"))
  print(results, row.names = FALSE)
  invisible(results)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output <- grep("^--output=", args, value = TRUE)
  runLambdaScalingCheck(if (length(output)) sub("^--output=", "", output[[1]]) else "results/dualavgLambdaScaling")
}
