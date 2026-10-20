# Evaluates expr under a fixed Mersenne-Twister seed and restores the caller's
# RNG kind and state, so seeded draws are reproducible on any worker.
.withFixedSeed <- function(seed, expr) {
  oldKind <- RNGkind()
  oldSeed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    do.call(RNGkind, as.list(oldKind))
    if (is.null(oldSeed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", oldSeed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  force(expr)
}

# Outcome-stratified fold ids: each outcome class is spread evenly over k folds.
.stratifiedFoldId <- function(y, k, seed) {
  .withFixedSeed(seed, {
    folds <- integer(length(y))
    for (class in sort(unique(y))) {
      rows <- which(y == class)
      folds[rows] <- sample(rep_len(seq_len(k), length(rows)))
    }
    folds
  })
}

# Searches the Laplace prior variance with the unimodal search used for DualAvg
# tuning: the same start, stopping rules and evaluation budget. scoreVariance()
# returns c(score, sd), where higher scores are better.
.searchPriorVariance <- function(scoreVariance, startingVariance = 0.01, maxEvals = 25L,
                                 stdStep = 2, stopByY = 1e-2, stopByX = log(1.5), firstCut = 1.0) {
  if (!is.numeric(maxEvals) || length(maxEvals) != 1L || !is.finite(maxEvals) || maxEvals < 1) {
    stop("The variance search needs a positive evaluation budget")
  }
  search <- unimodalSearchInit(stdStep, stopByY, stopByX, firstCut, init = startingVariance)
  trace <- list()
  evaluate <- function(variance, iteration) {
    score <- scoreVariance(variance)
    if (!is.finite(score[[1]])) stop("Validation score is not finite for prior variance ", variance)
    trace[[length(trace) + 1L]] <<- data.frame(iteration = iteration, variance = variance,
      score = score[[1]], scoreSd = score[[2]])
    search$try(variance, score[[1]], score[[2]])
  }
  evaluate(startingVariance, 0L)
  stopReason <- "searchConverged"
  iteration <- 1L
  repeat {
    step <- search$step()
    if (!step$continue) break
    if (iteration >= maxEvals) {
      stopReason <- "maxEvaluations"
      warning("Prior variance search reached its evaluation budget; using the best evaluated candidate",
        call. = FALSE)
      break
    }
    if (!is.finite(step$nextX) || step$nextX <= 0) {
      stop("Prior variance search proposed a non-positive or non-finite candidate")
    }
    evaluate(step$nextX, iteration)
    iteration <- iteration + 1L
  }
  list(variance = search$bestX(), score = search$bestY(), trace = do.call(rbind, trace),
    stopReason = stopReason)
}
