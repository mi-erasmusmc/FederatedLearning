# Fit y approximately c-1 + c1*log(x) + c2*(log(x))^2 by weighted least-squares
quadrLogFit <- function(x, y, ySd) {
  n <- length(x)
  stopifnot(n >= 3)
  w <- ifelse(ySd > 0, 1 / ySd, NA_real_)
  if (all(is.na(w))) {
    w[] <- 1
  } else {
    m <- min(ySd[ySd > 0])
    z <- if (is.finite(m)) 100 / m else 1
    w[is.na(w)] <- z
  }
  L <- log(x)
  X <- cbind(1, L, L^2)
  W <- diag(w, n, n)
  XtW <- t(X) %*% W
  coefs <- solve(XtW %*% X, XtW %*% y)
  as.numeric(coefs)
}

unimodalSearchInit <- function(stdStep = 2,
                               stopByY = 1e-2,
                               stopByX = log(1.5),
                               firstCut = 1.0,
                               init = 1.0) {
  state <- new.env()
  state$x <- numeric(0)
  state$y <- numeric(0)
  state$ySd <- numeric(0)
  state$bestIdx <- NA_integer_
  state$stdStep <- stdStep
  state$stopByY <- stopByY
  state$stopByX <- stopByX
  state$firstCut <- firstCut
  state$init <- init

  state$step <- function() {
    x <- state$x
    y <- state$y
    ys <- state$ySd
    n <- length(x)
    stopifnot(n >= 1)
    cont <- TRUE
    nextX <- NA_real_
    expected <- NA_real_

    if (n == 1) {
      if (x[1] < state$firstCut) {
        nextX <- x[1] * state$stdStep
      } else {
        nextX <- x[1] / state$stdStep
      }
    } else if (n == 2) {
      # step away from the worse end:
      if (y[1] > y[2]) {
        nextX <- x[1] / state$stdStep
      } else {
        nextX <- x[2] * state$stdStep
      }
    } else {
      bi <- state$bestIdx
      xs <- sort(x)
      # are we still on the left edge?
      if (x[bi] == xs[1]) {
        nextX <- x[bi] / state$stdStep
        if (!(x[bi] > .Machine$double.xmin)) cont <- FALSE
        # ...or on the right edge?
      } else if (x[bi] == xs[length(xs)]) {
        nextX <- x[bi] * state$stdStep
        if (!(x[bi] < Inf)) cont <- FALSE

        # maximum is bracketed, so do a quadratic fit in log-space
      } else {
        co <- quadrLogFit(x, y, ys)
        c0 <- co[1]
        c1 <- co[2]
        c2 <- co[3]
        maxVal <- y[bi]

        if (!is.finite(c2) || abs(c2) < .Machine$double.eps) {
          cont <- FALSE
          expected <- maxVal
          nextX <- x[bi]
        } else {
          logm <- -c1 / (2 * c2)
          expected <- c0 - c1^2 / (4 * c2)
          if (!is.finite(logm) || !is.finite(expected)) {
            cont <- FALSE
            expected <- maxVal
            nextX <- x[bi]
          } else {
            # stopping tests
            if (maxVal == 0) {
              cont <- FALSE
            } else if (((expected - maxVal) / abs(maxVal)) < state$stopByY) {
              cont <- FALSE
            } else if (abs(logm - log(x[bi])) < state$stopByX)  {
              cont <- FALSE
            }
            nextX <- exp(logm)
          }
        }
      }
    }
    list(continue = cont, nextX = nextX, expected = expected)
  }

  state$try <- function(xNew, yNew, sdNew = 0) {
    state$x <- c(state$x, xNew)
    state$y <- c(state$y, yNew)
    state$ySd <- c(state$ySd, sdNew)
    state$bestIdx <- which.max(state$y)
    invisible(NULL)
  }

  state$bestX <- function() {
    if (is.na(state$bestIdx)) {
      return(NULL)
    }
    state$x[state$bestIdx]
  }
  state$bestY <- function() {
    if (is.na(state$bestIdx)) {
      return(NULL)
    }
    state$y[state$bestIdx]
  }
  state
}

.finiteMean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  mean(x)
}

.finiteSd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= 1L) {
    return(0)
  }
  stats::sd(x)
}

.innerCvScoreFromEvaluation <- function(ev) {
  if (!"auc" %in% names(ev)) {
    return(NA_real_)
  }
  auc <- as.numeric(ev$auc)
  if (length(auc) == 0L || any(!is.finite(auc))) {
    return(NA_real_)
  }
  mean(auc)
}

.innerCvScoreMean <- function(scores) {
  scores <- as.numeric(scores)
  if (length(scores) == 0L || any(!is.finite(scores))) {
    return(NA_real_)
  }
  mean(scores)
}

.innerCvScoreSd <- function(scores) {
  scores <- as.numeric(scores)
  if (length(scores) == 0L || any(!is.finite(scores))) {
    return(NA_real_)
  }
  if (length(scores) == 1L) {
    return(0)
  }
  stats::sd(scores)
}

# Now a little wrapper that
#  - repeatedly calls step()
#  - runs your inner-CV at each new lambda
#  - calls try(lambda, mean_auc, sd_auc)
#  - returns final best lambda plus its CV mean
tuneLambda <- function(cl, algorithm, configBase, trainIds,
                        rounds, clientFrac, epsilon,
                        lambdaStrategy, lambdaDefault, totalPopSize,
                        globalMap,
                        stdStep = 2,
                        stopByY = 1e-2,
                        stopByX = log(1.5),
                        firstCut = 1.0,
                        verbose = TRUE) {
  context <- list(
    cl = cl,
    configBase = configBase,
    trainIds = trainIds,
    rounds = rounds,
    clientFrac = clientFrac,
    epsilon = epsilon,
    totalPopSize = totalPopSize,
    lambdaDefault = lambdaDefault,
    globalMap = globalMap
  )

  baseLambda <- lambdaDefault
  if (is.function(lambdaStrategy$seed)) {
    seedVal <- lambdaStrategy$seed(context)
    if (is.numeric(seedVal) && length(seedVal) > 0 && is.finite(seedVal[1]) && seedVal[1] > 0) {
      baseLambda <- seedVal[1]
    }
  }
  if (!is.numeric(baseLambda) || length(baseLambda) == 0 || !is.finite(baseLambda[1]) || baseLambda[1] <= 0) {
    stop("Unable to determine a positive starting lambda for tuning")
  }
  baseLambda <- as.numeric(baseLambda[1])

  initLambda <- lambdaStrategy$initial(baseLambda, totalPopSize, context)
  if (!is.numeric(initLambda) || length(initLambda) == 0 || !is.finite(initLambda[1]) || initLambda[1] <= 0) {
    stop("Lambda strategy produced a non-positive transformed lambda")
  }
  initLambda <- as.numeric(initLambda[1])

  search <- unimodalSearchInit(stdStep, stopByY, stopByX, firstCut, init = initLambda)
  useWarmStarts <- isTRUE(configBase$warmStartLambdaPath)
  warmState <- new.env(parent = emptyenv())

  fitValidationFold <- function(lambda, valId, iterLabel) {
    train2 <- setdiff(trainIds, valId)
    trainCluster <- subsetCluster(cl, train2)
    valCluster <- subsetCluster(cl, valId)
    key <- paste(valId, paste(train2, collapse = "-"), sep = ":")
    ws <- if (useWarmStarts && exists(key, envir = warmState, inherits = FALSE)) {
      get(key, envir = warmState, inherits = FALSE)
    } else {
      NULL
    }
    cfg <- c(
      configBase,
      list(
        lambda = lambda,
        rounds = rounds,
        epsilon = epsilon,
        clientFrac = clientFrac,
        mapping = globalMap,
        p = nrow(globalMap)
      )
    )
    if (!is.null(ws)) {
      cfg$initialZ <- ws$z
      cfg$roundOffset <- ws$roundOffset
    }
    if (verbose) {
      message(
        "Fitting on folds ", paste(train2, collapse = ""),
        " validating on fold ", valId,
        if (!is.null(ws)) sprintf(" (warm start roundOffset=%s)", ws$roundOffset) else ""
      )
    }
    res <- fitFederated(trainCluster, algorithm, cfg, verbose = verbose)
    if (useWarmStarts && !is.null(res$z)) {
      assign(
        key,
        list(
          z = res$z,
          roundOffset = (ws$roundOffset %||% 0L) + (res$roundsCompleted %||% rounds),
          lambda = lambda,
          iter = iterLabel
        ),
        envir = warmState
      )
    }
    clusterCreateMatrices(valCluster, res$config)
    ev <- clusterEvaluateModel(valCluster, res$w)
    .innerCvScoreFromEvaluation(ev)
  }

  aucs0 <- sapply(trainIds, function(valId) {
    fitValidationFold(initLambda, valId, iterLabel = 0L)
  })

  m <- .innerCvScoreMean(aucs0)
  s <- .innerCvScoreSd(aucs0)
  if (!is.finite(m)) {
    stop("Unable to compute a finite inner-CV AUC for the initial lambda")
  }

  if (verbose) {
    message(sprintf("[iter %2d] initial lambda = %.5g  (inner-CV AUC = %.5g)",
           0, initLambda, m)) 
  }
  search$try(initLambda, m, s)

  # continue with the search
  iter <- 1
  repeat {
    s <- search$step()
    if (!s$continue) {
      if (verbose) {
       message(sprintf("stopping search after %d iterations. best lambda = %g",
             iter - 1, search$bestX()))
      }
      break
    }
    lambdaTry <- s$nextX
    if (verbose) {
      message(sprintf("[iter %2d] proposing lambda = %.5g  (predicted auc = %.5g)",
             iter, lambdaTry, s$expected))
    }
    # your inner CV over trainIds
    aucs <- sapply(trainIds, function(valId) {
      fitValidationFold(lambdaTry, valId, iterLabel = iter)
    })

    m <- .innerCvScoreMean(aucs)
    s <- .innerCvScoreSd(aucs)
    if (!is.finite(m)) {
      stop("Unable to compute a finite inner-CV AUC for lambda ", lambdaTry)
    }
    if (verbose) {
      message(sprintf("[iter %2d] observed inner-CV AUC: mean = %.5g,  sd = %.5g",
             iter, m, s))
    }
    search$try(lambdaTry, m, s)
    iter <- iter + 1
  }

  bestLambda <- search$bestX()
  if (is.null(bestLambda) || !is.finite(bestLambda)) {
    bestLambda <- initLambda
  }
  bestLambda <- as.numeric(bestLambda)[1]
  bestPerf <- search$bestY()
  if (is.null(bestPerf)) {
    bestPerf <- m
  }
  list(
    bestLambda = lambdaStrategy$final(bestLambda, totalPopSize, context),
    bestLambdaTrain = bestLambda,
    perf = bestPerf
  )
}
