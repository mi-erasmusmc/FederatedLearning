# Fit y ≃ c-1 + c1*log(x) + c2*(log(x))^2 by weighted least‐squares
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

        # maximum is bracketed → do a quadratic‐fit in log‐space
      } else {
        co <- quadrLogFit(x, y, ys)
        c0 <- co[1]
        c1 <- co[2]
        c2 <- co[3]
        logm <- -c1 / (2 * c2)
        expected <- c0 - c1^2 / (4 * c2)
        maxVal <- y[bi]

        # stopping tests
        if (maxVal == 0) {
          cont <- FALSE
        } else if ((expected - maxVal) / abs(maxVal) < state$stopByY) {
          cont <- FALSE
        } else if (abs(logm - log(x[bi])) < state$stopByX)  {
        cont <- FALSE
        }
        nextX <- exp(logm)
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
  state
}

# Now a little wrapper that
#  • repeatedly calls step()
#  • runs your inner‐CV at each new lambda
#  • calls try(lambda, mean_auc, sd_auc)
#  • returns final best λ plus its CV‐mean
tuneLambda <- function(cl, algorithm, configBase, trainIds,
                        rounds, clientFrac, epsilon,
                        stdStep = 2,
                        stopByY = 1e-2,
                        stopByX = log(1.5),
                        firstCut = 1.0,
                        initLambda = 1.0,
                        verbose = TRUE) {
  search <- unimodalSearchInit(stdStep, stopByY, stopByX, firstCut, init = initLambda)

  # initial λ to seed the 
  cfg0 <- c(configBase, list(
    lambda = initLambda,
    rounds = rounds,
    epsilon = epsilon,
    clientFrac = clientFrac
  ))

  aucs0 <- sapply(trainIds, function(valId) {
    train2 <- setdiff(trainIds, valId)
    message("Fitting on folds ", train2, " validating on fold ", valId)
    res <- fitFederated(cl[train2], algorithm, cfg0)
    ev <- evaluateClient(cl[valId], res$w, config = res$config)
    ev
  })

  m <- mean(aucs0)
  s <- sd(aucs0)

  if (verbose) {
    message(sprintf("[iter %2d] initial λ = %.5g  (predicted auc = %.5g)",
           0, initLambda, m)) 
  }
  search$try(initLambda, m, s)

  # continue with the search
  iter <- 1
  repeat {
    s <- search$step()
    if (!s$continue) {
      if (verbose) {
       message(sprintf("→ stopping search after %d iterations. best lambda = %g",
             iter - 1, search$bestX()))
      }
      break
    }
    lambdaTry <- s$nextX
    if (verbose) {
      message(sprintf("[iter %2d] proposing λ = %.5g  (predicted auc = %.5g)",
             iter, lambdaTry, s$expected))
    }
    cfg <- c(
      configBase,
      list(
        lambda = lambdaTry,
        rounds = rounds,
        epsilon = epsilon,
        clientFrac = clientFrac
      )
    )

    # your inner CV over trainIds
    aucs <- sapply(trainIds, function(valId) {
      train2 <- setdiff(trainIds, valId)
      res <- fitFederated(cl[train2], algorithm, cfg)
      ev <- evaluateClient(cl[valId], res$w, res$config)
      ev
    })

    m <- mean(aucs)
    s <- sd(aucs)
    if (verbose) {
      message(sprintf("[iter %2d] observed auc: mean = %.5g,  sd = %.5g",
             iter, m, s))
    }
    search$try(lambdaTry, m, s)
    iter <- iter + 1
  }

  bestLambda <- search$bestX()
  list(bestLambda = bestLambda, perf = mean(aucs))
}
