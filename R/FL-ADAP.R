# ADAP-baseline (two-round) federated lasso-logistic
# from: https://doi.org/10.1038/s41598-022-14029-9
#
# Round 0:
#   - Clients fit local lasso-logistic (glmnet) and return (bhat_i, n_i)
#   - Server computes bbar (sample-size-weighted average) and broadcasts
# Round 1:
#   - Clients compute grad_i(bbar) and diag(H)_i(bbar), averaged per-sample
#   - Server aggregates G, Hdiag and solves a diagonal-penalized quadratic.
.serverInitADAP <- function(config) {
  p <- config$p + as.integer(isTRUE(config$intercept))
  list(
    phase = 0L,
    bbar  = rep(0, p),
    w     = rep(0, p)
  )
}

.clientUpdateADAP <- function(clientData, serverBroadcast, config) {
  phase <- serverBroadcast$phase %||% 0L

  if (phase == 0L) {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Please add 'glmnet' to DESCRIPTION Imports and install it.")
    }
    x <- clientData$xMatrix
    y <- clientData$yLabels

    penalty_factor <- rep(1, ncol(x))
    if (isTRUE(config$intercept)) {
      penalty_factor[1] <- 0
    }

    fit <- glmnet::cv.glmnet(
      x = x,
      y = y,
      family = "binomial",
      alpha = 1,
      intercept = FALSE,
      standardize = FALSE,
      penalty.factor = penalty_factor
    )
    coef_mat <- stats::coef(fit, s = "lambda.min")
    b <- as.numeric(coef_mat[-1, , drop = FALSE])

    list(bhat = b, n = clientData$n)
  } else if (phase == 1L) {
    bbar <- serverBroadcast$bbar
    x <- clientData$xMatrix
    y <- clientData$yLabels
    n <- clientData$n

    lin  <- as.numeric(x %*% bbar)
    pvec <- stats::plogis(lin)
    res  <- pvec - y

    grad <- as.numeric(Matrix::crossprod(x, res)) / n

    wdiag <- pvec * (1 - pvec)
    hdiag <- as.numeric(Matrix::colSums((x^2) * wdiag)) / n

    list(grad = grad, hdiag = hdiag, n = n)
  } else {
    list()
  }
}

.serverRoundADAP <- function(serverState, clientReports, config) {
  phase <- serverState$phase %||% 0L

  if (phase == 0L) {
    # Aggregate local lasso inits
    bhats <- lapply(clientReports, `[[`, "bhat")
    ns    <- vapply(clientReports, `[[`, numeric(1), "n")
    stopifnot(length(unique(vapply(bhats, length, 1L))) == 1L)
    p <- length(bhats[[1]])

    bmat <- do.call(cbind, bhats)            # p x M
    wts  <- ns / sum(ns)
    bbar <- as.numeric(bmat %*% wts)

    newState <- list(
      phase = 1L,
      bbar  = bbar,
      w     = bbar # publish a usable vector
    )
    return(list(
      state  = newState,
      report = list(w = bbar) 
    ))
  }

  if (phase == 1L) {
    ns    <- vapply(clientReports, `[[`, numeric(1), "n")
    N     <- sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))   # p x M
    hdiags <- do.call(cbind, lapply(clientReports, `[[`, "hdiag")) # p x M

    G     <- as.numeric(grads %*% (ns / N))    # length p
    Hdiag <- as.numeric(hdiags %*% (ns / N))   # length p

    epsH  <- 1e-12
    Hdiag <- pmax(Hdiag, epsH)

    bbar <- serverState$bbar

    a <- G - Hdiag * bbar

    lambda <- config$lambda
    w <- -(a / Hdiag)

    soft <- function(v, t) {
      ifelse(v > t, v - t, ifelse(v < -t, v + t, 0))
    }
    penalize <- rep(TRUE, length(w))
    if (isTRUE(config$intercept)) {
      penalize[1] <- FALSE
      w[1] <- -(a[1] / Hdiag[1])
    }
    idx <- which(penalize)
    if (length(idx) > 0) {
      w[idx] <- soft(w[idx], lambda / Hdiag[idx])
    }

    newState <- list(
      phase = 2L,
      bbar  = bbar,
      w     = w
    )
    return(list(
      state  = newState,
      report = list(w = w)
    ))
  }

  list(
    state  = serverState,
    report = list(w = serverState$w)
  )
}

.lambdaStrategyAdap <- function() {
  list(
    seed = function(context) {
      cl <- context$cl
      cfg <- context$configBase
      if (!is.null(context$globalMap)) {
        globalMap <- context$globalMap
      } else {
        mapType <- cfg$mapType %||% "union"
        globalMap <- clusterCollectCovRefs(cl, type = mapType)
      }
      if (is.null(globalMap) || nrow(globalMap) == 0) {
        stop("Global map is empty; cannot seed lambda for ADAP")
      }
      if (is.null(cl) || length(cl) == 0) {
        return(NA_real_)
      }
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop("Please add 'glmnet' to DESCRIPTION Imports and install it.")
      }
      cfgPrep <- cfg
      cfgPrep$mapping <- globalMap
      cfgPrep$p <- nrow(globalMap)
      intercept <- isTRUE(cfgPrep$intercept)
      lambdaVals <- parallel::clusterCall(
        cl,
        function(cfg, intercept) {
          if (!requireNamespace("glmnet", quietly = TRUE)) {
            stop("Please add 'glmnet' to DESCRIPTION Imports and install it.")
          }
          clientData <- FederatedLearning::createClientMatrix(plpData, cfg)
          x <- clientData$xMatrix
          y <- clientData$yLabels
          penalty_factor <- rep(1, ncol(x))
          if (intercept) {
            penalty_factor[1] <- 0
          }
          fit <- glmnet::cv.glmnet(
            x = x,
            y = y,
            family = "binomial",
            alpha = 1,
            intercept = FALSE,
            standardize = FALSE,
            penalty.factor = penalty_factor
          )
          fit$lambda.1se
        },
        cfg = cfgPrep,
        intercept = intercept
      )
      lambdaVals <- unlist(lambdaVals)
      lambdaVals <- lambdaVals[is.finite(lambdaVals) & lambdaVals > 0]
      if (length(lambdaVals) == 0) {
        return(NA_real_)
      }
      max(lambdaVals)
    },
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )
}

.registerAlgorithm(
  "ADAP",
  serverInit   = .serverInitADAP,
  clientInit   = NULL,
  clientUpdate = .clientUpdateADAP,
  serverRound  = .serverRoundADAP,
  lambdaStrategy = .lambdaStrategyAdap()
)
