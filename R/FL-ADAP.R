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
  p <- config[["p"]] + as.integer(isTRUE(config$intercept))
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

    if (!is.null(config$localLambda)) {
      fit <- glmnet::glmnet(
        x = x,
        y = y,
        family = "binomial",
        alpha = 1,
        lambda = config$localLambda,
        intercept = FALSE,
        standardize = FALSE,
        penalty.factor = penalty_factor
      )
      coef_mat <- stats::coef(fit, s = config$localLambda)
    } else {
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
    }
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
  "ADAPDiagClosedForm",
  serverInit   = .serverInitADAP,
  clientInit   = NULL,
  clientUpdate = .clientUpdateADAP,
  serverRound  = .serverRoundADAP,
  lambdaStrategy = .lambdaStrategyAdap()
)

.stripInterceptColumn <- function(x, config) {
  if (isTRUE(config$intercept)) {
    x[, -1, drop = FALSE]
  } else {
    x
  }
}

.addInterceptColumn <- function(x) {
  cbind(Matrix::Matrix(1, nrow(x), 1, sparse = TRUE), x)
}

.softScalar <- function(x, threshold) {
  if (x > threshold) {
    x - threshold
  } else if (x < -threshold) {
    x + threshold
  } else {
    0
  }
}

.logisticNegGradient <- function(beta, xDesign, y) {
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  as.numeric(Matrix::crossprod(xDesign, pVec - y)) / length(y)
}

.logisticNegHessian <- function(beta, xDesign) {
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  wDiag <- pVec * (1 - pVec)
  as.matrix(Matrix::crossprod(xDesign, Matrix::Diagonal(x = wDiag) %*% xDesign)) / nrow(xDesign)
}

.logisticNegHessianDiag <- function(beta, xDesign) {
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  wDiag <- pVec * (1 - pVec)
  as.numeric(Matrix::colSums((xDesign^2) * wDiag)) / nrow(xDesign)
}

.negLogLikMean <- function(beta, xDesign, y) {
  lin <- as.numeric(xDesign %*% beta)
  val <- binaryLogLoss(lin, y, meanLoss = TRUE)
  if (is.finite(val)) val else .Machine$double.xmax / 1e100
}

.fitPdaStyleLocalLasso <- function(xRaw, xDesign, y, config) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Please install glmnet for ADAP")
  }
  maxDenseCells <- config$maxDenseInitCells %||% 5e7
  useDensePdaPath <- isTRUE((nrow(xDesign) * ncol(xDesign)) <= maxDenseCells)

  if (useDensePdaPath) {
    xFit <- as.matrix(xDesign)
    penaltyFactor <- rep(1, ncol(xFit))
    penaltyFactor[1] <- 0
    fit <- glmnet::cv.glmnet(
      x = xFit,
      y = y,
      family = "binomial",
      alpha = 1,
      intercept = FALSE,
      standardize = config$standardize %||% FALSE,
      penalty.factor = penaltyFactor
    )
    beta <- as.numeric(stats::coef(fit, s = "lambda.min"))
    if (length(beta) == ncol(xFit) + 1L) {
      beta <- beta[-1]
    }
  } else {
    fit <- glmnet::cv.glmnet(
      x = xRaw,
      y = y,
      family = "binomial",
      alpha = 1,
      intercept = TRUE,
      standardize = config$standardize %||% FALSE
    )
    beta <- as.numeric(stats::coef(fit, s = "lambda.min"))
  }
  list(beta = beta, lambda = fit$lambda.min, usedDensePdaPath = useDensePdaPath)
}

.coordDescentQuadraticLasso <- function(aTilde, B, betaInit, lambda,
                                        maxIter = 100L, tol = 1e-5,
                                        penalize = NULL) {
  beta <- betaInit
  p <- length(beta)
  if (is.null(penalize)) {
    penalize <- rep(TRUE, p)
    penalize[1] <- FALSE
  }
  diagB <- diag(B)
  for (iter in seq_len(maxIter)) {
    betaOld <- beta
    for (j in seq_len(p)) {
      hjj <- diagB[j]
      if (!is.finite(hjj) || hjj <= 0) {
        hjj <- 1e-10
      }
      b <- aTilde[j] + sum(B[j, ] * beta) - hjj * beta[j]
      z <- -b / hjj
      beta[j] <- if (penalize[j]) .softScalar(z, lambda / hjj) else z
    }
    diffObj <- as.numeric(t(aTilde) %*% (beta - betaOld) +
      t(beta) %*% B %*% beta / 2 -
      t(betaOld) %*% B %*% betaOld / 2)
    if (is.finite(diffObj) && abs(diffObj) < tol) {
      break
    }
  }
  beta
}

.adapSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                     globalGrad, globalHess) {
  hEval <- .logisticNegHessian(betaEval, xDesign)
  hBar <- .logisticNegHessian(betaBar, xDesign)
  B <- hEval + globalHess - hBar
  aTilde <- .logisticNegGradient(betaEval, xDesign, y) -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    .logisticNegGradient(betaBar, xDesign, y) -
    as.numeric(t(betaBar) %*% (globalHess - hBar))
  list(aTilde = as.numeric(aTilde), B = B)
}

.adapFirstOrderSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                               globalGrad) {
  hEval <- .logisticNegHessian(betaEval, xDesign)
  deltaGrad <- globalGrad - .logisticNegGradient(betaBar, xDesign, y)
  aTilde <- .logisticNegGradient(betaEval, xDesign, y) -
    as.numeric(t(betaEval) %*% hEval) +
    deltaGrad
  list(aTilde = as.numeric(aTilde), B = hEval)
}

.adapDiagSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                         globalGrad, globalHessDiag,
                                         mode = c("second", "first")) {
  mode <- match.arg(mode)
  hEvalDiag <- .logisticNegHessianDiag(betaEval, xDesign)
  gradEval <- .logisticNegGradient(betaEval, xDesign, y)
  if (identical(mode, "first")) {
    aTilde <- gradEval - betaEval * hEvalDiag +
      globalGrad -
      .logisticNegGradient(betaBar, xDesign, y)
    Bdiag <- hEvalDiag
  } else {
    hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
    Bdiag <- hEvalDiag + globalHessDiag - hBarDiag
    aTilde <- gradEval - betaEval * hEvalDiag +
      globalGrad -
      .logisticNegGradient(betaBar, xDesign, y) -
      betaBar * (globalHessDiag - hBarDiag)
  }
  Bdiag[!is.finite(Bdiag) | Bdiag <= 0] <- 1e-10
  list(aTilde = as.numeric(aTilde), Bdiag = as.numeric(Bdiag))
}

.adapLocalFullRemoteDiagSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                                        globalGrad,
                                                        globalHessDiag) {
  hEval <- .logisticNegHessian(betaEval, xDesign)
  hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
  p <- length(betaEval)
  correctionDiag <- globalHessDiag - hBarDiag
  B <- hEval + diag(correctionDiag, p, p)
  aTilde <- .logisticNegGradient(betaEval, xDesign, y) -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    .logisticNegGradient(betaBar, xDesign, y) -
    betaBar * correctionDiag
  list(aTilde = as.numeric(aTilde), B = B)
}

.fitPdaAdapSurrogate <- function(xDesign, y, betaLead, betaBar,
                                 globalGrad, globalHess, lambda,
                                 maxOuter = 100L, maxInner = 100L,
                                 tol = 1e-5) {
  beta <- betaLead
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHess = globalHess
    )
    beta <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize
    )
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      break
    }
  }
  beta
}

.pdaAdapPdaLambdaSeq <- function(globalGrad, nLead, p, gridLen = 100L) {
  lamMax <- if (p > 1L) max(abs(globalGrad[-1]), na.rm = TRUE) else max(abs(globalGrad), na.rm = TRUE)
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1
  }
  lamMin <- lamMax * if (nLead < p) 0.02 else 1e-4
  exp(seq(log(lamMax), log(lamMin), length.out = gridLen))
}

.fitPdaAdapPdaProx <- function(xDesign, y, beta0, globalGrad, globalHess,
                               lambda, useFull = TRUE,
                               maxIter = 1000L, tol = 1e-6,
                               ridge = 1e-4) {
  beta <- beta0
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE

  gLead0 <- .logisticNegGradient(beta0, xDesign, y)
  if (isTRUE(useFull)) {
    hLead0 <- .logisticNegHessian(beta0, xDesign)
    hCorr <- globalHess - hLead0
  } else {
    hLead0 <- .logisticNegHessianDiag(beta0, xDesign)
    hCorr <- as.numeric(globalHess - hLead0)
  }

  bCorr <- globalGrad - gLead0
  for (iter in seq_len(maxIter)) {
    betaOld <- beta
    gLocal <- .logisticNegGradient(beta, xDesign, y)
    hLocalDiag <- .logisticNegHessianDiag(beta, xDesign)
    if (isTRUE(useFull)) {
      gTilde <- gLocal + bCorr + as.numeric(hCorr %*% (beta - beta0))
      hDiag <- hLocalDiag + diag(hCorr)
    } else {
      gTilde <- gLocal + bCorr + hCorr * (beta - beta0)
      hDiag <- hLocalDiag + hCorr
    }
    hDiag <- pmax(hDiag, ridge)
    for (j in seq_along(beta)) {
      z <- beta[j] - gTilde[j] / hDiag[j]
      beta[j] <- if (penalize[j]) .softScalar(z, lambda / hDiag[j]) else z
    }
    delta <- max(abs(beta - betaOld), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      break
    }
  }
  beta
}

.pdaAdapPdaLeadCv <- function(xDesign, y, betaBar, globalGrad, globalHess,
                              lambdaSeq, useFull = TRUE,
                              foldsK = 5L, seed = 42L,
                              maxIter = 1000L, tol = 1e-6,
                              ridge = 1e-4) {
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  for (li in seq_along(lambdaSeq)) {
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      idxVal <- which(folds == fold)
      idxTr <- setdiff(seq_len(n), idxVal)
      fit <- .fitPdaAdapPdaProx(
        xDesign = xDesign[idxTr, , drop = FALSE],
        y = y[idxTr],
        beta0 = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHess,
        lambda = lambdaSeq[li],
        useFull = useFull,
        maxIter = maxIter,
        tol = tol,
        ridge = ridge
      )
      foldLoss[fold] <- .negLogLikMean(fit, xDesign[idxVal, , drop = FALSE], y[idxVal])
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- which.min(scores)
  list(lambda = lambdaSeq[idx], scores = scores)
}

.fitPdaAdapRemoteDiagSurrogate <- function(xDesign, y, betaLead, betaBar,
                                           globalGrad, globalHessDiag,
                                           lambda,
                                           maxOuter = 100L, maxInner = 100L,
                                           tol = 1e-5) {
  beta <- betaLead
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag
    )
    beta <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize
    )
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      break
    }
  }
  beta
}

.fitPdaAdapFirstOrderSurrogate <- function(xDesign, y, betaLead, betaBar,
                                           globalGrad, lambda,
                                           maxOuter = 100L, maxInner = 100L,
                                           tol = 1e-5) {
  beta <- betaLead
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapFirstOrderSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad
    )
    beta <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize
    )
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      break
    }
  }
  beta
}

.fitDiagQuadraticLasso <- function(aTilde, Bdiag, betaInit, lambda,
                                   penalize = NULL) {
  if (is.null(penalize)) {
    penalize <- rep(TRUE, length(betaInit))
    penalize[1] <- FALSE
  }
  Bdiag[!is.finite(Bdiag) | Bdiag <= 0] <- 1e-10
  beta <- -aTilde / Bdiag
  idx <- which(penalize)
  if (length(idx) > 0) {
    beta[idx] <- vapply(
      idx,
      function(j) .softScalar(beta[j], lambda / Bdiag[j]),
      numeric(1)
    )
  }
  beta
}

.fitPdaAdapDiagSurrogate <- function(xDesign, y, betaLead, betaBar,
                                     globalGrad, globalHessDiag = NULL,
                                     lambda,
                                     mode = c("second", "first"),
                                     maxOuter = 100L, tol = 1e-5) {
  mode <- match.arg(mode)
  beta <- betaLead
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapDiagSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      mode = mode
    )
    beta <- .fitDiagQuadraticLasso(
      aTilde = comp$aTilde,
      Bdiag = comp$Bdiag,
      betaInit = beta,
      lambda = lambda,
      penalize = penalize
    )
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (is.finite(delta) && delta < tol) {
      break
    }
  }
  beta
}

.pdaAdapLambdaSeq <- function(xDesign, y, betaLead, betaBar, globalGrad, globalHess,
                              gridLen = 100L) {
  comp <- .adapSurrogateComponents(
    betaEval = betaLead,
    betaBar = betaBar,
    xDesign = xDesign,
    y = y,
    globalGrad = globalGrad,
    globalHess = globalHess
  )
  p <- length(betaLead)
  offDiag <- comp$B - diag(diag(comp$B), p, p)
  lamMax <- max(abs(comp$aTilde[-1] + as.numeric((offDiag %*% betaBar)[-1])), na.rm = TRUE)
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1
  }
  lamMin <- if (nrow(xDesign) < ncol(xDesign)) 0.02 * lamMax else 1e-4 * lamMax
  rev(exp(seq(log(lamMin), log(lamMax), length.out = gridLen)))
}

.pdaAdapLeadCv <- function(xDesign, y, betaLead, betaBar,
                           globalGrad, globalHess, lambdaSeq,
                           totalN,
                           foldsK = 5L, seed = 42L,
                           maxOuter = 100L, maxInner = 100L,
                           tol = 1e-5) {
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  for (li in seq_along(lambdaSeq)) {
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      idxVal <- which(folds == fold)
      idxTr <- which(folds != fold)
      nVal <- length(idxVal)
      gradVal <- .logisticNegGradient(betaBar, xDesign[idxVal, , drop = FALSE], y[idxVal])
      hessVal <- .logisticNegHessian(betaBar, xDesign[idxVal, , drop = FALSE])
      denom <- max(totalN - nVal, 1L)
      gradTrainGlobal <- (globalGrad * totalN - gradVal * nVal) / denom
      hessTrainGlobal <- (globalHess * totalN - hessVal * nVal) / denom
      fit <- .fitPdaAdapSurrogate(
        xDesign = xDesign[idxTr, , drop = FALSE],
        y = y[idxTr],
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = gradTrainGlobal,
        globalHess = hessTrainGlobal,
        lambda = lambdaSeq[li],
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol
      )
      foldLoss[fold] <- .negLogLikMean(fit, xDesign[idxVal, , drop = FALSE], y[idxVal])
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- which.min(scores)
  list(lambda = lambdaSeq[idx], scores = scores)
}

.pdaAdapFirstLambdaSeq <- function(xDesign, y, betaLead, betaBar, globalGrad,
                                   gridLen = 100L) {
  comp <- .adapFirstOrderSurrogateComponents(
    betaEval = betaLead,
    betaBar = betaBar,
    xDesign = xDesign,
    y = y,
    globalGrad = globalGrad
  )
  p <- length(betaLead)
  offDiag <- comp$B - diag(diag(comp$B), p, p)
  lamMax <- max(abs(comp$aTilde[-1] + as.numeric((offDiag %*% betaBar)[-1])), na.rm = TRUE)
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1
  }
  lamMin <- if (nrow(xDesign) < ncol(xDesign)) 0.02 * lamMax else 1e-4 * lamMax
  rev(exp(seq(log(lamMin), log(lamMax), length.out = gridLen)))
}

.pdaAdapFirstLeadCv <- function(xDesign, y, betaLead, betaBar,
                                globalGrad, lambdaSeq, totalN,
                                foldsK = 5L, seed = 42L,
                                maxOuter = 100L, maxInner = 100L,
                                tol = 1e-5) {
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  for (li in seq_along(lambdaSeq)) {
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      idxVal <- which(folds == fold)
      idxTr <- which(folds != fold)
      nVal <- length(idxVal)
      gradVal <- .logisticNegGradient(betaBar, xDesign[idxVal, , drop = FALSE], y[idxVal])
      denom <- max(totalN - nVal, 1L)
      gradTrainGlobal <- (globalGrad * totalN - gradVal * nVal) / denom
      fit <- .fitPdaAdapFirstOrderSurrogate(
        xDesign = xDesign[idxTr, , drop = FALSE],
        y = y[idxTr],
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = gradTrainGlobal,
        lambda = lambdaSeq[li],
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol
      )
      foldLoss[fold] <- .negLogLikMean(fit, xDesign[idxVal, , drop = FALSE], y[idxVal])
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- which.min(scores)
  list(lambda = lambdaSeq[idx], scores = scores)
}

.pdaAdapDiagLambdaSeq <- function(xDesign, y, betaLead, betaBar,
                                  globalGrad, globalHessDiag = NULL,
                                  mode = c("second", "first"),
                                  gridLen = 100L) {
  mode <- match.arg(mode)
  if (identical(mode, "second")) {
    comp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = betaLead,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag
    )
    p <- length(betaLead)
    offDiag <- comp$B - diag(diag(comp$B), p, p)
    lamMax <- max(abs(comp$aTilde[-1] + as.numeric((offDiag %*% betaBar)[-1])), na.rm = TRUE)
  } else {
    comp <- .adapDiagSurrogateComponents(
      betaEval = betaLead,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      mode = mode
    )
    lamMax <- max(abs(comp$aTilde[-1]), na.rm = TRUE)
  }
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1
  }
  lamMin <- if (nrow(xDesign) < ncol(xDesign)) 0.02 * lamMax else 1e-4 * lamMax
  rev(exp(seq(log(lamMin), log(lamMax), length.out = gridLen)))
}

.pdaAdapDiagLeadCv <- function(xDesign, y, betaLead, betaBar,
                               globalGrad, globalHessDiag = NULL,
                               lambdaSeq, totalN,
                               mode = c("second", "first"),
                               foldsK = 5L, seed = 42L,
                               maxOuter = 100L, tol = 1e-5) {
  mode <- match.arg(mode)
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  for (li in seq_along(lambdaSeq)) {
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      idxVal <- which(folds == fold)
      idxTr <- which(folds != fold)
      nVal <- length(idxVal)
      gradVal <- .logisticNegGradient(betaBar, xDesign[idxVal, , drop = FALSE], y[idxVal])
      denom <- max(totalN - nVal, 1L)
      gradTrainGlobal <- (globalGrad * totalN - gradVal * nVal) / denom
      hessTrainGlobalDiag <- NULL
      if (identical(mode, "second")) {
        hessValDiag <- .logisticNegHessianDiag(betaBar, xDesign[idxVal, , drop = FALSE])
        hessTrainGlobalDiag <- (globalHessDiag * totalN - hessValDiag * nVal) / denom
        fit <- .fitPdaAdapRemoteDiagSurrogate(
          xDesign = xDesign[idxTr, , drop = FALSE],
          y = y[idxTr],
          betaLead = betaLead,
          betaBar = betaBar,
          globalGrad = gradTrainGlobal,
          globalHessDiag = hessTrainGlobalDiag,
          lambda = lambdaSeq[li],
          maxOuter = maxOuter,
          tol = tol
        )
      } else {
        fit <- .fitPdaAdapDiagSurrogate(
          xDesign = xDesign[idxTr, , drop = FALSE],
          y = y[idxTr],
          betaLead = betaLead,
          betaBar = betaBar,
          globalGrad = gradTrainGlobal,
          globalHessDiag = hessTrainGlobalDiag,
          lambda = lambdaSeq[li],
          mode = mode,
          maxOuter = maxOuter,
          tol = tol
        )
      }
      foldLoss[fold] <- .negLogLikMean(fit, xDesign[idxVal, , drop = FALSE], y[idxVal])
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- which.min(scores)
  list(lambda = lambdaSeq[idx], scores = scores)
}

.serverInitPdaAdap <- function(config) {
  p <- config[["p"]] + 1L
  list(
    phase = 0L,
    p = p,
    adapSolveStyle = "fullQuadratic",
    betaBar = rep(0, p),
    betaLead = rep(0, p),
    leadIndex = NA_integer_,
    globalGrad = NULL,
    globalHess = NULL,
    lambdaSeq = NULL,
    selectedLambda = config$lambda %||% NA_real_,
    w = rep(0, p)
  )
}

.serverInitPdaAdapPda <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapSolveStyle <- "pda"
  state
}

.clientUpdatePdaAdap <- function(clientData, serverBroadcast, config) {
  phase <- serverBroadcast$phase %||% 0L
  xRaw <- .stripInterceptColumn(clientData$xMatrix, config)
  xDesign <- .addInterceptColumn(xRaw)
  y <- clientData$yLabels

  if (phase == 0L) {
    init <- .fitPdaStyleLocalLasso(xRaw, xDesign, y, config)
    return(list(bhat = init$beta, n = clientData$n, lambdaLocal = init$lambda))
  }

  if (phase == 1L) {
    betaBar <- serverBroadcast$betaBar
    return(list(
      grad = .logisticNegGradient(betaBar, xDesign, y),
      Hess = .logisticNegHessian(betaBar, xDesign),
      n = clientData$n
    ))
  }

  if (phase == 2L) {
    localId <- getOption("FederatedLearning.localId", NA_integer_)
    if (!isTRUE(localId == serverBroadcast$leadIndex)) {
      return(NULL)
    }
    betaBar <- serverBroadcast$betaBar
    betaLead <- serverBroadcast$betaLead
    globalGrad <- serverBroadcast$globalGrad
    globalHess <- serverBroadcast$globalHess
    lambdaSeq <- serverBroadcast$lambdaSeq
    solveStyle <- serverBroadcast$adapSolveStyle %||% config$adapSolveStyle %||% "fullQuadratic"
    if (identical(solveStyle, "pda")) {
      if (is.null(lambdaSeq)) {
        lambdaSeq <- .pdaAdapPdaLambdaSeq(
          globalGrad = globalGrad,
          nLead = length(y),
          p = length(betaBar),
          gridLen = config$lambdaGridLen %||% 100L
        )
      }
      cv <- .pdaAdapPdaLeadCv(
        xDesign = xDesign,
        y = y,
        betaBar = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHess,
        lambdaSeq = lambdaSeq,
        useFull = TRUE,
        foldsK = config$foldsK %||% 5L,
        seed = config$cvSeed %||% 42L,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4
      )
      lambda <- config$lambda %||% cv$lambda
      w <- .fitPdaAdapPdaProx(
        xDesign = xDesign,
        y = y,
        beta0 = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHess,
        lambda = lambda,
        useFull = TRUE,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4
      )
      return(list(w = w, selectedLambda = lambda, lambdaSeq = lambdaSeq, cvScores = cv$scores))
    }
    if (is.null(lambdaSeq)) {
      lambdaSeq <- .pdaAdapLambdaSeq(
        xDesign,
        y,
        betaLead,
        betaBar,
        globalGrad,
        globalHess,
        gridLen = config$lambdaGridLen %||% 100L
      )
    }
    cv <- .pdaAdapLeadCv(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = globalHess,
      lambdaSeq = lambdaSeq,
      totalN = serverBroadcast$totalN,
      foldsK = config$foldsK %||% 5L,
      seed = config$cvSeed %||% 42L,
      maxOuter = config$maxOuter %||% 100L,
      maxInner = config$maxInner %||% 100L,
      tol = config$tol %||% 1e-5
    )
    lambda <- config$lambda %||% cv$lambda
    w <- .fitPdaAdapSurrogate(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = globalHess,
      lambda = lambda,
      maxOuter = config$maxOuter %||% 100L,
      maxInner = config$maxInner %||% 100L,
      tol = config$tol %||% 1e-5
    )
    return(list(w = w, selectedLambda = lambda, lambdaSeq = lambdaSeq, cvScores = cv$scores))
  }

  list()
}

.serverRoundPdaAdap <- function(serverState, clientReports, config) {
  phase <- serverState$phase %||% 0L
  if (phase == 0L) {
    bhats <- lapply(clientReports, `[[`, "bhat")
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    bmat <- do.call(cbind, bhats)
    weights <- ns / sum(ns)
    betaBar <- as.numeric(bmat %*% weights)
    leadIndex <- config$leadIndex %||% which.max(ns)
    betaLead <- as.numeric(bmat[, leadIndex])
    state <- serverState
    state$phase <- 1L
    state$betaBar <- betaBar
    state$betaLead <- betaLead
    state$leadIndex <- leadIndex
    state$totalN <- sum(ns)
    state$w <- betaBar
    return(list(
      state = state,
      report = list(
        w = betaBar,
        leadIndex = leadIndex,
        skipConvergence = TRUE,
        communicationNumbers = length(betaBar) * length(clientReports)
      )
    ))
  }

  if (phase == 1L) {
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    weights <- ns / sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))
    hessList <- lapply(clientReports, `[[`, "Hess")
    globalGrad <- as.numeric(grads %*% weights)
    globalHess <- Reduce(`+`, Map(function(H, w) H * w, hessList, weights))
    state <- serverState
    state$phase <- 2L
    state$globalGrad <- globalGrad
    state$globalHess <- globalHess
    state$lambdaSeq <- config$lambdaSeq
    hDiag <- diag(globalHess)
    hCond <- tryCatch(kappa(globalHess), error = function(e) NA_real_)
    state$hessianDim <- paste(dim(globalHess), collapse = "x")
    state$hessianDiagMin <- min(hDiag, na.rm = TRUE)
    state$hessianDiagMax <- max(hDiag, na.rm = TRUE)
    state$hessianCondition <- hCond
    return(list(
      state = state,
      report = list(
        w = state$w,
        leadIndex = state$leadIndex,
        skipConvergence = TRUE,
        hessianDim = state$hessianDim,
        hessianDiagMin = state$hessianDiagMin,
        hessianDiagMax = state$hessianDiagMax,
        hessianCondition = state$hessianCondition,
        communicationNumbers = length(globalGrad) * length(clientReports) +
          length(globalHess) * length(clientReports)
      )
    ))
  }

  if (phase == 2L) {
    leadReport <- clientReports[[serverState$leadIndex]]
    if (!is.null(leadReport$w)) {
      state <- serverState
      state$phase <- 3L
      state$w <- leadReport$w
      state$selectedLambda <- leadReport$selectedLambda
      state$lambdaSeq <- leadReport$lambdaSeq
      return(list(
        state = state,
        report = list(
          w = leadReport$w,
          done = TRUE,
          leadIndex = state$leadIndex,
          selectedLambda = leadReport$selectedLambda,
          lambdaSeq = leadReport$lambdaSeq,
          cvScores = leadReport$cvScores,
          hessianDim = state$hessianDim %||% NA_character_,
          hessianDiagMin = state$hessianDiagMin %||% NA_real_,
          hessianDiagMax = state$hessianDiagMax %||% NA_real_,
          hessianCondition = state$hessianCondition %||% NA_real_,
          communicationNumbers = length(leadReport$w)
        )
      ))
    }
  }

  list(state = serverState, report = list(w = serverState$w))
}

.lambdaStrategyPdaAdap <- function() {
  list(
    seed = function(context) NA_real_,
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )
}

.registerAlgorithm(
  "ADAP",
  serverInit = .serverInitPdaAdap,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdap,
  serverRound = .serverRoundPdaAdap,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.registerAlgorithm(
  "ADAP_PDA",
  serverInit = .serverInitPdaAdapPda,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdap,
  serverRound = .serverRoundPdaAdap,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.serverInitPdaAdapReduced <- function(config, mode = c("first", "diag")) {
  mode <- match.arg(mode)
  p <- config[["p"]] + 1L
  list(
    phase = 0L,
    p = p,
    adapReducedMode = mode,
    adapDiagStyle = config$adapDiagStyle %||% "localFullRemoteDiag",
    betaBar = rep(0, p),
    betaLead = rep(0, p),
    leadIndex = NA_integer_,
    totalN = NA_real_,
    globalGrad = NULL,
    globalHessDiag = NULL,
    lambdaSeq = NULL,
    selectedLambda = config$lambda %||% NA_real_,
    w = rep(0, p)
  )
}

.serverInitPdaAdap1 <- function(config) {
  .serverInitPdaAdapReduced(config, mode = "first")
}

.serverInitPdaAdapDiag <- function(config) {
  .serverInitPdaAdapReduced(config, mode = "diag")
}

.clientUpdatePdaAdapReduced <- function(clientData, serverBroadcast, config) {
  phase <- serverBroadcast$phase %||% 0L
  mode <- serverBroadcast$adapReducedMode %||% config$adapReducedMode %||% "diag"
  diagStyle <- serverBroadcast$adapDiagStyle %||% config$adapDiagStyle %||% "localFullRemoteDiag"
  xRaw <- .stripInterceptColumn(clientData$xMatrix, config)
  xDesign <- .addInterceptColumn(xRaw)
  y <- clientData$yLabels

  if (phase == 0L) {
    init <- .fitPdaStyleLocalLasso(xRaw, xDesign, y, config)
    return(list(bhat = init$beta, n = clientData$n, lambdaLocal = init$lambda))
  }

  if (phase == 1L) {
    betaBar <- serverBroadcast$betaBar
    out <- list(
      grad = .logisticNegGradient(betaBar, xDesign, y),
      n = clientData$n
    )
    if (identical(mode, "diag")) {
      out$HessDiag <- .logisticNegHessianDiag(betaBar, xDesign)
    }
    return(out)
  }

  if (phase == 2L) {
    localId <- getOption("FederatedLearning.localId", NA_integer_)
    if (!isTRUE(localId == serverBroadcast$leadIndex)) {
      return(NULL)
    }
    betaBar <- serverBroadcast$betaBar
    betaLead <- serverBroadcast$betaLead
    globalGrad <- serverBroadcast$globalGrad
    globalHessDiag <- serverBroadcast$globalHessDiag
    lambdaSeq <- serverBroadcast$lambdaSeq
    if (identical(mode, "first")) {
      if (is.null(lambdaSeq)) {
        lambdaSeq <- .pdaAdapFirstLambdaSeq(
          xDesign = xDesign,
          y = y,
          betaLead = betaLead,
          betaBar = betaBar,
          globalGrad = globalGrad,
          gridLen = config$lambdaGridLen %||% 100L
        )
      }
      cv <- .pdaAdapFirstLeadCv(
        xDesign = xDesign,
        y = y,
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        lambdaSeq = lambdaSeq,
        totalN = serverBroadcast$totalN,
        foldsK = config$foldsK %||% 5L,
        seed = config$cvSeed %||% 42L,
        maxOuter = config$maxOuter %||% 100L,
        maxInner = config$maxInner %||% 100L,
        tol = config$tol %||% 1e-5
      )
      lambda <- config$lambda %||% cv$lambda
      w <- .fitPdaAdapFirstOrderSurrogate(
        xDesign = xDesign,
        y = y,
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        lambda = lambda,
        maxOuter = config$maxOuter %||% 100L,
        maxInner = config$maxInner %||% 100L,
        tol = config$tol %||% 1e-5
      )
      return(list(w = w, selectedLambda = lambda, lambdaSeq = lambdaSeq, cvScores = cv$scores))
    }

    if (identical(diagStyle, "pda")) {
      if (is.null(lambdaSeq)) {
        lambdaSeq <- .pdaAdapPdaLambdaSeq(
          globalGrad = globalGrad,
          nLead = length(y),
          p = length(betaBar),
          gridLen = config$lambdaGridLen %||% 100L
        )
      }
      cv <- .pdaAdapPdaLeadCv(
        xDesign = xDesign,
        y = y,
        betaBar = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHessDiag,
        lambdaSeq = lambdaSeq,
        useFull = FALSE,
        foldsK = config$foldsK %||% 5L,
        seed = config$cvSeed %||% 42L,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4
      )
      lambda <- config$lambda %||% cv$lambda
      w <- .fitPdaAdapPdaProx(
        xDesign = xDesign,
        y = y,
        beta0 = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHessDiag,
        lambda = lambda,
        useFull = FALSE,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4
      )
      return(list(w = w, selectedLambda = lambda, lambdaSeq = lambdaSeq, cvScores = cv$scores))
    }

    if (is.null(lambdaSeq)) {
      lambdaSeq <- .pdaAdapDiagLambdaSeq(
        xDesign = xDesign,
        y = y,
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        globalHessDiag = globalHessDiag,
        mode = "second",
        gridLen = config$lambdaGridLen %||% 100L
      )
    }
    cv <- .pdaAdapDiagLeadCv(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      lambdaSeq = lambdaSeq,
      totalN = serverBroadcast$totalN,
      mode = "second",
      foldsK = config$foldsK %||% 5L,
      seed = config$cvSeed %||% 42L,
      maxOuter = config$maxOuter %||% 100L,
      tol = config$tol %||% 1e-5
    )
    lambda <- config$lambda %||% cv$lambda
    w <- .fitPdaAdapRemoteDiagSurrogate(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      lambda = lambda,
      maxOuter = config$maxOuter %||% 100L,
      maxInner = config$maxInner %||% 100L,
      tol = config$tol %||% 1e-5
    )
    return(list(w = w, selectedLambda = lambda, lambdaSeq = lambdaSeq, cvScores = cv$scores))
  }

  list()
}

.serverRoundPdaAdapReduced <- function(serverState, clientReports, config) {
  phase <- serverState$phase %||% 0L
  mode <- serverState$adapReducedMode %||% config$adapReducedMode %||% "diag"

  if (phase == 0L) {
    bhats <- lapply(clientReports, `[[`, "bhat")
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    bmat <- do.call(cbind, bhats)
    weights <- ns / sum(ns)
    betaBar <- as.numeric(bmat %*% weights)
    leadIndex <- config$leadIndex %||% which.max(ns)
    betaLead <- as.numeric(bmat[, leadIndex])
    state <- serverState
    state$phase <- 1L
    state$betaBar <- betaBar
    state$betaLead <- betaLead
    state$leadIndex <- leadIndex
    state$totalN <- sum(ns)
    state$w <- betaBar
    return(list(
      state = state,
      report = list(
        w = betaBar,
        leadIndex = leadIndex,
        skipConvergence = TRUE,
        communicationNumbers = length(betaBar) * length(clientReports)
      )
    ))
  }

  if (phase == 1L) {
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    weights <- ns / sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))
    globalGrad <- as.numeric(grads %*% weights)
    state <- serverState
    state$phase <- 2L
    state$globalGrad <- globalGrad
    state$lambdaSeq <- config$lambdaSeq
    hessianDim <- NA_character_
    hessianDiagMin <- NA_real_
    hessianDiagMax <- NA_real_
    communicationNumbers <- length(globalGrad) * length(clientReports)
    if (identical(mode, "diag")) {
      hessDiags <- do.call(cbind, lapply(clientReports, `[[`, "HessDiag"))
      globalHessDiag <- as.numeric(hessDiags %*% weights)
      state$globalHessDiag <- globalHessDiag
      hessianDim <- paste0(length(globalHessDiag), " diagonal")
      hessianDiagMin <- min(globalHessDiag, na.rm = TRUE)
      hessianDiagMax <- max(globalHessDiag, na.rm = TRUE)
      communicationNumbers <- communicationNumbers + length(globalHessDiag) * length(clientReports)
    }
    return(list(
      state = state,
      report = list(
        w = state$w,
        leadIndex = state$leadIndex,
        skipConvergence = TRUE,
        hessianDim = hessianDim,
        hessianDiagMin = hessianDiagMin,
        hessianDiagMax = hessianDiagMax,
        hessianCondition = NA_real_,
        communicationNumbers = communicationNumbers
      )
    ))
  }

  if (phase == 2L) {
    leadReport <- clientReports[[serverState$leadIndex]]
    if (!is.null(leadReport$w)) {
      state <- serverState
      state$phase <- 3L
      state$w <- leadReport$w
      state$selectedLambda <- leadReport$selectedLambda
      state$lambdaSeq <- leadReport$lambdaSeq
      return(list(
        state = state,
        report = list(
          w = leadReport$w,
          done = TRUE,
          leadIndex = state$leadIndex,
          selectedLambda = leadReport$selectedLambda,
          lambdaSeq = leadReport$lambdaSeq,
          cvScores = leadReport$cvScores,
          hessianDim = if (identical(mode, "diag")) paste0(length(state$globalHessDiag), " diagonal") else NA_character_,
          hessianDiagMin = if (identical(mode, "diag")) min(state$globalHessDiag, na.rm = TRUE) else NA_real_,
          hessianDiagMax = if (identical(mode, "diag")) max(state$globalHessDiag, na.rm = TRUE) else NA_real_,
          hessianCondition = NA_real_,
          communicationNumbers = length(leadReport$w)
        )
      ))
    }
  }

  list(state = serverState, report = list(w = serverState$w))
}

.registerAlgorithm(
  "ADAP1",
  serverInit = .serverInitPdaAdap1,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdapReduced,
  serverRound = .serverRoundPdaAdapReduced,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.registerAlgorithm(
  "ADAPDiag",
  serverInit = .serverInitPdaAdapDiag,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdapReduced,
  serverRound = .serverRoundPdaAdapReduced,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)
