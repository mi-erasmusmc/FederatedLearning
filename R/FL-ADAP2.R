.adap2CacheEnv <- new.env(parent = emptyenv())

.serverInitADAP2 <- function(config) {
  p <- config[["p"]] + as.integer(isTRUE(config$intercept))
  hessian <- config$hessian %||% "full"
  maxFullHessianP <- config$maxFullHessianP %||% 250L
  if (identical(hessian, "full") && p > maxFullHessianP) {
    stop(sprintf(
      "ADAP2 full Hessian requested with p=%d, above maxFullHessianP=%d. Use featureSet='ageSex'/'ageSexPhenotypes', set hessian='diag', or raise maxFullHessianP explicitly.",
      p,
      maxFullHessianP
    ))
  }
  mode <- config$request %||% "none"
  cacheKey <- config$cacheKey
  state <- list(
    phase = 0L,
    p = p,
    leadIndex = NA_integer_,
    beta0 = rep(0, p),
    Gother = NULL,
    Hother = NULL,
    mode = mode,
    lambda = config$lambda %||% 1.0,
    foldsK = config$foldsK %||% 5L,
    hessian = hessian,
    leadPolicy = config$leadPolicy %||% "maxN",
    w = rep(0, p),
    cvMetric = NA_real_,
    cache = .adap2CacheEnv,
    surrogateKey = NULL,
    cacheKey = cacheKey
  )

  if (!is.null(cacheKey) && exists(cacheKey, envir = .adap2CacheEnv, inherits = FALSE)) {
    entry <- get(cacheKey, envir = .adap2CacheEnv, inherits = FALSE)
    state$phase <- 2L
    state$p <- entry$p
    state$leadIndex <- entry$leadIndex
    state$beta0 <- entry$beta0
    state$Gother <- entry$Gother
    state$Hother <- entry$Hother
    state$w <- entry$beta0
    state$surrogateKey <- cacheKey
    state$hessian <- entry$hessian
    state$foldsK <- config$foldsK %||% entry$foldsK %||% state$foldsK
  }

  state
}

.clientUpdateADAP2 <- function(clientData, serverBroadcast, config) {
  phase <- serverBroadcast$phase %||% 0L
  hessMode <- config$hessian %||% "full"

  if (phase == 0L) {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Please add 'glmnet' to DESCRIPTION Imports and install it.")
    }
    x <- clientData$xMatrix
    y <- clientData$yLabels
    penaltyFactor <- rep(1, ncol(x))
    if (isTRUE(config$intercept)) {
      penaltyFactor[1] <- 0
    }
    fit <- glmnet::cv.glmnet(
      x = x,
      y = y,
      family = "binomial",
      alpha = 1,
      intercept = FALSE,
      standardize = FALSE,
      penalty.factor = penaltyFactor
    )
    coefMat <- stats::coef(fit, s = "lambda.min")
    bLoc <- as.numeric(coefMat[-1, , drop = FALSE])
    return(list(bhat = bLoc, n = clientData$n))
  }

  if (phase == 1L) {
    beta0 <- serverBroadcast$beta0
    x <- clientData$xMatrix
    y <- clientData$yLabels
    n <- clientData$n
    lin <- as.numeric(x %*% beta0)
    pVec <- pmin(pmax(stats::plogis(lin), 1e-6), 1 - 1e-6)
    res <- pVec - y
    grad <- as.numeric(Matrix::crossprod(x, res)) / n
    wdiag <- pVec * (1 - pVec)

    if (identical(hessMode, "full")) {
      Hk <- as.matrix(Matrix::t(x) %*% Matrix::Diagonal(x = wdiag) %*% x) / n
      return(list(grad = grad, Hess = Hk, n = n))
    }
    Hdiag <- as.numeric(Matrix::colSums((x^2) * wdiag)) / n
    return(list(grad = grad, Hdiag = Hdiag, n = n))
  }

  if (phase == 2L) {
    localId <- getOption("FederatedLearning.localId", NA_integer_)
    leadIndex <- serverBroadcast$leadIndex
    if (!isTRUE(localId == leadIndex)) {
      return(NULL)
    }
    mode <- serverBroadcast$mode %||% "cv"
    beta0 <- serverBroadcast$beta0
    bvec <- serverBroadcast$Gother
    Hcorr <- serverBroadcast$Hother
    lambda <- serverBroadcast$lambda
    foldsK <- serverBroadcast$foldsK %||% 5L
    intercept <- isTRUE(config$intercept)
    maxIter <- config$maxIter %||% 100
    tol <- config$tol %||% 1e-6
    cvSeed <- config$cvSeed %||% 42L
    metric <- serverBroadcast$cvMetricType %||% config$cvMetric %||% "deviance"

    x <- clientData$xMatrix
    y <- clientData$yLabels

    if (identical(mode, "lambdaRange")) {
      range <- tryCatch(
        .leadLambdaRange(
          x = x,
          y = y,
          beta0 = beta0,
          b = bvec,
          Hcorr = Hcorr,
          intercept = intercept
        ),
        error = function(e) {
          stop(sprintf("ADAP2 lead lambda-range failure (lambda=%s): %s", format(lambda), conditionMessage(e)))
        }
      )
      return(range)
    }

    if (identical(mode, "cv")) {
      cvRes <- tryCatch(
        .leadSurrogateCV(
          x = x,
          y = y,
          beta0 = beta0,
          b = bvec,
          Hcorr = Hcorr,
          lambda = lambda,
          foldsK = foldsK,
          seed = cvSeed,
          intercept = intercept,
          maxIter = maxIter,
          tol = tol,
          metric = metric
        ),
        error = function(e) {
          stop(sprintf(
            "ADAP2 lead CV failure (lambda=%s, metric=%s): %s",
            format(lambda), metric, conditionMessage(e)
          ))
        }
      )
      return(list(cvMetric = cvRes$metric))
    }

    wHat <- tryCatch(
      .leadOptimizeSurrogate(
        x = x,
        y = y,
        beta0 = beta0,
        b = bvec,
        Hcorr = Hcorr,
        lambda = lambda,
        intercept = intercept,
        maxIter = maxIter,
        tol = tol
      ),
      error = function(e) {
        stop(sprintf("ADAP2 lead fit failure (lambda=%s): %s", format(lambda), conditionMessage(e)))
      }
    )
    return(list(w = wHat))
  }

  list()
}

.serverRoundADAP2 <- function(serverState, clientReports, config) {
  phase <- serverState$phase %||% 0L
  hessMode <- serverState$hessian %||% "full"
  serverState$lambda <- config$lambda %||% serverState$lambda
  serverState$mode <- serverState$mode %||% config$request %||% "none"
  cacheKey <- serverState$cacheKey

  if (phase == 0L) {
    bhats <- lapply(clientReports, `[[`, "bhat")
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    stopifnot(length(unique(vapply(bhats, length, 1L))) == 1L)
    p <- length(bhats[[1]])
    bmat <- do.call(cbind, bhats)
    leadPolicy <- serverState$leadPolicy %||% "maxN"
    leadIndex <- switch(leadPolicy,
      maxN = which.max(ns),
      first = 1L,
      {
        if (is.numeric(leadPolicy) && length(leadPolicy) == 1L) {
          as.integer(leadPolicy)
        } else {
          1L
        }
      }
    )
    beta0 <- as.numeric(bmat[, leadIndex])
    newState <- serverState
    newState$phase <- 1L
    newState$leadIndex <- leadIndex
    newState$beta0 <- beta0
    newState$w <- beta0
    newState$p <- p
    newState$foldsK <- config$foldsK %||% newState$foldsK
    return(list(state = newState, report = list(w = beta0, skipConvergence = TRUE)))
  }

  if (phase == 1L) {
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    N <- sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))
    weights <- ns / N
    Gsum <- as.numeric(grads %*% weights)
    leadIdx <- serverState$leadIndex
    gLead <- grads[, leadIdx]
    Gother <- Gsum - gLead

    if (identical(hessMode, "full")) {
      Hs <- lapply(clientReports, `[[`, "Hess")
      Hsum <- Reduce(`+`, Map(function(H, w) H * w, Hs, weights))
      Hlead <- Hs[[leadIdx]]
      Hother <- Hsum - Hlead
      hDiag <- diag(Hsum)
    } else {
      Hd <- do.call(cbind, lapply(clientReports, `[[`, "Hdiag"))
      Hsum <- as.numeric(Hd %*% weights)
      Hlead <- Hd[, leadIdx]
      Hother <- Hsum - Hlead
      hDiag <- Hsum
    }

    beta0 <- serverState$beta0
    key <- cacheKey %||% digest::digest(list(
      lead = leadIdx,
      beta0 = beta0,
      hessian = hessMode,
      nClients = length(ns)
    ))

    entry <- list(
      beta0 = beta0,
      leadIndex = leadIdx,
      Gother = Gother,
      Hother = Hother,
      hessian = hessMode,
      foldsK = serverState$foldsK,
      p = serverState$p
    )
    assign(key, entry, envir = .adap2CacheEnv)

    newState <- serverState
    newState$phase <- 2L
    newState$Gother <- Gother
    newState$Hother <- Hother
    newState$surrogateKey <- key
    newState$cacheKey <- key
    newState$lambda <- config$lambda %||% newState$lambda
    newState$mode <- config$request %||% newState$mode
    newState$foldsK <- config$foldsK %||% newState$foldsK
    hCond <- if (identical(hessMode, "full")) {
      tryCatch(kappa(Hsum), error = function(e) NA_real_)
    } else {
      NA_real_
    }
    newState$hessianDim <- if (identical(hessMode, "full")) paste(dim(Hsum), collapse = "x") else as.character(length(Hsum))
    newState$hessianDiagMin <- min(hDiag, na.rm = TRUE)
    newState$hessianDiagMax <- max(hDiag, na.rm = TRUE)
    newState$hessianCondition <- hCond
    return(list(
      state = newState,
      report = list(
        w = newState$w,
        leadIndex = leadIdx,
        skipConvergence = TRUE,
        hessianDim = newState$hessianDim,
        hessianDiagMin = newState$hessianDiagMin,
        hessianDiagMax = newState$hessianDiagMax,
        hessianCondition = newState$hessianCondition,
        communicationNumbers = length(Gsum) * length(clientReports) +
          length(Hsum) * length(clientReports)
      )
    ))
  }

  if (phase == 2L) {
    leadIdx <- serverState$leadIndex
    leadReport <- clientReports[[leadIdx]]
    if (identical(serverState$mode, "lambdaRange") && !is.null(leadReport$lambdaSeq)) {
      newState <- serverState
      newState$lambdaSeq <- leadReport$lambdaSeq
      newState$lambdaMax <- leadReport$lambdaMax
      newState$lambdaMin <- leadReport$lambdaMin
      report <- list(
        lambdaSeq = leadReport$lambdaSeq,
        lambdaMax = leadReport$lambdaMax,
        lambdaMin = leadReport$lambdaMin,
        cacheKey = newState$cacheKey,
        w = newState$w,
        done = TRUE
      )
      return(list(state = newState, report = report))
    }
    if (identical(serverState$mode, "cv") && !is.null(leadReport$cvMetric)) {
      newState <- serverState
      newState$cvMetric <- leadReport$cvMetric
      report <- list(cvMetric = leadReport$cvMetric, done = TRUE, leadIndex = leadIdx)
      return(list(state = newState, report = report))
    }
    if (identical(serverState$mode, "fit") && !is.null(leadReport$w)) {
      newState <- serverState
      newState$phase <- 3L
      newState$w <- leadReport$w
      report <- list(
        w = leadReport$w,
        done = TRUE,
        leadIndex = leadIdx,
        hessianDim = serverState$hessianDim %||% NA_character_,
        hessianDiagMin = serverState$hessianDiagMin %||% NA_real_,
        hessianDiagMax = serverState$hessianDiagMax %||% NA_real_,
        hessianCondition = serverState$hessianCondition %||% NA_real_
      )
      return(list(state = newState, report = report))
    }
  }

  list(state = serverState, report = list(w = serverState$w))
}

.leadOptimizeSurrogate <- function(x, y, beta0, b, Hcorr, lambda,
                                   intercept = TRUE, maxIter = 100, tol = 1e-6) {
  n <- length(y)
  p <- ncol(x)
  beta <- beta0
  soft <- function(v, t) ifelse(v > t, v - t, ifelse(v < -t, v + t, 0))
  penalize <- rep(TRUE, p)
  if (intercept) {
    penalize[1] <- FALSE
  }
  lin <- as.numeric(x %*% beta)
  isFull <- is.matrix(Hcorr) && all(dim(Hcorr) == c(p, p))
  if (!isFull) {
    HdiagCorr <- as.numeric(Hcorr)
  }

  for (iter in seq_len(maxIter)) {
    betaOld <- beta
    pVec <- pmin(pmax(stats::plogis(lin), 1e-6), 1 - 1e-6)
    wdiag <- pVec * (1 - pVec)
    glead <- as.numeric(Matrix::crossprod(x, (pVec - y))) / n
    hleadDiag <- as.numeric(Matrix::colSums((x^2) * wdiag)) / n

    for (j in seq_len(p)) {
      if (isFull) {
        gj <- glead[j] + b[j] + sum(Hcorr[j, ] * (beta - beta0))
        hjj <- hleadDiag[j] + Hcorr[j, j]
      } else {
        Hcorr_j <- HdiagCorr[j]
        if (!is.finite(Hcorr_j)) {
          Hcorr_j <- 0
        }
        gj <- glead[j] + b[j] + Hcorr_j * (beta[j] - beta0[j])
        hjj <- hleadDiag[j] + Hcorr_j
      }
      if (!is.finite(gj)) {
        gj <- 0
      }
      if (!is.finite(hjj) || hjj <= 0) {
        hjj <- 1e-10
      }
      z <- beta[j] - gj / hjj
      betaNew <- if (penalize[j]) soft(z, lambda / hjj) else z
      if (!is.finite(betaNew)) {
        betaNew <- beta[j]
      }
      if (!isTRUE(all.equal(betaNew, beta[j], tolerance = 0))) {
        lin <- lin + as.numeric(x[, j]) * (betaNew - beta[j])
        beta[j] <- betaNew
      }
    }
    delta <- max(abs(beta - betaOld), na.rm = TRUE)
    if (!is.finite(delta)) {
      delta <- Inf
    }
    if (delta < tol) {
      break
    }
  }
  beta
}

.leadSurrogateCV <- function(x, y, beta0, b, Hcorr, lambda,
                             foldsK = 5L, seed = 42L,
                             intercept = TRUE, maxIter = 100, tol = 1e-6,
                             metric = c("deviance", "auc")) {
  metric <- match.arg(metric)
  n <- length(y)
  set.seed(seed)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- numeric(foldsK)

  for (k in seq_len(foldsK)) {
    idxVal <- which(folds == k)
    idxTr <- which(folds != k)
    xtr <- x[idxTr, , drop = FALSE]
    ytr <- y[idxTr]
    xval <- x[idxVal, , drop = FALSE]
    yval <- y[idxVal]

    wHat <- .leadOptimizeSurrogate(
      x = xtr,
      y = ytr,
      beta0 = beta0,
      b = b,
      Hcorr = Hcorr,
      lambda = lambda,
      intercept = intercept,
      maxIter = maxIter,
      tol = tol
    )

    linVal <- as.numeric(xval %*% wHat)
    pVal <- pmin(pmax(stats::plogis(linVal), 1e-6), 1 - 1e-6)
    if (metric == "deviance") {
      scores[k] <- -mean(yval * log(pVal) + (1 - yval) * log(1 - pVal))
    } else {
      if (!requireNamespace("pROC", quietly = TRUE)) {
        stop("Please add 'pROC' to DESCRIPTION Imports and install it.")
      }
      if (length(unique(yval)) < 2) {
        aucVal <- 0.5
      } else {
        rocObj <- tryCatch(
          pROC::roc(response = yval, predictor = pVal, quiet = TRUE, direction = "<"),
          error = function(e) NULL
        )
        aucVal <- if (!is.null(rocObj) && is.finite(as.numeric(rocObj$auc))) {
          as.numeric(rocObj$auc)
        } else {
          0.5
        }
      }
      scores[k] <- aucVal
    }
  }

  list(metric = mean(scores))
}

.leadLambdaRange <- function(x, y, beta0, b, Hcorr, intercept = TRUE) {
  n <- length(y)
  p <- ncol(x)
  lin <- as.numeric(x %*% beta0)
  pVec <- pmin(pmax(stats::plogis(lin), 1e-6), 1 - 1e-6)
  res <- pVec - y
  gLead <- as.numeric(Matrix::crossprod(x, res)) / n
  penalize <- rep(TRUE, p)
  if (intercept) {
    penalize[1] <- FALSE
  }
  gTotal <- gLead + b
  lamVec <- abs(gTotal[penalize])
  lamVec <- lamVec[is.finite(lamVec)]
  lamMax <- if (length(lamVec) > 0) max(lamVec) else NA_real_
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1.0
  }
  frac <- if (n < p) 0.02 else 1e-4
  lamMin <- max(lamMax * frac, lamMax * 1e-6)
  gridLen <- 100L
  lambdaSeq <- exp(seq(log(lamMax), log(lamMin), length.out = gridLen))
  list(lambdaSeq = lambdaSeq, lambdaMax = lamMax, lambdaMin = lamMin)
}

.lambdaStrategyAdap2 <- function() {
  list(
    seed = function(context) NA_real_,
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )
}

.registerAlgorithm(
  "ADAP2",
  serverInit = .serverInitADAP2,
  clientInit = NULL,
  clientUpdate = .clientUpdateADAP2,
  serverRound = .serverRoundADAP2,
  lambdaStrategy = .lambdaStrategyAdap2()
)
