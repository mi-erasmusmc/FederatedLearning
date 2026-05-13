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

.fixedAdapLambda <- function(config) {
  lambda <- config[["lambda", exact = TRUE]]
  if (is.null(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda < 0) {
    return(NULL)
  }
  lambda
}

.logisticNegGradient <- function(beta, xDesign, y) {
  if (inherits(xDesign, "sparseMatrix")) {
    return(as.numeric(logisticGradientCpp(.asDgCMatrix(xDesign), beta, y)))
  }
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  as.numeric(Matrix::crossprod(xDesign, pVec - y)) / length(y)
}

.logisticNegHessian <- function(beta, xDesign) {
  if (inherits(xDesign, "sparseMatrix")) {
    return(as.matrix(logisticHessianCpp(.asDgCMatrix(xDesign), beta)))
  }
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  wDiag <- pVec * (1 - pVec)
  as.matrix(Matrix::crossprod(xDesign, Matrix::Diagonal(x = wDiag) %*% xDesign)) / nrow(xDesign)
}

.logisticNegGradientHessian <- function(beta, xDesign, y) {
  if (inherits(xDesign, "sparseMatrix")) {
    out <- logisticGradientHessianCpp(.asDgCMatrix(xDesign), beta, y)
    return(list(gradient = as.numeric(out$gradient), hessian = as.matrix(out$hessian)))
  }
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  residual <- pVec - y
  wDiag <- pVec * (1 - pVec)
  list(
    gradient = as.numeric(Matrix::crossprod(xDesign, residual)) / length(y),
    hessian = as.matrix(Matrix::crossprod(xDesign, Matrix::Diagonal(x = wDiag) %*% xDesign)) / nrow(xDesign)
  )
}

.logisticNegHessianDiag <- function(beta, xDesign) {
  if (inherits(xDesign, "sparseMatrix")) {
    return(as.numeric(logisticHessianDiagCpp(.asDgCMatrix(xDesign), beta)))
  }
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  wDiag <- pVec * (1 - pVec)
  as.numeric(Matrix::colSums((xDesign^2) * wDiag)) / nrow(xDesign)
}

.logisticNegGradientHessianDiag <- function(beta, xDesign, y) {
  if (inherits(xDesign, "sparseMatrix")) {
    out <- logisticGradientHessianDiagCpp(.asDgCMatrix(xDesign), beta, y)
    return(list(gradient = as.numeric(out$gradient), hessianDiag = as.numeric(out$hessianDiag)))
  }
  pVec <- pmin(pmax(stats::plogis(as.numeric(xDesign %*% beta)), 1e-8), 1 - 1e-8)
  residual <- pVec - y
  wDiag <- pVec * (1 - pVec)
  list(
    gradient = as.numeric(Matrix::crossprod(xDesign, residual)) / length(y),
    hessianDiag = as.numeric(Matrix::colSums((xDesign^2) * wDiag)) / nrow(xDesign)
  )
}

.negLogLikMean <- function(beta, xDesign, y) {
  lin <- as.numeric(xDesign %*% beta)
  val <- binaryLogLoss(lin, y, meanLoss = TRUE)
  if (is.finite(val)) val else .Machine$double.xmax / 1e100
}

.adapCvMetric <- function(beta, xDesign, y, metric = c("deviance", "auc")) {
  metric <- match.arg(metric)
  if (identical(metric, "deviance")) {
    return(.negLogLikMean(beta, xDesign, y))
  }
  if (length(unique(y)) < 2L) {
    return(NA_real_)
  }
  preds <- stats::plogis(as.numeric(xDesign %*% beta))
  as.numeric(pROC::roc(response = y, predictor = preds, quiet = TRUE)$auc)
}

.adapBestLambdaIndex <- function(scores, lambdaSeq, metric = c("deviance", "auc"),
                                 tieTolerance = 1e-8) {
  metric <- match.arg(metric)
  finite <- is.finite(scores)
  if (!any(finite)) {
    return(NA_integer_)
  }
  if (identical(metric, "auc")) {
    best <- max(scores[finite], na.rm = TRUE)
    candidates <- which(finite & scores >= best - tieTolerance)
    return(candidates[which.max(lambdaSeq[candidates])])
  }
  best <- min(scores[finite], na.rm = TRUE)
  candidates <- which(finite & scores <= best + tieTolerance)
  candidates[which.max(lambdaSeq[candidates])]
}

.adapCvSubset <- function(y, maxRows = Inf, seed = 42L) {
  n <- length(y)
  if (!is.finite(maxRows)) {
    return(seq_len(n))
  }
  maxRows <- as.integer(maxRows)
  if (length(maxRows) != 1L || is.na(maxRows) || maxRows <= 0L || maxRows >= n) {
    return(seq_len(n))
  }
  oldSeed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(oldSeed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", oldSeed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  cases <- which(y == 1L)
  controls <- which(y == 0L)
  if (length(cases) == 0L || length(controls) == 0L) {
    return(sort(sample(seq_len(n), maxRows)))
  }
  targetCases <- min(length(cases), max(1L, round(maxRows * length(cases) / n)))
  targetControls <- maxRows - targetCases
  if (targetControls > length(controls)) {
    targetControls <- length(controls)
    targetCases <- min(length(cases), maxRows - targetControls)
  }
  sort(c(
    sample(cases, targetCases),
    sample(controls, targetControls)
  ))
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
  if (is.matrix(B)) {
    return(as.numeric(quadraticLassoCdCpp(
      aTilde = as.numeric(aTilde),
      bMatrix = B,
      betaInit = as.numeric(betaInit),
      lambda = lambda,
      maxIter = as.integer(maxIter),
      tol = tol,
      penalizeNullable = penalize
    )))
  }
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
      if (!is.finite(z)) {
        z <- beta[j]
      }
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
                                     globalGrad, globalHess,
                                     gradBar = NULL, hBar = NULL) {
  evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
  hEval <- evalTerms$hessian
  if (is.null(hBar)) {
    hBar <- .logisticNegHessian(betaBar, xDesign)
  }
  if (is.null(gradBar)) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  }
  B <- hEval + globalHess - hBar
  aTilde <- evalTerms$gradient -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    gradBar -
    as.numeric(t(betaBar) %*% (globalHess - hBar))
  list(aTilde = as.numeric(aTilde), B = B)
}

.adapFirstOrderSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                               globalGrad, gradBar = NULL) {
  evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
  hEval <- evalTerms$hessian
  if (is.null(gradBar)) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  }
  deltaGrad <- globalGrad - gradBar
  aTilde <- evalTerms$gradient -
    as.numeric(t(betaEval) %*% hEval) +
    deltaGrad
  list(aTilde = as.numeric(aTilde), B = hEval)
}

.adapLocalFullRemoteDiagSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                                        globalGrad,
                                                        globalHessDiag,
                                                        gradBar = NULL,
                                                        hBarDiag = NULL) {
  evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
  hEval <- evalTerms$hessian
  if (is.null(hBarDiag)) {
    hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
  }
  if (is.null(gradBar)) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  }
  p <- length(betaEval)
  correctionDiag <- globalHessDiag - hBarDiag
  B <- hEval + diag(correctionDiag, p, p)
  aTilde <- evalTerms$gradient -
    as.numeric(t(betaEval) %*% hEval) +
    globalGrad -
    gradBar -
    betaBar * correctionDiag
  list(aTilde = as.numeric(aTilde), B = B)
}

.fitPdaAdapSurrogate <- function(xDesign, y, betaLead, betaBar,
                                 globalGrad, globalHess, lambda,
                                 maxOuter = 100L, maxInner = 100L,
                                 tol = 1e-5, betaInit = NULL) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  if (inherits(xDesign, "sparseMatrix")) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
    hBar <- .logisticNegHessian(betaBar, xDesign)
    out <- adapFullSurrogateFitCpp(
      x = .asDgCMatrix(xDesign),
      y = y,
      betaStart = beta,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = as.matrix(globalHess),
      gradBar = gradBar,
      hBar = as.matrix(hBar),
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol
    )
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBar <- .logisticNegHessian(betaBar, xDesign)
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHess = globalHess,
      gradBar = gradBar,
      hBar = hBar
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
                               ridge = 1e-4, betaInit = NULL) {
  beta <- if (is.null(betaInit)) beta0 else betaInit
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE

  if (isTRUE(useFull)) {
    leadTerms <- .logisticNegGradientHessian(beta0, xDesign, y)
    hCorr <- globalHess - leadTerms$hessian
  } else {
    leadTerms <- .logisticNegGradientHessianDiag(beta0, xDesign, y)
    hCorr <- as.numeric(globalHess - leadTerms$hessianDiag)
  }

  bCorr <- globalGrad - leadTerms$gradient
  for (iter in seq_len(maxIter)) {
    betaOld <- beta
    localTerms <- .logisticNegGradientHessianDiag(beta, xDesign, y)
    gLocal <- localTerms$gradient
    hLocalDiag <- localTerms$hessianDiag
    if (isTRUE(useFull)) {
      gTilde <- gLocal + bCorr + as.numeric(hCorr %*% (beta - beta0))
      hDiag <- hLocalDiag + diag(hCorr)
    } else {
      gTilde <- gLocal + bCorr + hCorr * (beta - beta0)
      hDiag <- hLocalDiag + hCorr
    }
    hDiag[!is.finite(hDiag) | hDiag <= 0] <- ridge
    hDiag <- pmax(hDiag, ridge)
    for (j in seq_along(beta)) {
      z <- beta[j] - gTilde[j] / hDiag[j]
      if (!is.finite(z)) {
        z <- beta[j]
      }
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
                              ridge = 1e-4,
                              selectionMetric = c("deviance", "auc"),
                              tieTolerance = 1e-8) {
  selectionMetric <- match.arg(selectionMetric)
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  warmStarts <- rep(list(betaBar), foldsK)
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
        ridge = ridge,
        betaInit = warmStarts[[fold]]
      )
      warmStarts[[fold]] <- fit
      foldLoss[fold] <- .adapCvMetric(
        fit,
        xDesign[idxVal, , drop = FALSE],
        y[idxVal],
        metric = selectionMetric
      )
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
  list(lambda = lambdaSeq[idx], scores = scores)
}

.fitPdaAdapRemoteDiagSurrogate <- function(xDesign, y, betaLead, betaBar,
                                           globalGrad, globalHessDiag,
                                           lambda,
                                           maxOuter = 100L, maxInner = 100L,
                                           tol = 1e-5, betaInit = NULL) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  if (inherits(xDesign, "sparseMatrix")) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
    hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
    out <- adapDiagSurrogateFitCpp(
      x = .asDgCMatrix(xDesign),
      y = y,
      betaStart = beta,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      gradBar = gradBar,
      hBarDiag = hBarDiag,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol
    )
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      gradBar = gradBar,
      hBarDiag = hBarDiag
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
                                           tol = 1e-5, betaInit = NULL) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  if (inherits(xDesign, "sparseMatrix")) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
    out <- adapFirstSurrogateFitCpp(
      x = .asDgCMatrix(xDesign),
      y = y,
      betaStart = beta,
      betaBar = betaBar,
      globalGrad = globalGrad,
      gradBar = gradBar,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol
    )
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  for (iter in seq_len(maxOuter)) {
    old <- beta
    comp <- .adapFirstOrderSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      gradBar = gradBar
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

.pdaAdapSurrogateLeadCv <- function(xDesign, y, betaInit, lambdaSeq,
                                    foldsK = 5L, seed = 42L,
                                    search = c("grid", "optimize"),
                                    searchTol = log(1.5),
                                    maxEvals = 25L,
                                    selectionMetric = c("deviance", "auc"),
                                    tieTolerance = 1e-8,
                                    makeFoldInfo,
                                    fitFold) {
  search <- match.arg(search)
  selectionMetric <- match.arg(selectionMetric)
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  foldInfo <- lapply(seq_len(foldsK), function(fold) {
    idxVal <- which(folds == fold)
    info <- list(
      fold = fold,
      idxVal = idxVal,
      idxTr = which(folds != fold),
      nVal = length(idxVal)
    )
    extra <- makeFoldInfo(info)
    c(info, extra)
  })

  evalCache <- new.env(parent = emptyenv())
  lambdaFits <- vector("list", foldsK)
  for (fold in seq_len(foldsK)) {
    lambdaFits[[fold]] <- list()
  }
  lambdaKey <- function(lambda) format(lambda, digits = 17, scientific = TRUE)
  closestWarmStart <- function(fold, lambda) {
    fits <- lambdaFits[[fold]]
    if (length(fits) == 0L) {
      return(betaInit)
    }
    fitLambdas <- as.numeric(names(fits))
    idx <- which.min(abs(log(fitLambdas) - log(lambda)))
    fits[[idx]]
  }
  evaluateLambda <- function(lambda) {
    key <- lambdaKey(lambda)
    if (exists(key, envir = evalCache, inherits = FALSE)) {
      return(get(key, envir = evalCache, inherits = FALSE)$score)
    }
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      info <- foldInfo[[fold]]
      fit <- fitFold(info, lambda, closestWarmStart(fold, lambda))
      lambdaFits[[fold]][[key]] <<- fit
      foldLoss[fold] <- .adapCvMetric(
        fit,
        xDesign[info$idxVal, , drop = FALSE],
        y[info$idxVal],
        metric = selectionMetric
      )
    }
    score <- mean(foldLoss, na.rm = TRUE)
    assign(key, list(lambda = lambda, score = score), envir = evalCache)
    score
  }

  if (identical(search, "grid") || length(lambdaSeq) < 3L) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
    return(list(lambda = lambdaSeq[idx], scores = scores))
  }

  lambdaRange <- range(lambdaSeq[is.finite(lambdaSeq) & lambdaSeq > 0])
  if (!all(is.finite(lambdaRange)) || lambdaRange[1] <= 0 || lambdaRange[1] == lambdaRange[2]) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
    return(list(lambda = lambdaSeq[idx], scores = scores))
  }

  maxEvals <- as.integer(maxEvals)
  if (length(maxEvals) != 1L || is.na(maxEvals) || maxEvals < 3L) {
    stop("maxEvals must be an integer of at least 3")
  }
  if (length(searchTol) != 1L || !is.finite(searchTol) || searchTol <= 0) {
    stop("searchTol must be a positive finite value")
  }

  objective <- function(logLambda) {
    evaluateLambda(exp(logLambda))
  }
  lower <- log(lambdaRange[1])
  upper <- log(lambdaRange[2])
  objective(lower)
  objective(upper)
  evalCount <- 2L
  if (maxEvals > evalCount && (upper - lower) > searchTol) {
    invPhi <- (sqrt(5) - 1) / 2
    invPhi2 <- (3 - sqrt(5)) / 2
    x1 <- lower + invPhi2 * (upper - lower)
    x2 <- lower + invPhi * (upper - lower)
    f1 <- objective(x1)
    f2 <- objective(x2)
    evalCount <- evalCount + 2L
    while (evalCount < maxEvals && (upper - lower) > searchTol) {
      betterLeft <- if (identical(selectionMetric, "auc")) f1 > f2 else f1 < f2
      if (betterLeft) {
        upper <- x2
        x2 <- x1
        f2 <- f1
        x1 <- lower + invPhi2 * (upper - lower)
        f1 <- objective(x1)
      } else {
        lower <- x1
        x1 <- x2
        f1 <- f2
        x2 <- lower + invPhi * (upper - lower)
        f2 <- objective(x2)
      }
      evalCount <- evalCount + 1L
    }
  }
  evaluated <- as.list(evalCache)
  lambdaVals <- vapply(evaluated, `[[`, numeric(1), "lambda")
  scores <- vapply(evaluated, `[[`, numeric(1), "score")
  keep <- order(lambdaVals, decreasing = TRUE)
  lambdaVals <- lambdaVals[keep]
  scores <- scores[keep]
  idx <- .adapBestLambdaIndex(scores, lambdaVals, selectionMetric, tieTolerance)
  list(lambda = lambdaVals[idx], scores = scores, lambdaSeq = lambdaVals)
}

.pdaAdapLeadCv <- function(xDesign, y, betaLead, betaBar,
                           globalGrad, globalHess, lambdaSeq,
                           totalN,
                           foldsK = 5L, seed = 42L,
                           maxOuter = 100L, maxInner = 100L,
                           tol = 1e-5,
                           search = c("grid", "optimize"),
                           searchTol = log(1.5),
                           maxEvals = 25L,
                           selectionMetric = c("deviance", "auc"),
                           tieTolerance = 1e-8,
                           cvMaxRows = Inf,
                           globalAdjustment = c("leaveValOut", "pda")) {
  globalAdjustment <- match.arg(globalAdjustment)
  cvIdx <- .adapCvSubset(y, maxRows = cvMaxRows, seed = seed)
  xCv <- xDesign[cvIdx, , drop = FALSE]
  yCv <- y[cvIdx]
  .pdaAdapSurrogateLeadCv(
    xDesign = xCv,
    y = yCv,
    betaInit = betaLead,
    lambdaSeq = lambdaSeq,
    foldsK = foldsK,
    seed = seed,
    search = search,
    searchTol = searchTol,
    maxEvals = maxEvals,
    selectionMetric = selectionMetric,
    tieTolerance = tieTolerance,
    makeFoldInfo = function(info) {
      if (identical(globalAdjustment, "leaveValOut")) {
        gradVal <- .logisticNegGradient(betaBar, xCv[info$idxVal, , drop = FALSE], yCv[info$idxVal])
        hessVal <- .logisticNegHessian(betaBar, xCv[info$idxVal, , drop = FALSE])
        denom <- max(totalN - info$nVal, 1L)
        return(list(
          gradTrainGlobal = (globalGrad * totalN - gradVal * info$nVal) / denom,
          hessTrainGlobal = (globalHess * totalN - hessVal * info$nVal) / denom
        ))
      }
      list(
        gradTrainGlobal = globalGrad,
        hessTrainGlobal = globalHess
      )
    },
    fitFold = function(info, lambda, warmStart) {
      .fitPdaAdapSurrogate(
        xDesign = xCv[info$idxTr, , drop = FALSE],
        y = yCv[info$idxTr],
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = info$gradTrainGlobal,
        globalHess = info$hessTrainGlobal,
        lambda = lambda,
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol,
        betaInit = warmStart
      )
    }
  )
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
                                tol = 1e-5,
                                search = c("grid", "optimize"),
                                searchTol = log(1.5),
                                maxEvals = 25L,
                                selectionMetric = c("deviance", "auc"),
                                tieTolerance = 1e-8,
                                cvMaxRows = Inf,
                                globalAdjustment = c("leaveValOut", "pda")) {
  globalAdjustment <- match.arg(globalAdjustment)
  cvIdx <- .adapCvSubset(y, maxRows = cvMaxRows, seed = seed)
  xCv <- xDesign[cvIdx, , drop = FALSE]
  yCv <- y[cvIdx]
  .pdaAdapSurrogateLeadCv(
    xDesign = xCv,
    y = yCv,
    betaInit = betaLead,
    lambdaSeq = lambdaSeq,
    foldsK = foldsK,
    seed = seed,
    search = search,
    searchTol = searchTol,
    maxEvals = maxEvals,
    selectionMetric = selectionMetric,
    tieTolerance = tieTolerance,
    makeFoldInfo = function(info) {
      if (identical(globalAdjustment, "leaveValOut")) {
        gradVal <- .logisticNegGradient(betaBar, xCv[info$idxVal, , drop = FALSE], yCv[info$idxVal])
        denom <- max(totalN - info$nVal, 1L)
        return(list(
          gradTrainGlobal = (globalGrad * totalN - gradVal * info$nVal) / denom
        ))
      }
      list(gradTrainGlobal = globalGrad)
    },
    fitFold = function(info, lambda, warmStart) {
      .fitPdaAdapFirstOrderSurrogate(
        xDesign = xCv[info$idxTr, , drop = FALSE],
        y = yCv[info$idxTr],
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = info$gradTrainGlobal,
        lambda = lambda,
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol,
        betaInit = warmStart
      )
    }
  )
}

.pdaAdapDiagLambdaSeq <- function(xDesign, y, betaLead, betaBar,
                                  globalGrad, globalHessDiag = NULL,
                                  gridLen = 100L) {
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
  if (!is.finite(lamMax) || lamMax <= 0) {
    lamMax <- 1
  }
  lamMin <- if (nrow(xDesign) < ncol(xDesign)) 0.02 * lamMax else 1e-4 * lamMax
  rev(exp(seq(log(lamMin), log(lamMax), length.out = gridLen)))
}

.pdaAdapDiagLeadCv <- function(xDesign, y, betaLead, betaBar,
                               globalGrad, globalHessDiag = NULL,
                               lambdaSeq, totalN,
                               foldsK = 5L, seed = 42L,
                               maxOuter = 100L, maxInner = 100L,
                               tol = 1e-5,
                               search = c("grid", "optimize"),
                               searchTol = log(1.5),
                               maxEvals = 25L,
                               selectionMetric = c("deviance", "auc"),
                               tieTolerance = 1e-8,
                               cvMaxRows = Inf,
                               globalAdjustment = c("leaveValOut", "pda")) {
  globalAdjustment <- match.arg(globalAdjustment)
  cvIdx <- .adapCvSubset(y, maxRows = cvMaxRows, seed = seed)
  xCv <- xDesign[cvIdx, , drop = FALSE]
  yCv <- y[cvIdx]
  .pdaAdapSurrogateLeadCv(
    xDesign = xCv,
    y = yCv,
    betaInit = betaLead,
    lambdaSeq = lambdaSeq,
    foldsK = foldsK,
    seed = seed,
    search = search,
    searchTol = searchTol,
    maxEvals = maxEvals,
    selectionMetric = selectionMetric,
    tieTolerance = tieTolerance,
    makeFoldInfo = function(info) {
      if (identical(globalAdjustment, "leaveValOut")) {
        gradVal <- .logisticNegGradient(betaBar, xCv[info$idxVal, , drop = FALSE], yCv[info$idxVal])
        denom <- max(totalN - info$nVal, 1L)
        hessValDiag <- .logisticNegHessianDiag(betaBar, xCv[info$idxVal, , drop = FALSE])
        return(list(
          gradTrainGlobal = (globalGrad * totalN - gradVal * info$nVal) / denom,
          hessTrainGlobalDiag = (globalHessDiag * totalN - hessValDiag * info$nVal) / denom
        ))
      }
      list(
        gradTrainGlobal = globalGrad,
        hessTrainGlobalDiag = globalHessDiag
      )
    },
    fitFold = function(info, lambda, warmStart) {
      .fitPdaAdapRemoteDiagSurrogate(
        xDesign = xCv[info$idxTr, , drop = FALSE],
        y = yCv[info$idxTr],
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = info$gradTrainGlobal,
        globalHessDiag = info$hessTrainGlobalDiag,
        lambda = lambda,
        maxOuter = maxOuter,
        maxInner = maxInner,
        tol = tol,
        betaInit = warmStart
      )
    }
  )
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
    selectedLambda = config[["lambda", exact = TRUE]] %||% NA_real_,
    w = rep(0, p)
  )
}

.serverInitPdaAdapPda <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapSolveStyle <- "fullQuadratic"
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
    fixedLambda <- .fixedAdapLambda(config)
    solveStyle <- serverBroadcast$adapSolveStyle %||% config$adapSolveStyle %||% "fullQuadratic"
    if (identical(solveStyle, "pda")) {
      if (!is.null(fixedLambda)) {
        lambda <- fixedLambda
        lambdaSeq <- lambda
        cvScores <- NA_real_
      } else {
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
          ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4,
          selectionMetric = config$lambdaSelectionMetric %||% "deviance",
          tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
      }
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
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
      ))
    }
    if (!is.null(fixedLambda)) {
      lambda <- fixedLambda
      lambdaSeq <- lambda
      cvScores <- NA_real_
    } else {
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
        tol = config$tol %||% 1e-5,
        search = config$lambdaSearch %||% "grid",
        searchTol = config$lambdaSearchTol %||% log(1.5),
        maxEvals = config$lambdaSearchMaxEvals %||% 25L,
        selectionMetric = config$lambdaSelectionMetric %||% "deviance",
        tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8,
        cvMaxRows = config$lambdaCvMaxRows %||% Inf,
        globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut"
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
    }
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
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
    ))
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
          lambdaSelectionMetric = leadReport$lambdaSelectionMetric %||% NA_character_,
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
    selectedLambda = config[["lambda", exact = TRUE]] %||% NA_real_,
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
    fixedLambda <- .fixedAdapLambda(config)
    if (identical(mode, "first")) {
      if (!is.null(fixedLambda)) {
        lambda <- fixedLambda
        lambdaSeq <- lambda
        cvScores <- NA_real_
      } else {
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
          tol = config$tol %||% 1e-5,
          search = config$lambdaSearch %||% "grid",
          searchTol = config$lambdaSearchTol %||% log(1.5),
          maxEvals = config$lambdaSearchMaxEvals %||% 25L,
          selectionMetric = config$lambdaSelectionMetric %||% "deviance",
          tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8,
          cvMaxRows = config$lambdaCvMaxRows %||% Inf,
          globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut"
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
        lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
      }
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
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
      ))
    }

    if (identical(diagStyle, "pda")) {
      if (!is.null(fixedLambda)) {
        lambda <- fixedLambda
        lambdaSeq <- lambda
        cvScores <- NA_real_
      } else {
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
        lambda <- cv$lambda
        cvScores <- cv$scores
      }
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
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        lambdaSelectionMetric = "deviance"
      ))
    }

    if (!is.null(fixedLambda)) {
      lambda <- fixedLambda
      lambdaSeq <- lambda
      cvScores <- NA_real_
    } else {
      if (is.null(lambdaSeq)) {
        lambdaSeq <- .pdaAdapDiagLambdaSeq(
          xDesign = xDesign,
          y = y,
          betaLead = betaLead,
          betaBar = betaBar,
          globalGrad = globalGrad,
          globalHessDiag = globalHessDiag,
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
        foldsK = config$foldsK %||% 5L,
        seed = config$cvSeed %||% 42L,
        maxOuter = config$maxOuter %||% 100L,
        maxInner = config$maxInner %||% 100L,
        tol = config$tol %||% 1e-5,
        search = config$lambdaSearch %||% "grid",
        searchTol = config$lambdaSearchTol %||% log(1.5),
        maxEvals = config$lambdaSearchMaxEvals %||% 25L,
        selectionMetric = config$lambdaSelectionMetric %||% "deviance",
        tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8,
        cvMaxRows = config$lambdaCvMaxRows %||% Inf,
        globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut"
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
    }
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
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
    ))
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
          lambdaSelectionMetric = leadReport$lambdaSelectionMetric %||% NA_character_,
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
