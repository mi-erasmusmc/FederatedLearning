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

.adapRangeStats <- function(x, prefix) {
  x <- as.numeric(x)
  finite <- is.finite(x)
  out <- c(
    length = length(x),
    finite = sum(finite),
    nonFinite = sum(!finite),
    min = NA_real_,
    max = NA_real_,
    maxAbs = NA_real_
  )
  if (any(finite)) {
    out["min"] <- min(x[finite])
    out["max"] <- max(x[finite])
    out["maxAbs"] <- max(abs(x[finite]))
  }
  suffix <- paste0(toupper(substr(names(out), 1, 1)), substring(names(out), 2))
  stats::setNames(out, paste0(prefix, suffix))
}

.adapMatrixStats <- function(B, prefix = "B") {
  B <- as.matrix(B)
  vals <- as.numeric(B)
  diagVals <- diag(B)
  finite <- is.finite(vals)
  eig <- tryCatch(
    eigen((B + t(B)) / 2, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  eigFinite <- is.finite(eig)
  c(
    stats::setNames(c(nrow(B), ncol(B), sum(!finite)), paste0(prefix, c("Rows", "Cols", "NonFinite"))),
    .adapRangeStats(diagVals, paste0(prefix, "Diag")),
    stats::setNames(sum(is.finite(diagVals) & diagVals <= 0), paste0(prefix, "DiagNonPositive")),
    stats::setNames(if (any(eigFinite)) min(eig[eigFinite]) else NA_real_, paste0(prefix, "EigenMin")),
    stats::setNames(if (any(eigFinite)) max(eig[eigFinite]) else NA_real_, paste0(prefix, "EigenMax")),
    stats::setNames(sum(is.finite(eig) & eig <= 0), paste0(prefix, "EigenNonPositive")),
    stats::setNames(tryCatch(kappa(B), error = function(e) NA_real_), paste0(prefix, "Kappa"))
  )
}

.adapSurrogateStats <- function(kind, xTrain, yTrain, betaLead, betaBar,
                                globalGrad, globalHess = NULL,
                                globalHessDiag = NULL) {
  tryCatch({
    comp <- switch(kind,
      full = .adapSurrogateComponents(
        betaEval = betaLead,
        betaBar = betaBar,
        xDesign = xTrain,
        y = yTrain,
        globalGrad = globalGrad,
        globalHess = globalHess
      ),
      first = .adapFirstOrderSurrogateComponents(
        betaEval = betaLead,
        betaBar = betaBar,
        xDesign = xTrain,
        y = yTrain,
        globalGrad = globalGrad
      ),
      diag = .adapLocalFullRemoteDiagSurrogateComponents(
        betaEval = betaLead,
        betaBar = betaBar,
        xDesign = xTrain,
        y = yTrain,
        globalGrad = globalGrad,
        globalHessDiag = globalHessDiag
      ),
      stop("Unsupported ADAP surrogate diagnostic kind: ", kind, call. = FALSE)
    )
    c(
      surrogateDiagnosticFailed = 0,
      .adapRangeStats(comp$aTilde, "aTilde"),
      .adapMatrixStats(comp$B, "B")
    )
  }, error = function(e) {
    c(
      surrogateDiagnosticFailed = 1,
      aTildeLength = NA_real_,
      aTildeFinite = NA_real_,
      aTildeNonFinite = NA_real_,
      aTildeMin = NA_real_,
      aTildeMax = NA_real_,
      aTildeMaxAbs = NA_real_,
      BRows = NA_real_,
      BCols = NA_real_,
      BNonFinite = NA_real_,
      BDiagLength = NA_real_,
      BDiagFinite = NA_real_,
      BDiagNonFinite = NA_real_,
      BDiagMin = NA_real_,
      BDiagMax = NA_real_,
      BDiagMaxAbs = NA_real_,
      BDiagNonPositive = NA_real_,
      BEigenMin = NA_real_,
      BEigenMax = NA_real_,
      BEigenNonPositive = NA_real_,
      BKappa = NA_real_
    )
  })
}

.adapValidationStats <- function(beta, xVal, yVal, metric) {
  eta <- as.numeric(xVal %*% beta)
  rawDeviance <- binaryLogLoss(eta, yVal, meanLoss = TRUE)
  score <- .adapCvMetric(beta, xVal, yVal, metric = metric)
  c(
    score = score,
    rawDeviance = rawDeviance,
    rawDevianceFinite = is.finite(rawDeviance),
    scoreFinite = is.finite(score),
    .adapRangeStats(eta, "eta")
  )
}

.adapFailedValidationStats <- function(metric) {
  score <- if (identical(metric, "auc")) 0 else 1e100
  c(
    score = score,
    rawDeviance = NA_real_,
    rawDevianceFinite = FALSE,
    scoreFinite = FALSE,
    etaLength = NA_real_,
    etaFinite = NA_real_,
    etaNonFinite = NA_real_,
    etaMin = NA_real_,
    etaMax = NA_real_,
    etaMaxAbs = NA_real_
  )
}

.adapFitBeta <- function(fit) {
  if (is.list(fit) && !is.null(fit$beta)) {
    return(as.numeric(fit$beta))
  }
  as.numeric(fit)
}

.adapFitFailureReason <- function(fit) {
  if (is.list(fit) && !is.null(fit$failureReason)) {
    reason <- as.character(fit$failureReason)
    if (length(reason) && !is.na(reason[1]) && nzchar(reason[1])) {
      return(reason[1])
    }
  }
  beta <- .adapFitBeta(fit)
  if (!all(is.finite(beta))) {
    return("non_finite_beta")
  }
  ""
}

.adapFitFailed <- function(fit) {
  nzchar(.adapFitFailureReason(fit))
}

.adapFitDiagnostics <- function(fit) {
  beta <- .adapFitBeta(fit)
  c(
    outerIterations = if (is.list(fit) && !is.null(fit$outerIterations)) fit$outerIterations else NA_real_,
    converged = if (is.list(fit) && !is.null(fit$converged)) isTRUE(fit$converged) else NA,
    .adapRangeStats(beta, "beta"),
    betaNonzero = sum(abs(beta) > 1e-8, na.rm = TRUE),
    fitFailed = .adapFitFailed(fit),
    innerIterations = if (is.list(fit) && !is.null(fit$innerIterations)) fit$innerIterations else NA_real_,
    innerConverged = if (is.list(fit) && !is.null(fit$innerConverged)) isTRUE(fit$innerConverged) else NA,
    innerObjective = if (is.list(fit) && !is.null(fit$innerObjective)) fit$innerObjective else NA_real_,
    innerMaxAbsStep = if (is.list(fit) && !is.null(fit$innerMaxAbsStep)) fit$innerMaxAbsStep else NA_real_,
    innerBacktracks = if (is.list(fit) && !is.null(fit$innerBacktracks)) fit$innerBacktracks else NA_real_
  )
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
                                        penalize = NULL,
                                        initialStepBound = 1,
                                        minStep = 1e-8,
                                        maxBacktracks = 25L,
                                        returnDetails = FALSE) {
  if (is.matrix(B)) {
    out <- tryCatch(
      as.numeric(quadraticLassoCdCpp(
        aTilde = as.numeric(aTilde),
        bMatrix = B,
        betaInit = as.numeric(betaInit),
        lambda = lambda,
        maxIter = as.integer(maxIter),
        tol = tol,
        penalizeNullable = penalize,
        initialStepBound = initialStepBound,
        minStep = minStep,
        maxBacktracks = as.integer(maxBacktracks)
      )),
      error = function(e) {
        if (!isTRUE(returnDetails)) {
          stop(e)
        }
        list(
          beta = as.numeric(betaInit),
          iterations = 0L,
          converged = FALSE,
          failureReason = conditionMessage(e),
          objective = NA_real_,
          maxAbsStep = NA_real_,
          backtracks = NA_integer_
        )
      }
    )
    if (!isTRUE(returnDetails) || is.list(out)) {
      return(out)
    }
    return(list(
      beta = out,
      iterations = NA_integer_,
      converged = NA,
      failureReason = "",
      objective = NA_real_,
      maxAbsStep = NA_real_,
      backtracks = NA_integer_
    ))
  }
  beta <- betaInit
  p <- length(beta)
  if (length(initialStepBound) != 1L || !is.finite(initialStepBound) || initialStepBound <= 0) {
    stop("initialStepBound must be a positive finite value", call. = FALSE)
  }
  if (length(minStep) != 1L || !is.finite(minStep) || minStep <= 0) {
    stop("minStep must be a positive finite value", call. = FALSE)
  }
  maxBacktracks <- as.integer(maxBacktracks)
  if (length(maxBacktracks) != 1L || is.na(maxBacktracks) || maxBacktracks < 0L) {
    stop("maxBacktracks must be non-negative", call. = FALSE)
  }
  if (is.null(penalize)) {
    penalize <- rep(TRUE, p)
    penalize[1] <- FALSE
  }
  diagB <- if (is.matrix(B)) diag(B) else Matrix::diag(B)
  fail <- function(reason, iter = 0L, objective = NA_real_, maxAbsStep = NA_real_, backtracks = NA_integer_) {
    if (isTRUE(returnDetails)) {
      return(list(
        beta = beta,
        iterations = iter,
        converged = FALSE,
        failureReason = reason,
        objective = objective,
        maxAbsStep = maxAbsStep,
        backtracks = backtracks
      ))
    }
    stop(reason, call. = FALSE)
  }
  if (!all(is.finite(c(aTilde, as.numeric(B), beta, lambda)))) {
    return(fail("non_finite_surrogate_input"))
  }
  if (any(!is.finite(diagB) | diagB <= 0)) {
    return(fail("non_positive_coordinate_curvature"))
  }
  objective <- function(betaValue) {
    as.numeric(crossprod(aTilde, betaValue) +
      crossprod(betaValue, B %*% betaValue) / 2 +
      lambda * sum(abs(betaValue[penalize])))
  }
  currentObjective <- objective(beta)
  if (!is.finite(currentObjective)) {
    return(fail("non_finite_objective"))
  }
  stepBounds <- rep(initialStepBound, p)
  backtracks <- 0L
  maxAbsStep <- NA_real_
  for (iter in seq_len(maxIter)) {
    betaOld <- beta
    oldObjective <- currentObjective
    maxAbsStep <- 0
    for (j in seq_len(p)) {
      hjj <- diagB[j]
      b <- aTilde[j] + sum(as.numeric(B[j, ]) * beta) - hjj * beta[j]
      z <- -b / hjj
      if (!is.finite(z)) {
        return(fail("non_finite_coordinate_update", iter, currentObjective, maxAbsStep, backtracks))
      }
      proposed <- if (penalize[j]) .softScalar(z, lambda / hjj) else z
      step <- proposed - beta[j]
      if (!is.finite(step)) {
        return(fail("non_finite_coordinate_step", iter, currentObjective, maxAbsStep, backtracks))
      }
      step <- max(min(step, stepBounds[j]), -stepBounds[j])
      if (abs(step) <= minStep) {
        next
      }
      oldBetaJ <- beta[j]
      smoothGrad <- aTilde[j] + sum(as.numeric(B[j, ]) * beta)
      accepted <- FALSE
      acceptedObjective <- NA_real_
      for (bt in seq_len(maxBacktracks + 1L)) {
        trialBetaJ <- oldBetaJ + step
        l1Delta <- if (penalize[j]) lambda * (abs(trialBetaJ) - abs(oldBetaJ)) else 0
        trialObjective <- currentObjective + smoothGrad * step + 0.5 * hjj * step^2 + l1Delta
        descentTol <- 1e-12 * (abs(currentObjective) + 1)
        if (is.finite(trialObjective) && trialObjective <= currentObjective + descentTol) {
          accepted <- TRUE
          acceptedObjective <- trialObjective
          break
        }
        step <- step / 2
        backtracks <- backtracks + 1L
        if (abs(step) <= minStep) {
          break
        }
      }
      if (!accepted) {
        return(fail("non_descent_coordinate_step", iter, currentObjective, maxAbsStep, backtracks))
      }
      beta[j] <- oldBetaJ + step
      currentObjective <- acceptedObjective
      maxAbsStep <- max(maxAbsStep, abs(step))
      stepBounds[j] <- max(2 * abs(step), stepBounds[j] / 2, minStep)
      if (!all(is.finite(beta)) || !is.finite(currentObjective)) {
        return(fail("non_finite_coordinate_state", iter, currentObjective, maxAbsStep, backtracks))
      }
    }
    diffObj <- currentObjective - oldObjective
    if (is.finite(diffObj) && abs(diffObj) < tol) {
      if (isTRUE(returnDetails)) {
        return(list(
          beta = beta,
          iterations = iter,
          converged = TRUE,
          failureReason = "",
          objective = currentObjective,
          maxAbsStep = maxAbsStep,
          backtracks = backtracks
        ))
      }
      return(beta)
    }
  }
  if (isTRUE(returnDetails)) {
    return(list(
      beta = beta,
      iterations = as.integer(maxIter),
      converged = FALSE,
      failureReason = "",
      objective = currentObjective,
      maxAbsStep = maxAbsStep,
      backtracks = backtracks
    ))
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
                                 tol = 1e-5, betaInit = NULL,
                                 returnDetails = FALSE,
                                 cdStepBound = 1,
                                 cdMinStep = 1e-8,
                                 cdMaxBacktracks = 25L) {
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
      tol = tol,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks)
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBar <- .logisticNegHessian(betaBar, xDesign)
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(
    iterations = NA_integer_,
    converged = NA,
    objective = NA_real_,
    maxAbsStep = NA_real_,
    backtracks = NA_integer_
  )
  for (iter in seq_len(maxOuter)) {
    iterations <- iter
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
    cd <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks),
      returnDetails = TRUE
    )
    beta <- cd$beta
    if (!identical(cd$failureReason, "")) {
      failureReason <- cd$failureReason
      break
    }
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (!is.finite(delta) || !all(is.finite(beta))) {
      failureReason <- "non_finite_outer_state"
      break
    }
    if (is.finite(delta) && delta < tol) {
      converged <- TRUE
      break
    }
  }
  if (isTRUE(returnDetails)) {
    return(list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason %||% "",
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_
    ))
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
                               ridge = 1e-4, betaInit = NULL,
                               returnDetails = FALSE) {
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
  iterations <- 0L
  converged <- FALSE
  for (iter in seq_len(maxIter)) {
    iterations <- iter
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
      converged <- TRUE
      break
    }
  }
  if (isTRUE(returnDetails)) {
    return(list(beta = beta, outerIterations = iterations, converged = converged))
  }
  beta
}

.pdaAdapPdaLeadCv <- function(xDesign, y, betaBar, globalGrad, globalHess,
                              lambdaSeq, useFull = TRUE,
                              foldsK = 5L, seed = 42L,
                              maxIter = 1000L, tol = 1e-6,
                              ridge = 1e-4,
                              selectionMetric = c("deviance", "auc"),
                              tieTolerance = 1e-8,
                              collectDiagnostics = FALSE) {
  selectionMetric <- match.arg(selectionMetric)
  set.seed(seed)
  n <- length(y)
  folds <- sample(rep_len(seq_len(foldsK), n))
  scores <- rep(NA_real_, length(lambdaSeq))
  warmStarts <- rep(list(betaBar), foldsK)
  diagnosticRows <- list()
  for (li in seq_along(lambdaSeq)) {
    foldLoss <- numeric(foldsK)
    for (fold in seq_len(foldsK)) {
      idxVal <- which(folds == fold)
      idxTr <- setdiff(seq_len(n), idxVal)
      fitObj <- .fitPdaAdapPdaProx(
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
        betaInit = warmStarts[[fold]],
        returnDetails = collectDiagnostics
      )
      fit <- .adapFitBeta(fitObj)
      warmStarts[[fold]] <- fit
      valStats <- if (.adapFitFailed(fitObj)) {
        .adapFailedValidationStats(selectionMetric)
      } else {
        .adapValidationStats(
          fit,
          xDesign[idxVal, , drop = FALSE],
          y[idxVal],
          metric = selectionMetric
        )
      }
      foldLoss[fold] <- unname(valStats["score"])
      if (isTRUE(collectDiagnostics)) {
        leadTerms <- if (isTRUE(useFull)) {
          .logisticNegGradientHessian(betaBar, xDesign[idxTr, , drop = FALSE], y[idxTr])
        } else {
          .logisticNegGradientHessianDiag(betaBar, xDesign[idxTr, , drop = FALSE], y[idxTr])
        }
        hStats <- if (isTRUE(useFull)) {
          .adapMatrixStats(globalHess - leadTerms$hessian, "hCorrection")
        } else {
          .adapRangeStats(globalHess - leadTerms$hessianDiag, "hCorrection")
        }
        diagnosticRows[[length(diagnosticRows) + 1L]] <<- data.frame(
          lambda = lambdaSeq[li],
          lambdaKey = format(lambdaSeq[li], digits = 17, scientific = TRUE),
          innerFold = fold,
          nTrain = length(idxTr),
          trainOutcomes = sum(y[idxTr] == 1),
          trainOutcomeRate = mean(y[idxTr] == 1),
          nValidation = length(idxVal),
          validationOutcomes = sum(y[idxVal] == 1),
          validationOutcomeRate = mean(y[idxVal] == 1),
          surrogateKind = if (isTRUE(useFull)) "pdaFull" else "pdaDiag",
          failureReason = .adapFitFailureReason(fitObj),
          t(c(.adapFitDiagnostics(fitObj), valStats, hStats)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
    scores[li] <- mean(foldLoss, na.rm = TRUE)
  }
  idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
  list(
    lambda = lambdaSeq[idx],
    scores = scores,
    diagnostics = if (length(diagnosticRows)) do.call(rbind, diagnosticRows) else NULL
  )
}

.fitPdaAdapRemoteDiagSurrogate <- function(xDesign, y, betaLead, betaBar,
                                           globalGrad, globalHessDiag,
                                           lambda,
                                           maxOuter = 100L, maxInner = 100L,
                                           tol = 1e-5, betaInit = NULL,
                                           returnDetails = FALSE,
                                           cdStepBound = 1,
                                           cdMinStep = 1e-8,
                                           cdMaxBacktracks = 25L) {
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
      tol = tol,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks)
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(
    iterations = NA_integer_,
    converged = NA,
    objective = NA_real_,
    maxAbsStep = NA_real_,
    backtracks = NA_integer_
  )
  for (iter in seq_len(maxOuter)) {
    iterations <- iter
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
    cd <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks),
      returnDetails = TRUE
    )
    beta <- cd$beta
    if (!identical(cd$failureReason, "")) {
      failureReason <- cd$failureReason
      break
    }
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (!is.finite(delta) || !all(is.finite(beta))) {
      failureReason <- "non_finite_outer_state"
      break
    }
    if (is.finite(delta) && delta < tol) {
      converged <- TRUE
      break
    }
  }
  if (isTRUE(returnDetails)) {
    return(list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason,
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_
    ))
  }
  beta
}

.fitPdaAdapFirstOrderSurrogate <- function(xDesign, y, betaLead, betaBar,
                                           globalGrad, lambda,
                                           maxOuter = 100L, maxInner = 100L,
                                           tol = 1e-5, betaInit = NULL,
                                           returnDetails = FALSE,
                                           cdStepBound = 1,
                                           cdMinStep = 1e-8,
                                           cdMaxBacktracks = 25L) {
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
      tol = tol,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks)
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(
    iterations = NA_integer_,
    converged = NA,
    objective = NA_real_,
    maxAbsStep = NA_real_,
    backtracks = NA_integer_
  )
  for (iter in seq_len(maxOuter)) {
    iterations <- iter
    old <- beta
    comp <- .adapFirstOrderSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      gradBar = gradBar
    )
    cd <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = beta,
      lambda = lambda,
      maxIter = maxInner,
      tol = tol,
      penalize = penalize,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks),
      returnDetails = TRUE
    )
    beta <- cd$beta
    if (!identical(cd$failureReason, "")) {
      failureReason <- cd$failureReason
      break
    }
    delta <- max(abs(beta - old), na.rm = TRUE)
    if (!is.finite(delta) || !all(is.finite(beta))) {
      failureReason <- "non_finite_outer_state"
      break
    }
    if (is.finite(delta) && delta < tol) {
      converged <- TRUE
      break
    }
  }
  if (isTRUE(returnDetails)) {
    return(list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason,
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_
    ))
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
                                    fitFold,
                                    collectDiagnostics = FALSE,
                                    surrogateKind = NA_character_) {
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
  diagnosticRows <- list()
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
      warmStart <- closestWarmStart(fold, lambda)
      fitObj <- fitFold(info, lambda, warmStart, collectDiagnostics = collectDiagnostics)
      fit <- .adapFitBeta(fitObj)
      lambdaFits[[fold]][[key]] <<- fit
      valStats <- if (.adapFitFailed(fitObj)) {
        .adapFailedValidationStats(selectionMetric)
      } else {
        .adapValidationStats(
          fit,
          xDesign[info$idxVal, , drop = FALSE],
          y[info$idxVal],
          metric = selectionMetric
        )
      }
      foldLoss[fold] <- unname(valStats["score"])
      if (isTRUE(collectDiagnostics)) {
        diagnosticRows <<- c(diagnosticRows, list(data.frame(
          lambda = lambda,
          lambdaKey = key,
          innerFold = fold,
          nTrain = length(info$idxTr),
          trainOutcomes = sum(y[info$idxTr] == 1),
          trainOutcomeRate = mean(y[info$idxTr] == 1),
          nValidation = length(info$idxVal),
          validationOutcomes = sum(y[info$idxVal] == 1),
          validationOutcomeRate = mean(y[info$idxVal] == 1),
          surrogateKind = surrogateKind,
          failureReason = .adapFitFailureReason(fitObj),
          t(c(
            .adapFitDiagnostics(fitObj),
            valStats,
            info$diagnostics %||% NULL
          )),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )))
      }
    }
    score <- mean(foldLoss, na.rm = TRUE)
    assign(key, list(lambda = lambda, score = score), envir = evalCache)
    score
  }

  if (identical(search, "grid") || length(lambdaSeq) < 3L) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
    return(list(
      lambda = lambdaSeq[idx],
      scores = scores,
      diagnostics = if (length(diagnosticRows)) do.call(rbind, diagnosticRows) else NULL
    ))
  }

  lambdaRange <- range(lambdaSeq[is.finite(lambdaSeq) & lambdaSeq > 0])
  if (!all(is.finite(lambdaRange)) || lambdaRange[1] <= 0 || lambdaRange[1] == lambdaRange[2]) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    idx <- .adapBestLambdaIndex(scores, lambdaSeq, selectionMetric, tieTolerance)
    return(list(
      lambda = lambdaSeq[idx],
      scores = scores,
      diagnostics = if (length(diagnosticRows)) do.call(rbind, diagnosticRows) else NULL
    ))
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
  list(
    lambda = lambdaVals[idx],
    scores = scores,
    lambdaSeq = lambdaVals,
    diagnostics = if (length(diagnosticRows)) do.call(rbind, diagnosticRows) else NULL
  )
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
                           globalAdjustment = c("leaveValOut", "pda"),
                           collectDiagnostics = FALSE,
                           cdStepBound = 1,
                           cdMinStep = 1e-8,
                           cdMaxBacktracks = 25L) {
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
        gradTrainGlobal <- (globalGrad * totalN - gradVal * info$nVal) / denom
        hessTrainGlobal <- (globalHess * totalN - hessVal * info$nVal) / denom
      } else {
        gradTrainGlobal <- globalGrad
        hessTrainGlobal <- globalHess
      }
      list(
        gradTrainGlobal = gradTrainGlobal,
        hessTrainGlobal = hessTrainGlobal,
        diagnostics = if (isTRUE(collectDiagnostics)) {
          .adapSurrogateStats(
            kind = "full",
            xTrain = xCv[info$idxTr, , drop = FALSE],
            yTrain = yCv[info$idxTr],
            betaLead = betaLead,
            betaBar = betaBar,
            globalGrad = gradTrainGlobal,
            globalHess = hessTrainGlobal
          )
        } else {
          NULL
        }
      )
    },
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE) {
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
        betaInit = warmStart,
        returnDetails = TRUE,
        cdStepBound = cdStepBound,
        cdMinStep = cdMinStep,
        cdMaxBacktracks = cdMaxBacktracks
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = "full"
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
                                globalAdjustment = c("leaveValOut", "pda"),
                                collectDiagnostics = FALSE,
                                cdStepBound = 1,
                                cdMinStep = 1e-8,
                                cdMaxBacktracks = 25L) {
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
        gradTrainGlobal <- (globalGrad * totalN - gradVal * info$nVal) / denom
      } else {
        gradTrainGlobal <- globalGrad
      }
      list(
        gradTrainGlobal = gradTrainGlobal,
        diagnostics = if (isTRUE(collectDiagnostics)) {
          .adapSurrogateStats(
            kind = "first",
            xTrain = xCv[info$idxTr, , drop = FALSE],
            yTrain = yCv[info$idxTr],
            betaLead = betaLead,
            betaBar = betaBar,
            globalGrad = gradTrainGlobal
          )
        } else {
          NULL
        }
      )
    },
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE) {
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
        betaInit = warmStart,
        returnDetails = TRUE,
        cdStepBound = cdStepBound,
        cdMinStep = cdMinStep,
        cdMaxBacktracks = cdMaxBacktracks
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = "first"
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
                               globalAdjustment = c("leaveValOut", "pda"),
                               collectDiagnostics = FALSE,
                               cdStepBound = 1,
                               cdMinStep = 1e-8,
                               cdMaxBacktracks = 25L) {
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
        gradTrainGlobal <- (globalGrad * totalN - gradVal * info$nVal) / denom
        hessTrainGlobalDiag <- (globalHessDiag * totalN - hessValDiag * info$nVal) / denom
      } else {
        gradTrainGlobal <- globalGrad
        hessTrainGlobalDiag <- globalHessDiag
      }
      list(
        gradTrainGlobal = gradTrainGlobal,
        hessTrainGlobalDiag = hessTrainGlobalDiag,
        diagnostics = if (isTRUE(collectDiagnostics)) {
          .adapSurrogateStats(
            kind = "diag",
            xTrain = xCv[info$idxTr, , drop = FALSE],
            yTrain = yCv[info$idxTr],
            betaLead = betaLead,
            betaBar = betaBar,
            globalGrad = gradTrainGlobal,
            globalHessDiag = hessTrainGlobalDiag
          )
        } else {
          NULL
        }
      )
    },
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE) {
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
        betaInit = warmStart,
        returnDetails = TRUE,
        cdStepBound = cdStepBound,
        cdMinStep = cdMinStep,
        cdMaxBacktracks = cdMaxBacktracks
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = "diag"
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
    cvDiagnostics <- NULL
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
          tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8,
          collectDiagnostics = isTRUE(config$adapCvDiagnostics)
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
        cvDiagnostics <- cv$diagnostics %||% NULL
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
        adapCvDiagnostics = cvDiagnostics,
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
        globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut",
        collectDiagnostics = isTRUE(config$adapCvDiagnostics),
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
      cvDiagnostics <- cv$diagnostics %||% NULL
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
      tol = config$tol %||% 1e-5,
      cdStepBound = config$adapCdStepBound %||% 1,
      cdMinStep = config$adapCdMinStep %||% 1e-8,
      cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
    )
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      adapCvDiagnostics = cvDiagnostics,
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
          adapCvDiagnostics = leadReport$adapCvDiagnostics %||% NULL,
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
    cvDiagnostics <- NULL
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
          globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut",
          collectDiagnostics = isTRUE(config$adapCvDiagnostics),
          cdStepBound = config$adapCdStepBound %||% 1,
          cdMinStep = config$adapCdMinStep %||% 1e-8,
          cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
        lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
        cvDiagnostics <- cv$diagnostics %||% NULL
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
        tol = config$tol %||% 1e-5,
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
      )
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        adapCvDiagnostics = cvDiagnostics,
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
          ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4,
          selectionMetric = config$lambdaSelectionMetric %||% "deviance",
          tieTolerance = config$lambdaSelectionTieTolerance %||% 1e-8,
          collectDiagnostics = isTRUE(config$adapCvDiagnostics)
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
        cvDiagnostics <- cv$diagnostics %||% NULL
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
        adapCvDiagnostics = cvDiagnostics,
        lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
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
        globalAdjustment = config$lambdaCvGlobalAdjustment %||% "leaveValOut",
        collectDiagnostics = isTRUE(config$adapCvDiagnostics),
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
      cvDiagnostics <- cv$diagnostics %||% NULL
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
      tol = config$tol %||% 1e-5,
      cdStepBound = config$adapCdStepBound %||% 1,
      cdMinStep = config$adapCdMinStep %||% 1e-8,
      cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L
    )
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      adapCvDiagnostics = cvDiagnostics,
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
          adapCvDiagnostics = leadReport$adapCvDiagnostics %||% NULL,
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
