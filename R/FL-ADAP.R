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

.adapHessianScale <- function(H) {
  diagVals <- if (is.matrix(H) || inherits(H, "Matrix")) diag(H) else as.numeric(H)
  diagVals <- diagVals[is.finite(diagVals)]
  if (!length(diagVals)) {
    return(1)
  }
  max(1, mean(abs(diagVals)))
}

.adapEigenRange <- function(B) {
  vals <- tryCatch(
    eigen((as.matrix(B) + t(as.matrix(B))) / 2, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  finite <- is.finite(vals)
  list(
    min = if (any(finite)) min(vals[finite]) else NA_real_,
    max = if (any(finite)) max(vals[finite]) else NA_real_
  )
}

.adapMaxConvAlpha <- function(globalHess, leadHess, leadWeightMin,
                              tau = 1e-10, maxIter = 60L) {
  lower <- min(max(as.numeric(leadWeightMin)[[1]], 0), 1)
  upper <- 1
  epsilon <- .adapEpsilonEig(globalHess, tau = tau)
  minEigAt <- function(alpha) .adapEigenRange(globalHess - alpha * leadHess)$min
  upperEig <- minEigAt(upper)
  lowerEig <- minEigAt(lower)
  if (is.finite(upperEig) && upperEig >= -epsilon) {
    return(list(
      alpha = upper,
      alphaMin = lower,
      alphaMax = upper,
      alphaStatus = "exact",
      alphaEigenMin = upperEig,
      alphaLowerEigenMin = lowerEig,
      alphaEpsilon = epsilon
    ))
  }
  if (!is.finite(lowerEig) || lowerEig < -epsilon) {
    return(list(
      alpha = lower,
      alphaMin = lower,
      alphaMax = upper,
      alphaStatus = "lower_not_psd",
      alphaEigenMin = lowerEig,
      alphaLowerEigenMin = lowerEig,
      alphaEpsilon = epsilon
    ))
  }
  lo <- lower
  hi <- upper
  eigMid <- lowerEig
  for (i in seq_len(maxIter)) {
    mid <- (lo + hi) / 2
    eigMid <- minEigAt(mid)
    if (is.finite(eigMid) && eigMid >= -epsilon) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  list(
    alpha = lo,
    alphaMin = lower,
    alphaMax = upper,
    alphaStatus = "maximal_convex",
    alphaEigenMin = minEigAt(lo),
    alphaLowerEigenMin = lowerEig,
    alphaEpsilon = epsilon
  )
}

.adapEpsilonEig <- function(referenceHess, tau = 1e-10) {
  tau * .adapHessianScale(referenceHess)
}

.adapEpsilonFloor <- function(referenceHess, tau = 1e-8) {
  tau * .adapHessianScale(referenceHess)
}

.adapFailureResult <- function(beta, reason, diagnostics = list()) {
  c(
    list(
      beta = as.numeric(beta),
      outerIterations = 0L,
      converged = FALSE,
      failureReason = reason,
      innerIterations = NA_integer_,
      innerConverged = NA,
      innerObjective = NA_real_,
      innerMaxAbsStep = NA_real_,
      innerBacktracks = NA_integer_,
      failingCoordinate = NA_integer_,
      coordinateCurvature = NA_real_,
      coordinateGradient = NA_real_,
      failureDiagMin = NA_real_,
      failureDiagMax = NA_real_,
      failureDiagNonPositive = NA_real_
    ),
    diagnostics
  )
}

.adapProxShift <- function(globalHess, correction, tau = 1e-8) {
  eig <- .adapEigenRange(correction)
  globalEig <- .adapEigenRange(globalHess)
  epsilonFloor <- .adapEpsilonFloor(globalHess, tau = tau)
  rho <- if (is.finite(eig$min)) max(0, epsilonFloor - eig$min) else NA_real_
  list(
    rho = rho,
    rhoOverGlobalEigenMax = if (is.finite(rho) && is.finite(globalEig$max) && globalEig$max > 0) {
      rho / globalEig$max
    } else {
      NA_real_
    },
    epsilonFloor = epsilonFloor,
    C_eigen_min = eig$min,
    C_eigen_max = eig$max,
    C_eigen_min_after_shift = if (is.finite(eig$min) && is.finite(rho)) eig$min + rho else NA_real_
  )
}

.adapDesignSummary <- function(xDesign, y) {
  n <- nrow(xDesign)
  p <- ncol(xDesign)
  out <- data.frame(
    coordinate = seq_len(p),
    nonzero = integer(p),
    mean = numeric(p),
    sd = numeric(p),
    min = numeric(p),
    max = numeric(p),
    outcomesWhenNonzero = integer(p),
    rowsWhenNonzero = integer(p)
  )
  for (j in seq_len(p)) {
    col <- as.numeric(xDesign[, j])
    finite <- is.finite(col)
    out$nonzero[j] <- sum(col != 0, na.rm = TRUE)
    out$mean[j] <- mean(col[finite])
    out$sd[j] <- stats::sd(col[finite])
    out$min[j] <- min(col[finite])
    out$max[j] <- max(col[finite])
    nz <- which(col != 0 & is.finite(col))
    out$outcomesWhenNonzero[j] <- sum(y[nz] == 1)
    out$rowsWhenNonzero[j] <- length(nz)
  }
  out$n <- n
  out
}

.adapKktStats <- function(aTilde, B, beta, lambda, penalize, tol = 1e-6) {
  grad <- as.numeric(aTilde + B %*% beta)
  violation <- numeric(length(beta))
  active <- abs(beta) > tol
  for (j in seq_along(beta)) {
    if (isTRUE(penalize[j])) {
      if (active[j]) {
        violation[j] <- abs(grad[j] + lambda * sign(beta[j]))
      } else {
        violation[j] <- max(abs(grad[j]) - lambda, 0)
      }
    } else {
      violation[j] <- abs(grad[j])
    }
  }
  finite <- is.finite(violation)
  list(
    gradient = grad,
    violation = violation,
    maxViolation = if (any(finite)) max(violation[finite]) else NA_real_,
    violating = sum(finite & violation > tol),
    maxCoordinate = if (any(finite)) which.max(violation) else NA_integer_
  )
}

.adapApplyKktStatus <- function(fit, comp, lambda, penalize,
                                kktTolerance = 1e-4,
                                betaAbsThreshold = 1e4,
                                etaAbsThreshold = 1e4,
                                xDesign = NULL) {
  beta <- .adapFitBeta(fit)
  fit$kktMaxAbs <- NA_real_
  fit$kktViolating <- NA_integer_
  fit$kktMaxCoordinate <- NA_integer_
  fit$betaMaxAbs <- if (length(beta)) max(abs(beta), na.rm = TRUE) else NA_real_
  fit$etaMaxAbs <- NA_real_
  fit$convergenceReason <- fit$convergenceReason %||% ""

  if (!length(beta) || any(!is.finite(beta))) {
    fit$failureReason <- "non_finite_solution"
    fit$converged <- FALSE
    return(fit)
  }
  if (is.finite(betaAbsThreshold) && is.finite(fit$betaMaxAbs) &&
      fit$betaMaxAbs > betaAbsThreshold) {
    fit$failureReason <- "beta_abs_too_large"
    fit$converged <- FALSE
    return(fit)
  }
  if (!is.null(xDesign)) {
    eta <- as.numeric(xDesign %*% beta)
    fit$etaMaxAbs <- if (length(eta)) max(abs(eta), na.rm = TRUE) else NA_real_
    if (any(!is.finite(eta))) {
      fit$failureReason <- "non_finite_linear_predictor"
      fit$converged <- FALSE
      return(fit)
    }
    if (is.finite(etaAbsThreshold) && is.finite(fit$etaMaxAbs) &&
        fit$etaMaxAbs > etaAbsThreshold) {
      fit$failureReason <- "eta_abs_too_large"
      fit$converged <- FALSE
      return(fit)
    }
  }

  kkt <- .adapKktStats(comp$aTilde, comp$B, beta, lambda, penalize, tol = kktTolerance)
  fit$kktMaxAbs <- kkt$maxViolation
  fit$kktViolating <- kkt$violating
  fit$kktMaxCoordinate <- kkt$maxCoordinate
  if (!is.finite(fit$kktMaxAbs)) {
    fit$failureReason <- "non_finite_kkt"
    fit$converged <- FALSE
    return(fit)
  }

  currentFailure <- fit$failureReason %||% ""
  if (fit$kktMaxAbs <= kktTolerance) {
    if (!nzchar(currentFailure) || identical(currentFailure, "max_outer_no_convergence")) {
      fit$failureReason <- ""
      fit$converged <- TRUE
      fit$convergenceReason <- "kkt_tolerance"
    }
  } else if (!nzchar(currentFailure) || isTRUE(fit$converged)) {
    fit$failureReason <- "kkt_not_satisfied"
    fit$converged <- FALSE
  }
  fit
}

.adapTraceRow <- function(iteration, betaBefore, betaAfter, xDesign, y,
                          comp, lambda, penalize, cd, failureReason = "") {
  etaBefore <- as.numeric(xDesign %*% betaBefore)
  etaAfter <- as.numeric(xDesign %*% betaAfter)
  kktBefore <- .adapKktStats(comp$aTilde, comp$B, betaBefore, lambda, penalize)
  kktAfter <- .adapKktStats(comp$aTilde, comp$B, betaAfter, lambda, penalize)
  data.frame(
    outerIteration = iteration,
    failureReason = failureReason,
    betaBeforeMaxAbs = max(abs(betaBefore), na.rm = TRUE),
    betaAfterMaxAbs = max(abs(betaAfter), na.rm = TRUE),
    betaAfterNonzero = sum(abs(betaAfter) > 1e-8, na.rm = TRUE),
    deltaMaxAbs = max(abs(betaAfter - betaBefore), na.rm = TRUE),
    etaBeforeMin = min(etaBefore, na.rm = TRUE),
    etaBeforeMax = max(etaBefore, na.rm = TRUE),
    etaBeforeMaxAbs = max(abs(etaBefore), na.rm = TRUE),
    etaAfterMin = min(etaAfter, na.rm = TRUE),
    etaAfterMax = max(etaAfter, na.rm = TRUE),
    etaAfterMaxAbs = max(abs(etaAfter), na.rm = TRUE),
    kktBeforeMax = kktBefore$maxViolation,
    kktBeforeViolating = kktBefore$violating,
    kktAfterMax = kktAfter$maxViolation,
    kktAfterViolating = kktAfter$violating,
    kktAfterMaxCoordinate = kktAfter$maxCoordinate,
    t(.adapMatrixStats(comp$B, "B")),
    innerIterations = cd$iterations %||% NA_integer_,
    innerConverged = isTRUE(cd$converged),
    innerObjective = cd$objective %||% NA_real_,
    innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
    innerBacktracks = cd$backtracks %||% NA_integer_,
    failingCoordinate = cd$failingCoordinate %||% NA_integer_,
    coordinateCurvature = cd$coordinateCurvature %||% NA_real_,
    coordinateGradient = cd$coordinateGradient %||% NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.adapTraceSnapshot <- function(label, iteration, betaBefore, betaAfter,
                               comp, fixed, cd, lambda, penalize) {
  list(
    label = label,
    outerIteration = iteration,
    lambda = lambda,
    penalize = penalize,
    betaBefore = betaBefore,
    betaAfter = betaAfter,
    aTilde = comp$aTilde,
    B = comp$B,
    fixed = fixed,
    cd = cd,
    kktBefore = .adapKktStats(comp$aTilde, comp$B, betaBefore, lambda, penalize),
    kktAfter = .adapKktStats(comp$aTilde, comp$B, betaAfter, lambda, penalize)
  )
}

.fitAdapTraceSurrogate <- function(kind, xDesign, y, betaLead, betaBar,
                                   globalGrad, lambda,
                                   globalHess = NULL, globalHessDiag = NULL,
                                   maxOuter = 100L, maxInner = 100L,
                                   tol = 1e-5, betaInit = NULL,
                                   cdStepBound = 1, cdMinStep = 1e-8,
                                   cdMaxBacktracks = 25L,
                                   traceContext = list()) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  fixed <- switch(kind,
    full = {
      hBar <- .logisticNegHessian(betaBar, xDesign)
      list(
        kind = "full",
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHess,
        gradBar = gradBar,
        leadHessAtBetaBar = hBar,
        hessianCorrection = globalHess - hBar,
        gradientCorrection = globalGrad - gradBar
      )
    },
    diag = {
      hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
      list(
        kind = "diag",
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        globalHessDiag = globalHessDiag,
        gradBar = gradBar,
        leadHessDiagAtBetaBar = hBarDiag,
        diagCorrection = globalHessDiag - hBarDiag,
        gradientCorrection = globalGrad - gradBar
      )
    },
    first = list(
      kind = "first",
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      gradBar = gradBar,
      gradientCorrection = globalGrad - gradBar
    ),
    stop("Unsupported ADAP trace kind: ", kind, call. = FALSE)
  )

  makeComp <- function(betaEval) {
    switch(kind,
      full = {
        evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
        hCorr <- fixed$hessianCorrection
        list(
          aTilde = as.numeric(evalTerms$gradient -
            as.numeric(t(betaEval) %*% evalTerms$hessian) +
            globalGrad - gradBar -
            as.numeric(t(betaBar) %*% hCorr)),
          B = evalTerms$hessian + hCorr,
          localGradient = evalTerms$gradient,
          localHessian = evalTerms$hessian,
          hessianCorrection = hCorr
        )
      },
      diag = {
        evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
        corr <- fixed$diagCorrection
        list(
          aTilde = as.numeric(evalTerms$gradient -
            as.numeric(t(betaEval) %*% evalTerms$hessian) +
            globalGrad - gradBar -
            betaBar * corr),
          B = evalTerms$hessian + diag(corr, length(corr), length(corr)),
          localGradient = evalTerms$gradient,
          localHessian = evalTerms$hessian,
          diagCorrection = corr
        )
      },
      first = {
        evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
        list(
          aTilde = as.numeric(evalTerms$gradient -
            as.numeric(t(betaEval) %*% evalTerms$hessian) +
            fixed$gradientCorrection),
          B = evalTerms$hessian,
          localGradient = evalTerms$gradient,
          localHessian = evalTerms$hessian,
          gradientCorrection = fixed$gradientCorrection
        )
      }
    )
  }

  rows <- list()
  snapshots <- list()
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(iterations = NA_integer_, converged = NA, objective = NA_real_)
  firstBadSnapshot <- FALSE

  for (iter in seq_len(maxOuter)) {
    iterations <- iter
    betaBefore <- beta
    comp <- makeComp(betaBefore)
    cd <- .coordDescentQuadraticLasso(
      aTilde = comp$aTilde,
      B = comp$B,
      betaInit = betaBefore,
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
    cdFailure <- .adapFitFailureReason(cd)
    row <- .adapTraceRow(iter, betaBefore, beta, xDesign, y, comp, lambda, penalize, cd, cdFailure)
    rows[[length(rows) + 1L]] <- row
    problem <- nzchar(cdFailure) || isTRUE(row$BEigenNonPositive > 0) ||
      !is.finite(row$deltaMaxAbs) || !is.finite(row$betaAfterMaxAbs)
    if (iter == 1L || (problem && !firstBadSnapshot)) {
      label <- if (iter == 1L) "first" else "firstProblem"
      snapshots[[length(snapshots) + 1L]] <- .adapTraceSnapshot(
        label, iter, betaBefore, beta, comp, fixed, cd, lambda, penalize
      )
      firstBadSnapshot <- firstBadSnapshot || problem
    }
    if (nzchar(cdFailure)) {
      failureReason <- cdFailure
      break
    }
    delta <- max(abs(beta - betaBefore), na.rm = TRUE)
    if (!is.finite(delta) || !all(is.finite(beta))) {
      failureReason <- "non_finite_outer_state"
      break
    }
    if (is.finite(delta) && delta < tol) {
      converged <- TRUE
      break
    }
  }

  if (length(rows) > 0L) {
    lastRow <- rows[[length(rows)]]
    snapshotIterations <- vapply(snapshots, function(x) x$outerIteration, numeric(1))
    if (!any(snapshotIterations == lastRow$outerIteration)) {
      comp <- makeComp(beta)
      snapshots[[length(snapshots) + 1L]] <- .adapTraceSnapshot(
        "last", lastRow$outerIteration, beta, beta, comp, fixed, cd, lambda, penalize
      )
    }
  }

  list(
    beta = beta,
    outerIterations = iterations,
    converged = converged,
    failureReason = failureReason,
    innerIterations = cd$iterations %||% NA_integer_,
    innerConverged = cd$converged %||% NA,
    innerObjective = cd$objective %||% NA_real_,
    innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
    innerBacktracks = cd$backtracks %||% NA_integer_,
    failingCoordinate = cd$failingCoordinate %||% NA_integer_,
    coordinateCurvature = cd$coordinateCurvature %||% NA_real_,
    coordinateGradient = cd$coordinateGradient %||% NA_real_,
    failureDiagMin = cd$failureDiagMin %||% NA_real_,
    failureDiagMax = cd$failureDiagMax %||% NA_real_,
    failureDiagNonPositive = cd$failureDiagNonPositive %||% NA_real_,
    trace = list(
      context = traceContext,
      kind = kind,
      lambda = lambda,
      xSummary = .adapDesignSummary(xDesign, y),
      ySummary = list(n = length(y), outcomes = sum(y == 1), outcomeRate = mean(y == 1)),
      fixed = fixed,
      rows = if (length(rows)) do.call(rbind, rows) else data.frame(),
      snapshots = snapshots
    )
  )
}

.adapSurrogateStats <- function(kind, xTrain, yTrain, betaLead, betaBar,
                                globalGrad, globalHess = NULL,
                                globalHessDiag = NULL,
                                leadWeight = 1) {
  tryCatch({
    comp <- switch(kind,
      full = .adapSurrogateComponents(
        betaEval = betaLead,
        betaBar = betaBar,
        xDesign = xTrain,
        y = yTrain,
        globalGrad = globalGrad,
        globalHess = globalHess,
        leadWeight = leadWeight
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
  if (is.list(fit) && !is.null(fit$converged) && isFALSE(fit$converged)) {
    return("max_outer_no_convergence")
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
    kktMaxAbs = if (is.list(fit) && !is.null(fit$kktMaxAbs)) fit$kktMaxAbs else NA_real_,
    kktViolating = if (is.list(fit) && !is.null(fit$kktViolating)) fit$kktViolating else NA_real_,
    kktMaxCoordinate = if (is.list(fit) && !is.null(fit$kktMaxCoordinate)) fit$kktMaxCoordinate else NA_real_,
    etaMaxAbs = if (is.list(fit) && !is.null(fit$etaMaxAbs)) fit$etaMaxAbs else NA_real_,
    innerIterations = if (is.list(fit) && !is.null(fit$innerIterations)) fit$innerIterations else NA_real_,
    innerConverged = if (is.list(fit) && !is.null(fit$innerConverged)) isTRUE(fit$innerConverged) else NA,
    innerObjective = if (is.list(fit) && !is.null(fit$innerObjective)) fit$innerObjective else NA_real_,
    innerMaxAbsStep = if (is.list(fit) && !is.null(fit$innerMaxAbsStep)) fit$innerMaxAbsStep else NA_real_,
    innerBacktracks = if (is.list(fit) && !is.null(fit$innerBacktracks)) fit$innerBacktracks else NA_real_,
    failingCoordinate = if (is.list(fit) && !is.null(fit$failingCoordinate)) fit$failingCoordinate else NA_real_,
    coordinateCurvature = if (is.list(fit) && !is.null(fit$coordinateCurvature)) fit$coordinateCurvature else NA_real_,
    coordinateGradient = if (is.list(fit) && !is.null(fit$coordinateGradient)) fit$coordinateGradient else NA_real_,
    failureDiagMin = if (is.list(fit) && !is.null(fit$failureDiagMin)) fit$failureDiagMin else NA_real_,
    failureDiagMax = if (is.list(fit) && !is.null(fit$failureDiagMax)) fit$failureDiagMax else NA_real_,
    failureDiagNonPositive = if (is.list(fit) && !is.null(fit$failureDiagNonPositive)) fit$failureDiagNonPositive else NA_real_
  )
}

.adapFitDiagnosticsForReport <- function(fit) {
  out <- as.list(.adapFitDiagnostics(fit))
  out$failureReason <- .adapFitFailureReason(fit)
  out
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

.adapCvFailureMessage <- function(method, scores, diagnostics = NULL) {
  nScores <- length(scores)
  reasonSummary <- "no failure diagnostics collected"
  if (!is.null(diagnostics) && nrow(diagnostics) > 0L && "failureReason" %in% names(diagnostics)) {
    reasons <- diagnostics$failureReason
    reasons[is.na(reasons) | !nzchar(reasons)] <- "<none>"
    tab <- sort(table(reasons), decreasing = TRUE)
    reasonSummary <- paste(paste(names(tab), as.integer(tab), sep = "="), collapse = ", ")
  }
  coordinateSummary <- ""
  if (!is.null(diagnostics) && nrow(diagnostics) > 0L &&
      all(c("failingCoordinate", "coordinateCurvature", "coordinateGradient") %in% names(diagnostics))) {
    failed <- diagnostics[!is.na(diagnostics$failingCoordinate), , drop = FALSE]
    if (nrow(failed) > 0L) {
      coordinateSummary <- sprintf(
        "; first failing coordinate=%s curvature=%s gradient=%s",
        failed$failingCoordinate[1],
        signif(failed$coordinateCurvature[1], 6),
        signif(failed$coordinateGradient[1], 6)
      )
    }
  }
  sprintf(
    "%s lambda CV failed: no candidate lambda had successful inner fits across all folds (%s candidates). Reasons: %s%s",
    method,
    nScores,
    reasonSummary,
    coordinateSummary
  )
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
          backtracks = NA_integer_,
          failingCoordinate = NA_integer_,
          coordinateCurvature = NA_real_,
          coordinateGradient = NA_real_,
          failureDiagMin = NA_real_,
          failureDiagMax = NA_real_,
          failureDiagNonPositive = NA_real_
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
      backtracks = NA_integer_,
      failingCoordinate = NA_integer_,
      coordinateCurvature = NA_real_,
      coordinateGradient = NA_real_,
      failureDiagMin = NA_real_,
      failureDiagMax = NA_real_,
      failureDiagNonPositive = NA_real_
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
  curvatureTol <- 1e-14
  fail <- function(reason, iter = 0L, objective = NA_real_, maxAbsStep = NA_real_, backtracks = NA_integer_,
                   coordinate = NA_integer_, curvature = NA_real_, gradient = NA_real_) {
    if (isTRUE(returnDetails)) {
      return(list(
        beta = beta,
        iterations = iter,
        converged = FALSE,
        failureReason = reason,
        objective = objective,
        maxAbsStep = maxAbsStep,
        backtracks = backtracks,
        failingCoordinate = coordinate,
        coordinateCurvature = curvature,
        coordinateGradient = gradient,
        failureDiagMin = suppressWarnings(min(diagB, na.rm = TRUE)),
        failureDiagMax = suppressWarnings(max(diagB, na.rm = TRUE)),
        failureDiagNonPositive = sum(is.finite(diagB) & diagB <= 0)
      ))
    }
    stop(reason, call. = FALSE)
  }
  if (!all(is.finite(c(aTilde, as.numeric(B), beta, lambda)))) {
    return(fail("non_finite_surrogate_input"))
  }
  badCurvature <- which(!is.finite(diagB) | diagB < -curvatureTol)
  if (length(badCurvature) > 0L) {
    j <- badCurvature[1]
    reason <- if (!is.finite(diagB[j])) "non_finite_coordinate_curvature" else "non_positive_coordinate_curvature"
    return(fail(reason, coordinate = j, curvature = diagB[j]))
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
      smoothGrad <- aTilde[j] + sum(as.numeric(B[j, ]) * beta)
      if (!is.finite(hjj) || hjj < -curvatureTol) {
        return(fail(
          "non_positive_coordinate_curvature", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
      }
      if (abs(hjj) <= curvatureTol) {
        if (!is.finite(smoothGrad)) {
          return(fail(
            "non_finite_coordinate_update", iter, currentObjective, maxAbsStep, backtracks,
            coordinate = j, curvature = hjj, gradient = smoothGrad
          ))
        }
        if (penalize[j]) {
          kktTol <- lambda + 1e-10 * (abs(currentObjective) + 1)
          if (abs(smoothGrad) <= kktTol) {
            if (abs(beta[j]) <= minStep) {
              next
            }
            oldBetaJ <- beta[j]
            step <- -oldBetaJ
            trialObjective <- currentObjective + smoothGrad * step - lambda * abs(oldBetaJ)
            descentTol <- 1e-12 * (abs(currentObjective) + 1)
            if (is.finite(trialObjective) && trialObjective <= currentObjective + descentTol) {
              beta[j] <- 0
              currentObjective <- trialObjective
              maxAbsStep <- max(maxAbsStep, abs(step))
              next
            }
          }
          return(fail(
            "zero_coordinate_curvature_unbounded", iter, currentObjective, maxAbsStep, backtracks,
            coordinate = j, curvature = hjj, gradient = smoothGrad
          ))
        }
        if (abs(smoothGrad) <= 1e-10 * (abs(currentObjective) + 1)) {
          next
        }
        return(fail(
          "zero_unpenalized_coordinate_curvature", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
      }
      b <- aTilde[j] + sum(as.numeric(B[j, ]) * beta) - hjj * beta[j]
      z <- -b / hjj
      if (!is.finite(z)) {
        return(fail(
          "non_finite_coordinate_update", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
      }
      proposed <- if (penalize[j]) .softScalar(z, lambda / hjj) else z
      step <- proposed - beta[j]
      if (!is.finite(step)) {
        return(fail(
          "non_finite_coordinate_step", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
      }
      step <- max(min(step, stepBounds[j]), -stepBounds[j])
      if (abs(step) <= minStep) {
        next
      }
      oldBetaJ <- beta[j]
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
        return(fail(
          "non_descent_coordinate_step", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
      }
      beta[j] <- oldBetaJ + step
      currentObjective <- acceptedObjective
      maxAbsStep <- max(maxAbsStep, abs(step))
      stepBounds[j] <- max(2 * abs(step), stepBounds[j] / 2, minStep)
      if (!all(is.finite(beta)) || !is.finite(currentObjective)) {
        return(fail(
          "non_finite_coordinate_state", iter, currentObjective, maxAbsStep, backtracks,
          coordinate = j, curvature = hjj, gradient = smoothGrad
        ))
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
          backtracks = backtracks,
          failingCoordinate = NA_integer_,
          coordinateCurvature = NA_real_,
          coordinateGradient = NA_real_,
          failureDiagMin = suppressWarnings(min(diagB, na.rm = TRUE)),
          failureDiagMax = suppressWarnings(max(diagB, na.rm = TRUE)),
          failureDiagNonPositive = sum(is.finite(diagB) & diagB <= 0)
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
      backtracks = backtracks,
      failingCoordinate = NA_integer_,
      coordinateCurvature = NA_real_,
      coordinateGradient = NA_real_,
      failureDiagMin = suppressWarnings(min(diagB, na.rm = TRUE)),
      failureDiagMax = suppressWarnings(max(diagB, na.rm = TRUE)),
      failureDiagNonPositive = sum(is.finite(diagB) & diagB <= 0)
    ))
  }
  beta
}

.adapSurrogateComponents <- function(betaEval, betaBar, xDesign, y,
                                     globalGrad, globalHess,
                                     gradBar = NULL, hBar = NULL,
                                     leadWeight = 1, proxRho = 0) {
  evalTerms <- .logisticNegGradientHessian(betaEval, xDesign, y)
  hEval <- evalTerms$hessian
  if (is.null(hBar)) {
    hBar <- .logisticNegHessian(betaBar, xDesign)
  }
  if (is.null(gradBar)) {
    gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  }
  correction <- globalHess - leadWeight * hBar
  if (is.finite(proxRho) && proxRho != 0) {
    correction <- correction + diag(proxRho, nrow(correction), ncol(correction))
  }
  B <- leadWeight * hEval + correction
  aTilde <- leadWeight * evalTerms$gradient -
    as.numeric(t(betaEval) %*% (leadWeight * hEval)) +
    globalGrad -
    leadWeight * gradBar -
    as.numeric(t(betaBar) %*% correction)
  list(aTilde = as.numeric(aTilde), B = B, correction = correction)
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
                                 cdMaxBacktracks = 25L,
                                 traceDiagnostics = FALSE,
                                 traceContext = list(),
                                 leadWeight = 1,
                                 proxRho = 0,
                                 strictCorrection = c("exact", "prox", "convex", "maxconv"),
                                 eigToleranceTau = 1e-10,
                                 proxTau = 1e-8,
                                 kktTolerance = 1e-4,
                                 betaAbsThreshold = 1e4,
                                 etaAbsThreshold = 1e4) {
  strictCorrection <- match.arg(strictCorrection)
  beta <- if (is.null(betaInit)) betaLead else betaInit
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBar <- .logisticNegHessian(betaBar, xDesign)
  baseCorrection <- globalHess - leadWeight * hBar
  eig <- .adapEigenRange(baseCorrection)
  epsilonEig <- .adapEpsilonEig(globalHess, tau = eigToleranceTau)
  epsilonFloor <- NA_real_
  rhoOverGlobalEigenMax <- NA_real_
  C_eigen_min_after_shift <- NA_real_
  if (identical(strictCorrection, "exact") && is.finite(eig$min) && eig$min < -epsilonEig) {
    fail <- .adapFailureResult(
      beta,
      "negative_C_eigenvalue",
      list(
        C_eigen_min = eig$min,
        C_eigen_max = eig$max,
        epsilonEig = epsilonEig,
        leadWeight = leadWeight,
        correction_diag_min = min(diag(baseCorrection), na.rm = TRUE),
        correction_diag_max = max(diag(baseCorrection), na.rm = TRUE)
      )
    )
    if (isTRUE(returnDetails)) {
      return(fail)
    }
    stop("negative_C_eigenvalue", call. = FALSE)
  }
  if (strictCorrection %in% c("convex", "maxconv") && is.finite(eig$min) && eig$min < -epsilonEig) {
    reason <- if (identical(strictCorrection, "maxconv")) {
      "maxconv_correction_not_psd"
    } else {
      "remote_hessian_not_psd"
    }
    fail <- .adapFailureResult(
      beta,
      reason,
      list(
        R_eigen_min = eig$min,
        R_eigen_max = eig$max,
        epsilonEig = epsilonEig,
        leadWeight = leadWeight,
        R_diag_min = min(diag(baseCorrection), na.rm = TRUE),
        R_diag_max = max(diag(baseCorrection), na.rm = TRUE)
      )
    )
    if (isTRUE(returnDetails)) {
      return(fail)
    }
    stop(reason, call. = FALSE)
  }
  if (identical(strictCorrection, "prox")) {
    shift <- .adapProxShift(globalHess, baseCorrection, tau = proxTau)
    proxRho <- shift$rho
    rhoOverGlobalEigenMax <- shift$rhoOverGlobalEigenMax
    epsilonFloor <- shift$epsilonFloor
    C_eigen_min_after_shift <- shift$C_eigen_min_after_shift
    if (!is.finite(proxRho)) {
      fail <- .adapFailureResult(beta, "non_finite_prox_rho", shift)
      if (isTRUE(returnDetails)) {
        return(fail)
      }
      stop("non_finite_prox_rho", call. = FALSE)
    }
  }
  if (isTRUE(traceDiagnostics) && identical(strictCorrection, "exact") &&
      isTRUE(all.equal(leadWeight, 1)) && isTRUE(all.equal(proxRho, 0))) {
    out <- .fitAdapTraceSurrogate(
      kind = "full",
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = globalHess,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol,
      betaInit = beta,
      cdStepBound = cdStepBound,
      cdMinStep = cdMinStep,
      cdMaxBacktracks = cdMaxBacktracks,
      traceContext = traceContext
    )
    out$C_eigen_min <- eig$min
    out$C_eigen_max <- eig$max
    out$leadWeight <- leadWeight
    out$rho <- proxRho
    out$rhoOverGlobalEigenMax <- rhoOverGlobalEigenMax
    out$proxAnchorsIntercept <- is.finite(proxRho) && proxRho > 0
    out$epsilonFloor <- epsilonFloor
    out$C_eigen_min_after_shift <- C_eigen_min_after_shift
    finalComp <- .adapSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHess = globalHess,
      gradBar = gradBar,
      hBar = hBar,
      leadWeight = leadWeight,
      proxRho = proxRho
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
  if (inherits(xDesign, "sparseMatrix")) {
    out <- adapFullSurrogateFitCpp(
      x = .asDgCMatrix(xDesign),
      y = y,
      betaStart = beta,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = as.matrix(globalHess),
      gradBar = gradBar,
      hBar = as.matrix(hBar),
      leadWeight = leadWeight,
      proxRho = proxRho,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol,
      initialStepBound = cdStepBound,
      minStep = cdMinStep,
      maxBacktracks = as.integer(cdMaxBacktracks)
    )
    if (!isTRUE(out$converged) && !nzchar(out$failureReason %||% "")) {
      out$failureReason <- "max_outer_no_convergence"
    }
    finalComp <- .adapSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHess = globalHess,
      gradBar = gradBar,
      hBar = hBar,
      leadWeight = leadWeight,
      proxRho = proxRho
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    out$C_eigen_min <- eig$min
    out$C_eigen_max <- eig$max
    out$leadWeight <- leadWeight
    out$rho <- proxRho
    out$rhoOverGlobalEigenMax <- rhoOverGlobalEigenMax
    out$proxAnchorsIntercept <- is.finite(proxRho) && proxRho > 0
    out$epsilonFloor <- epsilonFloor
    out$C_eigen_min_after_shift <- C_eigen_min_after_shift
    out$correction_diag_min <- min(diag(baseCorrection), na.rm = TRUE)
    out$correction_diag_max <- max(diag(baseCorrection), na.rm = TRUE)
    if (isTRUE(returnDetails)) {
      return(out)
    }
    if (.adapFitFailed(out)) {
      stop(.adapFitFailureReason(out), call. = FALSE)
    }
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(
    iterations = NA_integer_,
    converged = NA,
    objective = NA_real_,
    maxAbsStep = NA_real_,
    backtracks = NA_integer_,
    failingCoordinate = NA_integer_,
    coordinateCurvature = NA_real_,
    coordinateGradient = NA_real_,
    failureDiagMin = NA_real_,
    failureDiagMax = NA_real_,
    failureDiagNonPositive = NA_real_
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
      hBar = hBar,
      leadWeight = leadWeight,
      proxRho = proxRho
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
    if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
      failureReason <- "max_outer_no_convergence"
    }
    out <- list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason %||% "",
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_,
      failingCoordinate = cd$failingCoordinate %||% NA_integer_,
      coordinateCurvature = cd$coordinateCurvature %||% NA_real_,
      coordinateGradient = cd$coordinateGradient %||% NA_real_,
      failureDiagMin = cd$failureDiagMin %||% NA_real_,
      failureDiagMax = cd$failureDiagMax %||% NA_real_,
      failureDiagNonPositive = cd$failureDiagNonPositive %||% NA_real_,
      C_eigen_min = eig$min,
      C_eigen_max = eig$max,
      leadWeight = leadWeight,
      rho = proxRho,
      rhoOverGlobalEigenMax = rhoOverGlobalEigenMax,
      proxAnchorsIntercept = is.finite(proxRho) && proxRho > 0,
      epsilonFloor = epsilonFloor,
      C_eigen_min_after_shift = C_eigen_min_after_shift,
      correction_diag_min = min(diag(baseCorrection), na.rm = TRUE),
      correction_diag_max = max(diag(baseCorrection), na.rm = TRUE)
    )
    finalComp <- .adapSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHess = globalHess,
      gradBar = gradBar,
      hBar = hBar,
      leadWeight = leadWeight,
      proxRho = proxRho
    )
    return(.adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    ))
  }
  if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
    stop("max_outer_no_convergence", call. = FALSE)
  }
  if (nzchar(failureReason %||% "")) {
    stop(failureReason, call. = FALSE)
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
                                           cdMaxBacktracks = 25L,
                                           traceDiagnostics = FALSE,
                                           traceContext = list(),
                                           diagToleranceTau = 1e-10,
                                           kktTolerance = 1e-4,
                                           betaAbsThreshold = 1e4,
                                           etaAbsThreshold = 1e4) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  gradBar <- .logisticNegGradient(betaBar, xDesign, y)
  hBarDiag <- .logisticNegHessianDiag(betaBar, xDesign)
  correctionDiag <- globalHessDiag - hBarDiag
  epsilonDiag <- diagToleranceTau * max(1, stats::median(abs(globalHessDiag[is.finite(globalHessDiag)]), na.rm = TRUE))
  if (!is.finite(epsilonDiag)) {
    epsilonDiag <- diagToleranceTau
  }
  if (any(is.finite(correctionDiag) & correctionDiag < -epsilonDiag)) {
    fail <- .adapFailureResult(
      beta,
      "negative_diagonal_correction",
      list(
        correction_diag_min = min(correctionDiag, na.rm = TRUE),
        correction_diag_max = max(correctionDiag, na.rm = TRUE),
        epsilonDiag = epsilonDiag
      )
    )
    if (isTRUE(returnDetails)) {
      return(fail)
    }
    stop("negative_diagonal_correction", call. = FALSE)
  }
  if (isTRUE(traceDiagnostics)) {
    out <- .fitAdapTraceSurrogate(
      kind = "diag",
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol,
      betaInit = beta,
      cdStepBound = cdStepBound,
      cdMinStep = cdMinStep,
      cdMaxBacktracks = cdMaxBacktracks,
      traceContext = traceContext
    )
    finalComp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      gradBar = gradBar,
      hBarDiag = hBarDiag
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
  if (inherits(xDesign, "sparseMatrix")) {
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
    if (!isTRUE(out$converged) && !nzchar(out$failureReason %||% "")) {
      out$failureReason <- "max_outer_no_convergence"
    }
    finalComp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      gradBar = gradBar,
      hBarDiag = hBarDiag
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    out$correction_diag_min <- min(correctionDiag, na.rm = TRUE)
    out$correction_diag_max <- max(correctionDiag, na.rm = TRUE)
    out$epsilonDiag <- epsilonDiag
    if (isTRUE(returnDetails)) {
      return(out)
    }
    if (.adapFitFailed(out)) {
      stop(.adapFitFailureReason(out), call. = FALSE)
    }
    return(as.numeric(out$beta))
  }
  penalize <- rep(TRUE, length(beta))
  penalize[1] <- FALSE
  iterations <- 0L
  converged <- FALSE
  failureReason <- ""
  cd <- list(
    iterations = NA_integer_,
    converged = NA,
    objective = NA_real_,
    maxAbsStep = NA_real_,
    backtracks = NA_integer_,
    failingCoordinate = NA_integer_,
    coordinateCurvature = NA_real_,
    coordinateGradient = NA_real_,
    failureDiagMin = NA_real_,
    failureDiagMax = NA_real_,
    failureDiagNonPositive = NA_real_
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
    if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
      failureReason <- "max_outer_no_convergence"
    }
    out <- list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason,
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_,
      failingCoordinate = cd$failingCoordinate %||% NA_integer_,
      coordinateCurvature = cd$coordinateCurvature %||% NA_real_,
      coordinateGradient = cd$coordinateGradient %||% NA_real_,
      failureDiagMin = cd$failureDiagMin %||% NA_real_,
      failureDiagMax = cd$failureDiagMax %||% NA_real_,
      failureDiagNonPositive = cd$failureDiagNonPositive %||% NA_real_,
      correction_diag_min = min(correctionDiag, na.rm = TRUE),
      correction_diag_max = max(correctionDiag, na.rm = TRUE),
      epsilonDiag = epsilonDiag
    )
    finalComp <- .adapLocalFullRemoteDiagSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      gradBar = gradBar,
      hBarDiag = hBarDiag
    )
    return(.adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    ))
  }
  if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
    stop("max_outer_no_convergence", call. = FALSE)
  }
  if (nzchar(failureReason %||% "")) {
    stop(failureReason, call. = FALSE)
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
                                           cdMaxBacktracks = 25L,
                                           traceDiagnostics = FALSE,
                                           traceContext = list(),
                                           kktTolerance = 1e-4,
                                           betaAbsThreshold = 1e4,
                                           etaAbsThreshold = 1e4) {
  beta <- if (is.null(betaInit)) betaLead else betaInit
  if (isTRUE(traceDiagnostics)) {
    out <- .fitAdapTraceSurrogate(
      kind = "first",
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      lambda = lambda,
      maxOuter = maxOuter,
      maxInner = maxInner,
      tol = tol,
      betaInit = beta,
      cdStepBound = cdStepBound,
      cdMinStep = cdMinStep,
      cdMaxBacktracks = cdMaxBacktracks,
      traceContext = traceContext
    )
    finalComp <- .adapFirstOrderSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      gradBar = .logisticNegGradient(betaBar, xDesign, y)
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    return(as.numeric(out$beta))
  }
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
    if (!isTRUE(out$converged) && !nzchar(out$failureReason %||% "")) {
      out$failureReason <- "max_outer_no_convergence"
    }
    finalComp <- .adapFirstOrderSurrogateComponents(
      betaEval = .adapFitBeta(out),
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      gradBar = gradBar
    )
    penalize <- rep(TRUE, length(beta))
    penalize[1] <- FALSE
    out <- .adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    )
    if (isTRUE(returnDetails)) {
      return(out)
    }
    if (.adapFitFailed(out)) {
      stop(.adapFitFailureReason(out), call. = FALSE)
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
    backtracks = NA_integer_,
    failingCoordinate = NA_integer_,
    coordinateCurvature = NA_real_,
    coordinateGradient = NA_real_,
    failureDiagMin = NA_real_,
    failureDiagMax = NA_real_,
    failureDiagNonPositive = NA_real_
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
    if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
      failureReason <- "max_outer_no_convergence"
    }
    out <- list(
      beta = beta,
      outerIterations = iterations,
      converged = converged,
      failureReason = failureReason,
      innerIterations = cd$iterations %||% NA_integer_,
      innerConverged = cd$converged %||% NA,
      innerObjective = cd$objective %||% NA_real_,
      innerMaxAbsStep = cd$maxAbsStep %||% NA_real_,
      innerBacktracks = cd$backtracks %||% NA_integer_,
      failingCoordinate = cd$failingCoordinate %||% NA_integer_,
      coordinateCurvature = cd$coordinateCurvature %||% NA_real_,
      coordinateGradient = cd$coordinateGradient %||% NA_real_,
      failureDiagMin = cd$failureDiagMin %||% NA_real_,
      failureDiagMax = cd$failureDiagMax %||% NA_real_,
      failureDiagNonPositive = cd$failureDiagNonPositive %||% NA_real_
    )
    finalComp <- .adapFirstOrderSurrogateComponents(
      betaEval = beta,
      betaBar = betaBar,
      xDesign = xDesign,
      y = y,
      globalGrad = globalGrad,
      gradBar = gradBar
    )
    return(.adapApplyKktStatus(
      out, finalComp, lambda, penalize,
      kktTolerance = kktTolerance,
      betaAbsThreshold = betaAbsThreshold,
      etaAbsThreshold = etaAbsThreshold,
      xDesign = xDesign
    ))
  }
  if (!isTRUE(converged) && !nzchar(failureReason %||% "")) {
    stop("max_outer_no_convergence", call. = FALSE)
  }
  if (nzchar(failureReason %||% "")) {
    stop(failureReason, call. = FALSE)
  }
  beta
}

.pdaAdapLambdaSeq <- function(xDesign, y, betaLead, betaBar, globalGrad, globalHess,
                              gridLen = 100L, leadWeight = 1, proxRho = 0) {
  comp <- .adapSurrogateComponents(
    betaEval = betaLead,
    betaBar = betaBar,
    xDesign = xDesign,
    y = y,
    globalGrad = globalGrad,
    globalHess = globalHess,
    leadWeight = leadWeight,
    proxRho = proxRho
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
                                    surrogateKind = NA_character_,
                                    collectTrace = FALSE,
                                    traceContext = list(),
                                    traceFile = NULL) {
  search <- match.arg(search)
  selectionMetric <- match.arg(selectionMetric)
  lambdaSeq <- sort(unique(lambdaSeq[is.finite(lambdaSeq) & lambdaSeq > 0]), decreasing = TRUE)
  if (length(lambdaSeq) == 0L) {
    stop("lambdaSeq must contain at least one positive finite value", call. = FALSE)
  }
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
  traceRows <- list()
  lambdaFits <- vector("list", foldsK)
  for (fold in seq_len(foldsK)) {
    lambdaFits[[fold]] <- list()
  }
  lambdaKey <- function(lambda) sprintf("%.17e", unname(as.numeric(lambda)))
  closestWarmStart <- function(fold, lambda) {
    lambda <- unname(as.numeric(lambda))[1]
    fits <- lambdaFits[[fold]]
    if (length(fits) == 0L) {
      return(betaInit)
    }
    fitLambdas <- as.numeric(names(fits))
    stronger <- is.finite(fitLambdas) & fitLambdas >= lambda
    if (!any(stronger)) {
      return(betaInit)
    }
    fits <- fits[stronger]
    fitLambdas <- fitLambdas[stronger]
    idx <- which.min(abs(log(fitLambdas) - log(lambda)))
    fits[[idx]]
  }
  evaluateLambda <- function(lambda) {
    lambda <- unname(as.numeric(lambda))[1]
    key <- lambdaKey(lambda)
    if (exists(key, envir = evalCache, inherits = FALSE)) {
      return(get(key, envir = evalCache, inherits = FALSE)$score)
    }
    foldLoss <- numeric(foldsK)
    foldFailed <- logical(foldsK)
    for (fold in seq_len(foldsK)) {
      info <- foldInfo[[fold]]
      warmStart <- closestWarmStart(fold, lambda)
      fitObj <- fitFold(
        info,
        lambda,
        warmStart,
        collectDiagnostics = collectDiagnostics,
        collectTrace = collectTrace
      )
      fit <- .adapFitBeta(fitObj)
      foldFailed[fold] <- .adapFitFailed(fitObj)
      if (!foldFailed[fold]) {
        lambdaFits[[fold]][[key]] <<- fit
      }
      if (isTRUE(collectTrace) && !is.null(fitObj$trace)) {
        traceRows[[length(traceRows) + 1L]] <<- c(
          list(
            lambda = lambda,
            lambdaKey = key,
            innerFold = fold,
            surrogateKind = surrogateKind,
            nTrain = length(info$idxTr),
            trainOutcomes = sum(y[info$idxTr] == 1),
            nValidation = length(info$idxVal),
            validationOutcomes = sum(y[info$idxVal] == 1)
          ),
          list(trace = fitObj$trace)
        )
      }
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
          leadWeight = info$leadWeightTrain %||% NA_real_,
          proxRho = info$proxRhoTrain %||% NA_real_,
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
    score <- unname(mean(foldLoss, na.rm = TRUE))
    assign(key, list(lambda = lambda, score = score, valid = !any(foldFailed)), envir = evalCache)
    score
  }

  cvResult <- function(lambdaVals, scores) {
    evaluated <- as.list(evalCache)
    if (length(evaluated) > 0L) {
      validByKey <- vapply(evaluated, function(x) isTRUE(x$valid), logical(1))
      names(validByKey) <- vapply(evaluated, function(x) lambdaKey(x$lambda), character(1))
      valid <- unname(validByKey[lambdaKey(lambdaVals)])
      valid[is.na(valid)] <- FALSE
    } else {
      valid <- rep(FALSE, length(lambdaVals))
    }
    selectScores <- scores
    selectScores[!valid] <- NA_real_
    idx <- .adapBestLambdaIndex(selectScores, lambdaVals, selectionMetric, tieTolerance)
    diagnostics <- if (length(diagnosticRows)) do.call(rbind, diagnosticRows) else NULL
    trace <- list(
      context = traceContext,
      surrogateKind = surrogateKind,
      lambdaSeq = lambdaVals,
      scores = scores,
      valid = valid,
      diagnostics = diagnostics,
      fits = traceRows
    )
    if (is.na(idx)) {
      methodName <- if (length(surrogateKind) == 1L && !is.na(surrogateKind) && nzchar(surrogateKind)) {
        surrogateKind
      } else {
        "ADAP"
      }
      msg <- .adapCvFailureMessage(methodName, scores, diagnostics)
      trace$failed <- TRUE
      trace$error <- msg
      if (isTRUE(collectTrace) && !is.null(traceFile)) {
        dir.create(dirname(traceFile), recursive = TRUE, showWarnings = FALSE)
        saveRDS(trace, traceFile)
      }
      stop(msg, call. = FALSE)
    }
    trace$failed <- FALSE
    trace$selectedLambda <- lambdaVals[idx]
    if (isTRUE(collectTrace) && !is.null(traceFile)) {
      dir.create(dirname(traceFile), recursive = TRUE, showWarnings = FALSE)
      saveRDS(trace, traceFile)
    }
    list(
      lambda = lambdaVals[idx],
      scores = scores,
      lambdaSeq = lambdaVals,
      valid = valid,
      diagnostics = diagnostics,
      trace = if (isTRUE(collectTrace)) trace else NULL
    )
  }

  if (identical(search, "grid") || length(lambdaSeq) < 3L) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    return(cvResult(lambdaSeq, scores))
  }

  lambdaRange <- range(lambdaSeq[is.finite(lambdaSeq) & lambdaSeq > 0])
  if (!all(is.finite(lambdaRange)) || lambdaRange[1] <= 0 || lambdaRange[1] == lambdaRange[2]) {
    scores <- vapply(lambdaSeq, evaluateLambda, numeric(1))
    return(cvResult(lambdaSeq, scores))
  }

  maxEvals <- as.integer(maxEvals)
  if (length(maxEvals) != 1L || is.na(maxEvals) || maxEvals < 3L) {
    stop("maxEvals must be an integer of at least 3")
  }
  if (length(searchTol) != 1L || !is.finite(searchTol) || searchTol <= 0) {
    stop("searchTol must be a positive finite value")
  }

  lower <- log(lambdaRange[1])
  upper <- log(lambdaRange[2])

  evaluatedRows <- function() {
    evaluated <- as.list(evalCache)
    if (length(evaluated) == 0L) {
      return(data.frame(lambda = numeric(), score = numeric(), valid = logical()))
    }
    data.frame(
      lambda = unname(vapply(evaluated, `[[`, numeric(1), "lambda")),
      score = unname(vapply(evaluated, `[[`, numeric(1), "score")),
      valid = unname(vapply(evaluated, function(x) isTRUE(x$valid), logical(1))),
      stringsAsFactors = FALSE
    )
  }
  evaluatedCount <- function() nrow(evaluatedRows())
  addLogLambda <- function(logLambda) {
    logLambda <- min(max(logLambda, lower), upper)
    rows <- evaluatedRows()
    if (nrow(rows) > 0L && any(abs(log(rows$lambda) - logLambda) <= 1e-8)) {
      return(FALSE)
    }
    evaluateLambda(exp(logLambda))
    TRUE
  }
  scoreForSearch <- function(scores) {
    if (identical(selectionMetric, "auc")) -scores else scores
  }

  searchFactor <- 10
  addLogLambda(upper)
  lastLogLambda <- upper
  repeat {
    if (evaluatedCount() >= maxEvals) {
      break
    }
    nextLogLambda <- max(lower, lastLogLambda - log(searchFactor))
    if (!addLogLambda(nextLogLambda)) {
      break
    }
    rows <- evaluatedRows()
    current <- rows[abs(log(rows$lambda) - nextLogLambda) <= 1e-8, , drop = FALSE]
    previous <- rows[abs(log(rows$lambda) - lastLogLambda) <= 1e-8, , drop = FALSE]
    if (nrow(current) == 0L || !isTRUE(current$valid[[1]]) || !is.finite(current$score[[1]])) {
      break
    }
    if (nrow(previous) > 0L && isTRUE(previous$valid[[1]]) && is.finite(previous$score[[1]])) {
      currentScore <- scoreForSearch(current$score[[1]])
      previousScore <- scoreForSearch(previous$score[[1]])
      if (currentScore > previousScore) {
        break
      }
    }
    if (nextLogLambda <= lower) {
      break
    }
    lastLogLambda <- nextLogLambda
  }

  repeat {
    if (evaluatedCount() >= maxEvals) {
      break
    }
    rows <- evaluatedRows()
    validRows <- rows[rows$valid & is.finite(rows$score) & rows$lambda > 0, , drop = FALSE]
    if (nrow(validRows) < 3L) {
      break
    }
    validRows <- validRows[order(validRows$lambda, decreasing = TRUE), , drop = FALSE]
    x <- log(validRows$lambda)
    searchScores <- scoreForSearch(validRows$score)
    bestPos <- which.min(searchScores)
    idx <- sort(unique(pmax(1L, pmin(nrow(validRows), c(bestPos - 1L, bestPos, bestPos + 1L)))))
    if (length(idx) < 3L) {
      idx <- sort(order(abs(x - x[[bestPos]]))[seq_len(min(3L, length(x)))])
    }
    if (length(idx) < 3L) {
      break
    }
    fit <- tryCatch(
      stats::lm(searchScores[idx] ~ x[idx] + I(x[idx]^2)),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      break
    }
    coefs <- stats::coef(fit)
    if (length(coefs) < 3L || !all(is.finite(coefs)) || coefs[[3L]] <= 0) {
      break
    }
    proposed <- -coefs[[2L]] / (2 * coefs[[3L]])
    if (!is.finite(proposed) || proposed <= min(x) || proposed >= max(x)) {
      break
    }
    if (min(abs(x - proposed)) <= searchTol) {
      intervals <- list()
      if (bestPos > 1L) {
        intervals[[length(intervals) + 1L]] <- c(x[[bestPos - 1L]], x[[bestPos]])
      }
      if (bestPos < length(x)) {
        intervals[[length(intervals) + 1L]] <- c(x[[bestPos]], x[[bestPos + 1L]])
      }
      if (length(intervals) == 0L) {
        break
      }
      gaps <- vapply(intervals, function(z) abs(diff(z)), numeric(1))
      if (max(gaps) <= searchTol) {
        break
      }
      proposed <- mean(intervals[[which.max(gaps)]])
    }
    if (!addLogLambda(proposed)) {
      break
    }
  }
  evaluated <- as.list(evalCache)
  lambdaVals <- unname(vapply(evaluated, `[[`, numeric(1), "lambda"))
  scores <- unname(vapply(evaluated, `[[`, numeric(1), "score"))
  keep <- order(lambdaVals, decreasing = TRUE)
  lambdaVals <- lambdaVals[keep]
  scores <- scores[keep]
  out <- cvResult(lambdaVals, scores)
  out$lambdaSeq <- lambdaVals
  out
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
                           collectTrace = FALSE,
                           traceContext = list(),
                           traceFile = NULL,
                           cdStepBound = 1,
                           cdMinStep = 1e-8,
                           cdMaxBacktracks = 25L,
                           surrogateVariant = c("exact", "prox", "convex", "maxconv"),
                           leadWeight = 1,
                           leadWeightMin = leadWeight,
                           proxTau = 1e-8,
                           maxConvTau = 1e-10,
                           kktTolerance = 1e-4,
                           betaAbsThreshold = 1e4,
                           etaAbsThreshold = 1e4) {
  globalAdjustment <- match.arg(globalAdjustment)
  surrogateVariant <- match.arg(surrogateVariant)
  surrogateLabel <- switch(surrogateVariant,
    exact = "full",
    prox = "prox",
    convex = "convex",
    maxconv = "maxconv"
  )
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
        leadWeightMinTrain <- length(info$idxTr) / denom
      } else {
        gradTrainGlobal <- globalGrad
        hessTrainGlobal <- globalHess
        leadWeightMinTrain <- leadWeightMin
      }
      hBarTrain <- .logisticNegHessian(betaBar, xCv[info$idxTr, , drop = FALSE])
      leadWeightTrain <- switch(surrogateVariant,
        exact = 1,
        prox = 1,
        convex = leadWeightMinTrain,
        maxconv = .adapMaxConvAlpha(
          hessTrainGlobal,
          hBarTrain,
          leadWeightMin = leadWeightMinTrain,
          tau = maxConvTau
        )$alpha
      )
      baseCorrection <- hessTrainGlobal - leadWeightTrain * hBarTrain
      proxRhoTrain <- if (identical(surrogateVariant, "prox")) {
        .adapProxShift(hessTrainGlobal, baseCorrection, tau = proxTau)$rho
      } else {
        0
      }
      list(
        gradTrainGlobal = gradTrainGlobal,
        hessTrainGlobal = hessTrainGlobal,
        leadWeightTrain = leadWeightTrain,
        proxRhoTrain = proxRhoTrain,
        diagnostics = if (isTRUE(collectDiagnostics)) {
          .adapSurrogateStats(
            kind = "full",
            xTrain = xCv[info$idxTr, , drop = FALSE],
            yTrain = yCv[info$idxTr],
            betaLead = betaLead,
            betaBar = betaBar,
            globalGrad = gradTrainGlobal,
            globalHess = hessTrainGlobal,
            leadWeight = leadWeightTrain
          )
        } else {
          NULL
        }
      )
    },
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
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
        cdMaxBacktracks = cdMaxBacktracks,
        traceDiagnostics = collectTrace,
        traceContext = c(traceContext, list(innerFold = info$fold, surrogateKind = surrogateLabel)),
        leadWeight = info$leadWeightTrain,
        proxRho = info$proxRhoTrain,
        strictCorrection = surrogateVariant,
        proxTau = proxTau,
        kktTolerance = kktTolerance,
        betaAbsThreshold = betaAbsThreshold,
        etaAbsThreshold = etaAbsThreshold
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = surrogateLabel,
    collectTrace = collectTrace,
    traceContext = c(traceContext, list(surrogateKind = surrogateLabel)),
    traceFile = traceFile
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
                                collectTrace = FALSE,
                                traceContext = list(),
                                traceFile = NULL,
                                cdStepBound = 1,
                                cdMinStep = 1e-8,
                                cdMaxBacktracks = 25L,
                                kktTolerance = 1e-4,
                                betaAbsThreshold = 1e4,
                                etaAbsThreshold = 1e4) {
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
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
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
        cdMaxBacktracks = cdMaxBacktracks,
        traceDiagnostics = collectTrace,
        traceContext = c(traceContext, list(innerFold = info$fold, surrogateKind = "first")),
        kktTolerance = kktTolerance,
        betaAbsThreshold = betaAbsThreshold,
        etaAbsThreshold = etaAbsThreshold
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = "first",
    collectTrace = collectTrace,
    traceContext = c(traceContext, list(surrogateKind = "first")),
    traceFile = traceFile
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
                               collectTrace = FALSE,
                               traceContext = list(),
                               traceFile = NULL,
                               cdStepBound = 1,
                               cdMinStep = 1e-8,
                               cdMaxBacktracks = 25L,
                               kktTolerance = 1e-4,
                               betaAbsThreshold = 1e4,
                               etaAbsThreshold = 1e4) {
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
    fitFold = function(info, lambda, warmStart, collectDiagnostics = FALSE, collectTrace = FALSE) {
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
        cdMaxBacktracks = cdMaxBacktracks,
        traceDiagnostics = collectTrace,
        traceContext = c(traceContext, list(innerFold = info$fold, surrogateKind = "diag")),
        kktTolerance = kktTolerance,
        betaAbsThreshold = betaAbsThreshold,
        etaAbsThreshold = etaAbsThreshold
      )
    },
    collectDiagnostics = collectDiagnostics,
    surrogateKind = "diag",
    collectTrace = collectTrace,
    traceContext = c(traceContext, list(surrogateKind = "diag")),
    traceFile = traceFile
  )
}

.serverInitPdaAdap <- function(config) {
  p <- config[["p"]] + 1L
  list(
    phase = 0L,
    p = p,
    adapSolveStyle = "fullQuadratic",
    adapSurrogateVariant = "exact",
    adapMethodName = "ADAP",
    betaBar = rep(0, p),
    betaLead = rep(0, p),
    leadIndex = NA_integer_,
    leadWeight = 1,
    leadWeightMin = NA_real_,
    maxConvAlpha = NA_real_,
    maxConvAlphaStatus = NA_character_,
    globalGrad = NULL,
    globalHess = NULL,
    lambdaSeq = NULL,
    selectedLambda = config[["lambda", exact = TRUE]] %||% NA_real_,
    w = rep(0, p)
  )
}

.serverInitPdaAdapPda <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapSolveStyle <- "pda"
  state$adapMethodName <- "ADAP_PDA"
  state
}

.serverInitPdaAdap2 <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapMethodName <- "ADAP2"
  state$adapSurrogateVariant <- "exact"
  state
}

.serverInitProxAdap <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapMethodName <- "Prox-ADAP"
  state$adapSurrogateVariant <- "prox"
  state
}

.serverInitCAdap <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapMethodName <- "C-ADAP"
  state$adapSurrogateVariant <- "convex"
  state
}

.serverInitMaxConvAdap <- function(config) {
  state <- .serverInitPdaAdap(config)
  state$adapMethodName <- "MaxConv-ADAP"
  state$adapSurrogateVariant <- "maxconv"
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
    surrogateVariant <- match.arg(
      serverBroadcast$adapSurrogateVariant %||% config$adapSurrogateVariant %||% "exact",
      c("exact", "prox", "convex", "maxconv")
    )
    methodName <- serverBroadcast$adapMethodName %||% config$adapMethodName %||%
      switch(surrogateVariant,
        exact = "ADAP",
        prox = "Prox-ADAP",
        convex = "C-ADAP",
        maxconv = "MaxConv-ADAP"
      )
    leadWeight <- serverBroadcast$leadWeight %||% 1
    leadWeightMin <- serverBroadcast$leadWeightMin %||% leadWeight
    cvDiagnostics <- NULL
    cvValid <- NULL
    cvTrace <- NULL
    finalTrace <- NULL
    traceEnabled <- isTRUE(config$adapTraceDiagnostics)
    finalMaxOuter <- config$adapFinalMaxOuter %||% max(1000L, config$maxOuter %||% 500L)
    baseTraceContext <- c(
      config$adapTraceContext %||% list(),
      list(
        leadIndex = localId,
        trainRows = length(y),
        trainOutcomes = sum(y == 1),
        covariateMap = config$mapping %||% NULL
      )
    )
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
        cvTrace <- cv$trace %||% NULL
      }
      fitObj <- .fitPdaAdapPdaProx(
        xDesign = xDesign,
        y = y,
        beta0 = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHess,
        lambda = lambda,
        useFull = TRUE,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4,
        returnDetails = TRUE
      )
      w <- .adapFitBeta(fitObj)
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        adapCvDiagnostics = cvDiagnostics,
        adapTrace = cvTrace,
        adapFinalDiagnostics = .adapFitDiagnosticsForReport(fitObj),
        lambdaSelectionMetric = config$lambdaSelectionMetric %||% "deviance"
      ))
    }
    if (!is.null(fixedLambda)) {
      lambda <- fixedLambda
      lambdaSeq <- lambda
      cvScores <- NA_real_
    } else {
      if (is.null(lambdaSeq)) {
        if (identical(surrogateVariant, "prox")) {
          hBarFull <- .logisticNegHessian(betaBar, xDesign)
          proxInit <- .adapProxShift(globalHess, globalHess - hBarFull, tau = config$adapProxTau %||% 1e-8)
          lambdaSeq <- .pdaAdapLambdaSeq(
            xDesign,
            y,
            betaLead,
            betaBar,
            globalGrad,
            globalHess,
            gridLen = config$lambdaGridLen %||% 100L,
            proxRho = proxInit$rho
          )
        } else {
          lambdaSeq <- .pdaAdapLambdaSeq(
            xDesign,
            y,
            betaLead,
            betaBar,
            globalGrad,
            globalHess,
            gridLen = config$lambdaGridLen %||% 100L,
            leadWeight = leadWeight
          )
        }
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
        maxOuter = config$maxOuter %||% 500L,
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
        collectTrace = traceEnabled,
        traceContext = c(baseTraceContext, list(method = methodName, phase = "cv")),
        traceFile = config$adapTraceFile %||% NULL,
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
        surrogateVariant = surrogateVariant,
        leadWeight = leadWeight,
        leadWeightMin = leadWeightMin,
        proxTau = config$adapProxTau %||% 1e-8,
        maxConvTau = config$adapMaxConvTau %||% config$adapCurvatureTau %||% 1e-10,
        kktTolerance = config$adapKktTolerance %||% 1e-4,
        betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
        etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
      cvDiagnostics <- cv$diagnostics %||% NULL
      cvValid <- cv$valid %||% NULL
      cvTrace <- cv$trace %||% NULL
    }
    fitObj <- .fitPdaAdapSurrogate(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHess = globalHess,
      lambda = lambda,
      maxOuter = finalMaxOuter,
      maxInner = config$maxInner %||% 100L,
      tol = config$tol %||% 1e-5,
      cdStepBound = config$adapCdStepBound %||% 1,
      cdMinStep = config$adapCdMinStep %||% 1e-8,
      cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
      returnDetails = TRUE,
      traceDiagnostics = traceEnabled,
      traceContext = c(baseTraceContext, list(method = methodName, phase = "final", selectedLambda = lambda)),
      leadWeight = leadWeight,
      strictCorrection = surrogateVariant,
      proxTau = config$adapProxTau %||% 1e-8,
      kktTolerance = config$adapKktTolerance %||% 1e-4,
      betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
      etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
    )
    if (.adapFitFailed(fitObj)) {
      stop(sprintf("%s final fit failed: %s", methodName, .adapFitFailureReason(fitObj)), call. = FALSE)
    }
    w <- .adapFitBeta(fitObj)
    finalTrace <- if (isTRUE(traceEnabled) && !is.null(fitObj$trace)) fitObj$trace else NULL
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      cvValid = cvValid,
      adapCvDiagnostics = cvDiagnostics,
      adapTrace = cvTrace,
      adapFinalTrace = finalTrace,
      adapFinalDiagnostics = .adapFitDiagnosticsForReport(fitObj),
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
    state$leadWeightMin <- weights[[leadIndex]]
    state$leadWeight <- 1
    state$totalN <- sum(ns)
    state$w <- betaBar
    return(list(
      state = state,
      report = list(
        w = betaBar,
        leadIndex = leadIndex,
        leadWeight = state$leadWeight,
        leadWeightMin = state$leadWeightMin,
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
    state$leadWeightMin <- weights[[state$leadIndex]]
    surrogateVariant <- state$adapSurrogateVariant %||% config$adapSurrogateVariant %||% "exact"
    leadHess <- hessList[[state$leadIndex]]
    siteEigenMins <- vapply(hessList, function(H) .adapEigenRange(H)$min, numeric(1))
    globalEig <- .adapEigenRange(globalHess)
    if (identical(surrogateVariant, "convex")) {
      state$leadWeight <- state$leadWeightMin
    } else if (identical(surrogateVariant, "maxconv")) {
      alpha <- .adapMaxConvAlpha(
        globalHess,
        leadHess,
        leadWeightMin = state$leadWeightMin,
        tau = config$adapMaxConvTau %||% config$adapCurvatureTau %||% 1e-10
      )
      state$leadWeight <- alpha$alpha
      state$maxConvAlpha <- alpha$alpha
      state$maxConvAlphaStatus <- alpha$alphaStatus
      state$maxConvAlphaEigenMin <- alpha$alphaEigenMin
      state$maxConvAlphaLowerEigenMin <- alpha$alphaLowerEigenMin
      state$maxConvAlphaEpsilon <- alpha$alphaEpsilon
    } else {
      state$leadWeight <- 1
    }
    correction <- globalHess - state$leadWeight * leadHess
    correctionEigVals <- tryCatch(
      eigen((correction + t(correction)) / 2, symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA_real_
    )
    correctionEigFinite <- is.finite(correctionEigVals)
    correctionEpsilon <- .adapEpsilonEig(globalHess, tau = config$adapCurvatureTau %||% 1e-10)
    correctionDiag <- diag(correction)
    state$globalHessianEigenMin <- globalEig$min
    state$leadHessianEigenMin <- siteEigenMins[[state$leadIndex]]
    state$siteHessianEigenMin <- min(siteEigenMins, na.rm = TRUE)
    state$siteHessianNegative <- sum(is.finite(siteEigenMins) & siteEigenMins < -correctionEpsilon)
    state$correctionEigenMin <- if (any(correctionEigFinite)) min(correctionEigVals[correctionEigFinite]) else NA_real_
    state$correctionEigenMax <- if (any(correctionEigFinite)) max(correctionEigVals[correctionEigFinite]) else NA_real_
    state$correctionEigenNegative <- if (any(correctionEigFinite)) sum(correctionEigVals[correctionEigFinite] < -correctionEpsilon) else NA_integer_
    state$curvatureStatus <- if (is.na(state$correctionEigenNegative)) {
      "unknown"
    } else if (state$correctionEigenNegative > 0L) {
      "indefinite"
    } else {
      "psd"
    }
    state$correctionDiagMin <- min(correctionDiag, na.rm = TRUE)
    state$correctionDiagMax <- max(correctionDiag, na.rm = TRUE)
    state$correctionDiagNegative <- sum(is.finite(correctionDiag) & correctionDiag < -correctionEpsilon)
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
        leadWeight = state$leadWeight,
        leadWeightMin = state$leadWeightMin,
        skipConvergence = TRUE,
        hessianDim = state$hessianDim,
        hessianDiagMin = state$hessianDiagMin,
        hessianDiagMax = state$hessianDiagMax,
        hessianCondition = state$hessianCondition,
        curvatureStatus = state$curvatureStatus,
        correctionEigenMin = state$correctionEigenMin,
        correctionEigenMax = state$correctionEigenMax,
        correctionEigenNegative = state$correctionEigenNegative,
        correctionDiagMin = state$correctionDiagMin,
        correctionDiagMax = state$correctionDiagMax,
        correctionDiagNegative = state$correctionDiagNegative,
        globalHessianEigenMin = state$globalHessianEigenMin,
        leadHessianEigenMin = state$leadHessianEigenMin,
        siteHessianEigenMin = state$siteHessianEigenMin,
        siteHessianNegative = state$siteHessianNegative,
        maxConvAlpha = state$maxConvAlpha %||% NA_real_,
        maxConvAlphaStatus = state$maxConvAlphaStatus %||% NA_character_,
        maxConvAlphaEigenMin = state$maxConvAlphaEigenMin %||% NA_real_,
        maxConvAlphaLowerEigenMin = state$maxConvAlphaLowerEigenMin %||% NA_real_,
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
          leadWeight = state$leadWeight,
          leadWeightMin = state$leadWeightMin,
          selectedLambda = leadReport$selectedLambda,
          lambdaSeq = leadReport$lambdaSeq,
          cvScores = leadReport$cvScores,
          cvValid = leadReport$cvValid %||% NULL,
          adapCvDiagnostics = leadReport$adapCvDiagnostics %||% NULL,
          adapTrace = leadReport$adapTrace %||% NULL,
          adapFinalTrace = leadReport$adapFinalTrace %||% NULL,
          adapFinalDiagnostics = leadReport$adapFinalDiagnostics %||% NULL,
          lambdaSelectionMetric = leadReport$lambdaSelectionMetric %||% NA_character_,
          hessianDim = state$hessianDim %||% NA_character_,
          hessianDiagMin = state$hessianDiagMin %||% NA_real_,
          hessianDiagMax = state$hessianDiagMax %||% NA_real_,
          hessianCondition = state$hessianCondition %||% NA_real_,
          curvatureStatus = state$curvatureStatus %||% NA_character_,
          correctionEigenMin = state$correctionEigenMin %||% NA_real_,
          correctionEigenMax = state$correctionEigenMax %||% NA_real_,
          correctionEigenNegative = state$correctionEigenNegative %||% NA_real_,
          correctionDiagMin = state$correctionDiagMin %||% NA_real_,
          correctionDiagMax = state$correctionDiagMax %||% NA_real_,
          correctionDiagNegative = state$correctionDiagNegative %||% NA_real_,
          globalHessianEigenMin = state$globalHessianEigenMin %||% NA_real_,
          leadHessianEigenMin = state$leadHessianEigenMin %||% NA_real_,
          siteHessianEigenMin = state$siteHessianEigenMin %||% NA_real_,
          siteHessianNegative = state$siteHessianNegative %||% NA_real_,
          maxConvAlpha = state$maxConvAlpha %||% NA_real_,
          maxConvAlphaStatus = state$maxConvAlphaStatus %||% NA_character_,
          maxConvAlphaEigenMin = state$maxConvAlphaEigenMin %||% NA_real_,
          maxConvAlphaLowerEigenMin = state$maxConvAlphaLowerEigenMin %||% NA_real_,
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

.registerAlgorithm(
  "ADAP2",
  serverInit = .serverInitPdaAdap2,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdap,
  serverRound = .serverRoundPdaAdap,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.registerAlgorithm(
  "Prox-ADAP",
  serverInit = .serverInitProxAdap,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdap,
  serverRound = .serverRoundPdaAdap,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.registerAlgorithm(
  "C-ADAP",
  serverInit = .serverInitCAdap,
  clientInit = NULL,
  clientUpdate = .clientUpdatePdaAdap,
  serverRound = .serverRoundPdaAdap,
  lambdaStrategy = .lambdaStrategyPdaAdap()
)

.registerAlgorithm(
  "MaxConv-ADAP",
  serverInit = .serverInitMaxConvAdap,
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
    cvValid <- NULL
    cvTrace <- NULL
    finalTrace <- NULL
    traceEnabled <- isTRUE(config$adapTraceDiagnostics)
    methodName <- if (identical(mode, "first")) "ADAP1" else "ADAPDiag"
    finalMaxOuter <- config$adapFinalMaxOuter %||% max(1000L, config$maxOuter %||% 500L)
    baseTraceContext <- c(
      config$adapTraceContext %||% list(),
      list(
        leadIndex = localId,
        trainRows = length(y),
        trainOutcomes = sum(y == 1),
        covariateMap = config$mapping %||% NULL
      )
    )
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
          maxOuter = config$maxOuter %||% 500L,
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
          collectTrace = traceEnabled,
          traceContext = c(baseTraceContext, list(method = methodName, phase = "cv")),
          traceFile = config$adapTraceFile %||% NULL,
          cdStepBound = config$adapCdStepBound %||% 1,
          cdMinStep = config$adapCdMinStep %||% 1e-8,
          cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
          kktTolerance = config$adapKktTolerance %||% 1e-4,
          betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
          etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
        )
        lambda <- cv$lambda
        cvScores <- cv$scores
        lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
        cvDiagnostics <- cv$diagnostics %||% NULL
        cvValid <- cv$valid %||% NULL
        cvTrace <- cv$trace %||% NULL
      }
      fitObj <- .fitPdaAdapFirstOrderSurrogate(
        xDesign = xDesign,
        y = y,
        betaLead = betaLead,
        betaBar = betaBar,
        globalGrad = globalGrad,
        lambda = lambda,
        maxOuter = finalMaxOuter,
        maxInner = config$maxInner %||% 100L,
        tol = config$tol %||% 1e-5,
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
        returnDetails = TRUE,
        traceDiagnostics = traceEnabled,
        traceContext = c(baseTraceContext, list(method = methodName, phase = "final", selectedLambda = lambda)),
        kktTolerance = config$adapKktTolerance %||% 1e-4,
        betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
        etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
      )
      if (.adapFitFailed(fitObj)) {
        stop(sprintf("%s final fit failed: %s", methodName, .adapFitFailureReason(fitObj)), call. = FALSE)
      }
      w <- .adapFitBeta(fitObj)
      finalTrace <- if (isTRUE(traceEnabled) && !is.null(fitObj$trace)) fitObj$trace else NULL
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        cvValid = cvValid,
        adapCvDiagnostics = cvDiagnostics,
        adapTrace = cvTrace,
        adapFinalTrace = finalTrace,
        adapFinalDiagnostics = .adapFitDiagnosticsForReport(fitObj),
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
        cvTrace <- cv$trace %||% NULL
      }
      fitObj <- .fitPdaAdapPdaProx(
        xDesign = xDesign,
        y = y,
        beta0 = betaBar,
        globalGrad = globalGrad,
        globalHess = globalHessDiag,
        lambda = lambda,
        useFull = FALSE,
        maxIter = config$maxIter %||% 1000L,
        tol = config$tol %||% 1e-6,
        ridge = config$hessianRidge %||% config$hessian_ridge %||% 1e-4,
        returnDetails = TRUE
      )
      w <- .adapFitBeta(fitObj)
      return(list(
        w = w,
        selectedLambda = lambda,
        lambdaSeq = lambdaSeq,
        cvScores = cvScores,
        adapCvDiagnostics = cvDiagnostics,
        adapTrace = cvTrace,
        adapFinalDiagnostics = .adapFitDiagnosticsForReport(fitObj),
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
        maxOuter = config$maxOuter %||% 500L,
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
        collectTrace = traceEnabled,
        traceContext = c(baseTraceContext, list(method = methodName, phase = "cv")),
        traceFile = config$adapTraceFile %||% NULL,
        cdStepBound = config$adapCdStepBound %||% 1,
        cdMinStep = config$adapCdMinStep %||% 1e-8,
        cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
        kktTolerance = config$adapKktTolerance %||% 1e-4,
        betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
        etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
      )
      lambda <- cv$lambda
      cvScores <- cv$scores
      lambdaSeq <- cv$lambdaSeq %||% lambdaSeq
      cvDiagnostics <- cv$diagnostics %||% NULL
      cvValid <- cv$valid %||% NULL
      cvTrace <- cv$trace %||% NULL
    }
    fitObj <- .fitPdaAdapRemoteDiagSurrogate(
      xDesign = xDesign,
      y = y,
      betaLead = betaLead,
      betaBar = betaBar,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      lambda = lambda,
      maxOuter = finalMaxOuter,
      maxInner = config$maxInner %||% 100L,
      tol = config$tol %||% 1e-5,
      cdStepBound = config$adapCdStepBound %||% 1,
      cdMinStep = config$adapCdMinStep %||% 1e-8,
      cdMaxBacktracks = config$adapCdMaxBacktracks %||% 25L,
      returnDetails = TRUE,
      traceDiagnostics = traceEnabled,
      traceContext = c(baseTraceContext, list(method = methodName, phase = "final", selectedLambda = lambda)),
      kktTolerance = config$adapKktTolerance %||% 1e-4,
      betaAbsThreshold = config$adapBetaAbsThreshold %||% 1e4,
      etaAbsThreshold = config$adapEtaAbsThreshold %||% 1e4
    )
    if (.adapFitFailed(fitObj)) {
      stop(sprintf("%s final fit failed: %s", methodName, .adapFitFailureReason(fitObj)), call. = FALSE)
    }
    w <- .adapFitBeta(fitObj)
    finalTrace <- if (isTRUE(traceEnabled) && !is.null(fitObj$trace)) fitObj$trace else NULL
    return(list(
      w = w,
      selectedLambda = lambda,
      lambdaSeq = lambdaSeq,
      cvScores = cvScores,
      cvValid = cvValid,
      adapCvDiagnostics = cvDiagnostics,
      adapTrace = cvTrace,
      adapFinalTrace = finalTrace,
      adapFinalDiagnostics = .adapFitDiagnosticsForReport(fitObj),
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
          cvValid = leadReport$cvValid %||% NULL,
          adapCvDiagnostics = leadReport$adapCvDiagnostics %||% NULL,
          adapTrace = leadReport$adapTrace %||% NULL,
          adapFinalTrace = leadReport$adapFinalTrace %||% NULL,
          adapFinalDiagnostics = leadReport$adapFinalDiagnostics %||% NULL,
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
