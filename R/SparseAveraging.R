softThresholdAverage <- function(w, tau, intercept = TRUE, scales = rep(1, length(w))) {
  if (!is.numeric(w) || !length(w) || any(!is.finite(w)) ||
      length(tau) != 1L || !is.finite(tau) || tau < 0 ||
      length(scales) != length(w) || any(!is.finite(scales) | scales <= 0)) {
    stop("Sparse averaging requires finite coefficients, positive scales and a nonnegative threshold")
  }
  if (tau == 0) return(w)
  slopes <- seq_along(w)
  if (isTRUE(intercept)) slopes <- slopes[-1L]
  # Threshold in the shared, pre-normalization coefficient units.
  w[slopes] <- sign(w[slopes]) * pmax(abs(w[slopes]) - tau * scales[slopes], 0)
  w
}

nodewisePrecision <- function(hessian, n, multiplier = 1) {
  p <- ncol(hessian)
  if (!is.matrix(hessian) || nrow(hessian) != p || p < 1L ||
      any(!is.finite(hessian)) || !isTRUE(all.equal(hessian, t(hessian))) ||
      length(n) != 1L || !is.finite(n) || n <= 0 ||
      length(multiplier) != 1L || !is.finite(multiplier) || multiplier < 0) {
    stop("Invalid Hessian, sample count or nodewise penalty multiplier")
  }
  if (any(diag(hessian) < 0)) stop("Negative logistic curvature")
  active <- which(diag(hessian) > 0)
  precision <- matrix(0, p, p)
  lambda <- multiplier * sqrt(log(max(p, 2L)) / n)
  if (!length(active)) stop("No positive logistic curvature for debiasing")
  scale <- sqrt(diag(hessian)[active])
  correlation <- hessian[active, active, drop = FALSE] / outer(scale, scale)
  spectral <- eigen(correlation, symmetric = TRUE)
  if (min(spectral$values) < -1e-10 * max(1, max(spectral$values))) {
    stop("Logistic curvature is not positive semidefinite")
  }
  q <- length(active)
  if (lambda == 0 && min(spectral$values) <= q * .Machine$double.eps * max(spectral$values)) {
    stop("Nodewise residual curvature is singular; use a positive nodewise penalty")
  }
  # A small pseudo-design has the same weighted Gram matrix as the patient rows.
  # Nodewise Gaussian lasso therefore needs no repeated passes over patient data.
  z <- sqrt(q) * (sqrt(pmax(spectral$values, 0)) * t(spectral$vectors))
  theta <- matrix(0, q, q)
  residualVariances <- numeric(q)
  for (j in seq_len(q)) {
    others <- setdiff(seq_len(q), j)
    gamma <- numeric(length(others))
    if (length(others)) {
      predictors <- z[, others, drop = FALSE]
      # glmnet requires at least two predictor columns and two observations.
      if (ncol(predictors) == 1L) predictors <- cbind(predictors, 0)
      fitArgs <- list(x = predictors, y = z[, j], family = "gaussian",
        alpha = 1, lambda = lambda, intercept = FALSE, standardize = FALSE)
      controls <- list(thresh = 1e-12, maxit = 100000L)
      if ("control" %in% names(formals(glmnet::glmnet))) {
        fitArgs$control <- controls
      } else {
        fitArgs <- c(fitArgs, controls)
      }
      fit <- do.call(glmnet::glmnet, fitArgs)
      if (fit$jerr != 0L) stop("Nodewise lasso did not converge")
      gamma <- as.numeric(fit$beta)[seq_along(others)]
    }
    residual <- z[, j] - as.numeric(z[, others, drop = FALSE] %*% gamma)
    tau2 <- mean(residual^2) + lambda * sum(abs(gamma))
    if (!is.finite(tau2) || tau2 <= .Machine$double.eps) {
      stop("Nodewise residual curvature is singular; use a positive nodewise penalty")
    }
    theta[j, j] <- 1 / tau2
    theta[j, others] <- -gamma / tau2
    residualVariances[j] <- tau2
  }
  precision[active, active] <- theta / outer(scale, scale)
  list(precision = precision, diagnostics = list(
    nodewiseLambda = lambda, nodewiseMultiplier = multiplier,
    zeroCurvatureColumns = setdiff(seq_len(p), active),
    minResidualCurvature = min(residualVariances),
    inverseResidualMax = max(abs(theta %*% correlation - diag(q))),
    standardizedHessianMinEigenvalue = min(spectral$values)
  ))
}

debiasLocalLasso <- function(clientData, w, multiplier = 1, maxFeatures = 512L) {
  x <- clientData$xMatrix
  y <- clientData$yLabels
  p <- ncol(x)
  if (length(maxFeatures) != 1L || !is.finite(maxFeatures) || maxFeatures < 1L || p > maxFeatures) {
    stop("DebiasedLocalAvgLasso requires at most ", maxFeatures,
      " matrix columns (dense local curvature); change --local-debias-max-features explicitly")
  }
  assertConformableWeights(w, x, context = "local debiasing")
  if (length(y) != nrow(x) || !length(y) || anyNA(y) || any(!y %in% c(0, 1)) ||
      (!is.null(clientData$n) && !identical(as.numeric(clientData$n), as.numeric(length(y))))) {
    stop("Local debiasing requires binary labels and consistent training sample counts")
  }
  x <- .asDgCMatrix(x)
  stats <- logisticObjectiveGradientCpp(x, w, as.numeric(y))
  eta <- as.numeric(x %*% w)
  curvature <- stats::plogis(eta) * stats::plogis(-eta)
  weighted <- Matrix::Diagonal(x = sqrt(curvature)) %*% x
  hessian <- as.matrix(Matrix::crossprod(weighted)) / length(y)
  inverse <- nodewisePrecision(hessian, length(y), multiplier)
  gradient <- as.numeric(stats$gradient)
  correction <- as.numeric(inverse$precision %*% gradient)
  if (any(!is.finite(correction))) stop("Non-finite local debiasing correction")
  list(w = w - correction, originalW = w, gradient = gradient,
    correction = correction, diagnostics = c(inverse$diagnostics,
      list(correctionMaxAbs = max(abs(correction)), n = length(y))))
}
