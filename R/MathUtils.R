#' Soft-threshold l1-proximal operator
#' @param z numeric vector (dual state)
#' @param alphaLambda nonnegative scalar (alpha * lambda)
#' @param intercept wether there is an intercept
#' @return numeric vector (primal state)
#' @export
proxL1 <- function(z, alphaLambda, intercept = TRUE) {
  w <- sign(z) * pmax(abs(z) - alphaLambda, 0)
  # don't regularize intercept
  if (intercept) {
    w[1] <- z[1]
  }
  w
}

.asDgCMatrix <- function(xMatrix) {
  if (inherits(xMatrix, "dgCMatrix")) {
    xMatrix
  } else {
    methods::as(xMatrix, "dgCMatrix")
  }
}

#' Stochastic gradient of logistic loss
#' @param weights   numeric vector of parameters
#' @param xMatrix   numeric matrix (n by p)
#' @param yLabels   numeric vector of binary labels
#' @return numeric vector (length p)
#' @export
gradLogistic <- function(weights, xMatrix, yLabels) {
  assertConformableWeights(weights, xMatrix, context = "gradLogistic")
  if (inherits(xMatrix, "sparseMatrix")) {
    return(as.numeric(logisticGradientCpp(.asDgCMatrix(xMatrix), weights, yLabels, eps = 0)))
  }
  eta <- as.numeric(xMatrix %*% weights)
  res <- stats::plogis(eta) - yLabels
  cases <- yLabels == 1
  res[cases] <- -stats::plogis(-eta[cases])
  as.numeric(crossprod(xMatrix, res)) / length(yLabels)
}

assertConformableWeights <- function(weights, xMatrix, context = "model") {
  p <- ncol(xMatrix)
  wLen <- length(weights)
  if (!identical(as.integer(p), as.integer(wLen))) {
    stop(
      context, " dimension mismatch: xMatrix has ", p,
      " columns but weights has length ", wLen,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

binaryLogLoss <- function(eta, yLabels, meanLoss = TRUE) {
  loss <- log1p(exp(-abs(eta))) +
    ifelse(eta >= 0, (1 - yLabels) * eta, -yLabels * eta)
  if (meanLoss) {
    mean(loss)
  } else {
    sum(loss)
  }
}

logisticNegLogLik <- function(weights, xMatrix, yLabels, meanLoss = FALSE) {
  assertConformableWeights(weights, xMatrix, context = "logisticNegLogLik")
  linearPred <- as.numeric(xMatrix %*% weights)
  binaryLogLoss(linearPred, yLabels, meanLoss = meanLoss)
}

cyclopsGradientObjective <- function(weights, xMatrix, yLabels) {
  assertConformableWeights(weights, xMatrix, context = "cyclopsGradientObjective")
  if (inherits(xMatrix, "sparseMatrix")) {
    return(cyclopsGradientObjectiveCpp(.asDgCMatrix(xMatrix), weights, yLabels))
  }
  sum(as.numeric(xMatrix %*% weights) * yLabels)
}

logisticLoss <- function(weights, xMatrix, yLabels) {
  -logisticNegLogLik(weights, xMatrix, yLabels, meanLoss = FALSE)
}

.lassoKktResidual <- function(weights, gradient, lambda, intercept = TRUE) {
  if (length(weights) == 0L || length(weights) != length(gradient) ||
      any(!is.finite(weights)) || any(!is.finite(gradient)) ||
      !is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda < 0) {
    stop("KKT residual requires finite, conformable coefficients/gradients and non-negative lambda")
  }
  residual <- pmax(abs(gradient) - lambda, 0)
  active <- weights != 0
  residual[active] <- abs(gradient[active] + lambda * sign(weights[active]))
  if (isTRUE(intercept)) residual[1L] <- abs(gradient[1L])
  residual
}
