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

proxFastL1 <- function(z, A, mu, lambda, intercept = FALSE) {
  u <- -z / (mu * A)
  w <- sign(u) * pmax(abs(u) - (lambda / mu), 0)
  if (intercept) w[1] <- u[1] # do not regularize the intercept
  w
}
proxL1Ridge <- function(z, A, mu, gamma, lambda, intercept = FALSE) {
  # combined quadratic coefficient
  denom <- mu * A + 2 * gamma

  # unregularized update
  u <- -z / denom

  # soft-threshold by (lambda * A)/denom
  w <- sign(u) * pmax(abs(u) - (lambda * A) / denom, 0)

  # leave intercept unshrunk
  if (intercept) {
    w[1] <- u[1]
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
    return(as.numeric(logisticGradientCpp(.asDgCMatrix(xMatrix), weights, yLabels)))
  }
  eta <- stats::plogis(as.vector(xMatrix %*% weights))
  res <- eta - yLabels
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
  logTerm <- ifelse(eta > 0, eta + log1p(exp(-eta)), log1p(exp(eta)))
  loss <- logTerm - yLabels * eta
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

logisticLoss <- function(weights, xMatrix, yLabels) {
  -logisticNegLogLik(weights, xMatrix, yLabels, meanLoss = FALSE)
}
