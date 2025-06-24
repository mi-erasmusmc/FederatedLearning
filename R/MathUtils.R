#' Soft‐threshold ℓ1‐proximal operator
#' @param z numeric vector (dual state)
#' @param alphaLambda nonnegative scalar (α·λ)
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

  # un‐regularized update
  u <- -z / denom

  # soft‐threshold by (lambda * A)/denom
  w <- sign(u) * pmax(abs(u) - (lambda * A) / denom, 0)

  # leave intercept un‐shrunk
  if (intercept) {
    w[1] <- u[1]
  }

  w
}
#' Stochastic gradient of logistic loss
#' @param weights   numeric vector of parameters
#' @param xMatrix   numeric matrix (n × p)
#' @param yLabels   numeric vector (length n, in {0,1})
#' @return numeric vector (length p)
#' @export
gradLogistic <- function(weights, xMatrix, yLabels) {
  eta <- stats::plogis(as.vector(xMatrix %*% weights)) # σ(Xw)
  res <- eta - yLabels
  as.numeric(crossprod(xMatrix, res)) / length(yLabels)
}

logisticLoss <- function(weights, xMatrix, yLabels) {
  linearPred <- as.numeric(xMatrix %*% weights)
  loss <- sum(yLabels * linearPred - log1p(exp(linearPred)))
  loss
}
