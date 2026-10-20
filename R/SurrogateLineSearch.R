.surrogateArmijoStep <- function(beta, candidate, gradient, lambda, penalize,
                                 objective, maxBacktracks = 30L) {
  if (length(candidate) != length(beta) || length(gradient) != length(beta) ||
      length(penalize) != length(beta) || !is.logical(penalize) || anyNA(penalize) ||
      any(!is.finite(c(beta, candidate, gradient))) ||
      length(lambda) != 1L || !is.finite(lambda) || lambda < 0 ||
      length(maxBacktracks) != 1L || !is.finite(maxBacktracks) ||
      maxBacktracks < 0 || maxBacktracks != floor(maxBacktracks)) {
    stop("Invalid surrogate line-search inputs")
  }
  direction <- candidate - beta
  before <- objective(beta)
  # Composite Armijo uses the full-step L1 change, not a derivative at zero.
  decrease <- sum(gradient * direction) +
    lambda * (sum(abs(candidate[penalize])) - sum(abs(beta[penalize])))
  result <- list(beta = beta, accepted = FALSE, alpha = 0, backtracks = 0L,
    objectiveBefore = before, objectiveAfter = before, predictedDecrease = decrease,
    failureReason = "")
  if (!is.finite(before)) {
    result$failureReason <- "non_finite_surrogate_objective"
    return(result)
  }
  if (all(direction == 0)) {
    result$accepted <- TRUE
    return(result)
  }
  if (!is.finite(decrease) || decrease >= 0) {
    result$failureReason <- "non_descent_surrogate_direction"
    return(result)
  }
  for (backtracks in 0:maxBacktracks) {
    alpha <- 0.5^backtracks
    trial <- if (backtracks == 0L) candidate else beta + alpha * direction
    after <- objective(trial)
    slack <- 32 * .Machine$double.eps * max(1, abs(before))
    if (is.finite(after) && after <= before + 1e-4 * alpha * decrease + slack) {
      result$beta <- trial
      result$accepted <- TRUE
      result$alpha <- alpha
      result$backtracks <- backtracks
      result$objectiveAfter <- after
      return(result)
    }
  }
  result$backtracks <- maxBacktracks
  result$failureReason <- "surrogate_line_search_failed"
  result
}
