# For mean logistic loss, q = y + r is dual feasible when q is in [0, 1],
# the unpenalized-intercept constraint mean(r) = 0 holds, and
# ||mean(X_pen * r)||_inf <= lambda. Its dual objective is mean(binary entropy(q)).
.dualGapRequest <- function(reports, lambda, intercept, aggregation, round) {
  mass <- as.numeric(weightedReportAverage(reports, "dualMass", aggregation))
  gradient <- weightedReportAverage(reports, "dualClassGradient", aggregation)
  scale <- as.numeric(weightedReportAverage(reports, "dualGradientScale", aggregation))
  if (length(mass) != 2L || any(!is.finite(mass)) || any(mass < 0) ||
      !is.matrix(gradient) || ncol(gradient) != 2L || any(!is.finite(gradient)) ||
      length(scale) != 1L || !is.finite(scale) || scale < 0 ||
      length(lambda) != 1L || !is.finite(lambda) || lambda <= 0) {
    stop("Invalid dual candidate summaries or lambda")
  }
  scales <- c(1, 1)
  if (isTRUE(intercept)) {
    scales <- if (any(mass == 0)) c(0, 0) else min(mass) / mass
  }
  penalized <- seq_len(nrow(gradient))
  if (isTRUE(intercept)) penalized <- penalized[-1L]
  score <- as.numeric(gradient %*% (scales * c(1, -1)))
  norm <- max(c(0, abs(score[penalized])))
  # Allow for cancellation between class totals, not just relative error in lambda.
  margin <- 64 * .Machine$double.eps * max(scale, norm, lambda)
  scales <- scales * min(1, lambda / (norm + margin))
  norm <- max(c(0, abs(as.numeric(gradient %*% (scales * c(1, -1))))[penalized]))
  list(round = round, scales = scales, gradientMaxAbs = norm)
}

.checkedDualityGap <- function(primal, lowerBound) {
  if (length(primal) != 1L || length(lowerBound) != 1L ||
      !is.finite(primal) || !is.finite(lowerBound) || primal < 0 || lowerBound < 0) {
    stop("Duality gap requires finite non-negative objectives")
  }
  raw <- primal - lowerBound
  if (raw < -64 * .Machine$double.eps * max(1, abs(primal), abs(lowerBound))) {
    stop("Negative duality gap: dual feasibility or objective scaling is incorrect")
  }
  max(0, raw)
}

.dualGapEvaluation <- function(reports, request, lambda, intercept, aggregation) {
  evaluations <- lapply(reports, `[[`, "dualEvaluation")
  if (any(vapply(evaluations, is.null, logical(1))) ||
      any(vapply(evaluations, `[[`, numeric(1), "round") != request$round)) {
    stop("Dual candidate round mismatch")
  }
  weights <- reportWeights(reports, aggregation)
  entropy <- vapply(evaluations, `[[`, numeric(1), "entropy")
  balances <- vapply(evaluations, `[[`, numeric(1), "balance")
  balance <- sum(weights * balances)
  if (any(!is.finite(entropy)) || any(entropy < 0) || any(entropy > log(2) + 1e-14) ||
      any(!is.finite(balances)) || !is.finite(request$gradientMaxAbs) ||
      request$gradientMaxAbs > lambda || (isTRUE(intercept) && abs(balance) > 1e-12)) {
    stop(sprintf("Dual feasibility check failed: gradient norm %.17g, lambda %.17g, balance %.17g, entropy range [%.17g, %.17g]",
      request$gradientMaxAbs, lambda, balance, min(entropy), max(entropy)))
  }
  list(objective = sum(weights * entropy), balance = balance)
}
