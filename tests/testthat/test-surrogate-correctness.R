test_that("surrogate derivatives preserve exact logistic tails", {
  for (eta in c(-1000, -40, -30, 0, 30, 40, 1000)) {
    for (y in 0:1) {
      gradient <- if (y == 1) -plogis(-eta) else plogis(eta)
      curvature <- plogis(eta) * plogis(-eta)
      for (x in list(matrix(1, 1, 1), Matrix::sparseMatrix(i = 1, j = 1, x = 1))) {
        full <- FederatedLearning:::.logisticNegGradientHessian(eta, x, y)
        diagonal <- FederatedLearning:::.logisticNegGradientHessianDiag(eta, x, y)
        gradients <- c(FederatedLearning:::.logisticNegGradient(eta, x, y),
          full$gradient, diagonal$gradient)
        hessians <- c(FederatedLearning:::.logisticNegHessian(eta, x),
          FederatedLearning:::.logisticNegHessianDiag(eta, x),
          full$hessian, diagonal$hessianDiag)
        # Relative comparisons expose errors far below a blanket 1e-8 tolerance.
        if (gradient == 0) expect_true(all(gradients == 0)) else
          expect_equal(gradients / gradient, rep(1, length(gradients)), tolerance = 1e-12)
        if (curvature == 0) expect_true(all(hessians == 0)) else
          expect_equal(hessians / curvature, rep(1, length(hessians)), tolerance = 1e-12)
      }
    }
  }
})

test_that("tail derivatives agree with loss and gradient finite differences", {
  x <- matrix(1, 1, 1)
  for (eta in c(-30, 30)) {
    y <- as.numeric(eta > 0)
    g <- FederatedLearning:::.logisticNegGradient(eta, x, y)
    h <- as.numeric(FederatedLearning:::.logisticNegHessian(eta, x))
    for (step in c(1e-3, 1e-4, 1e-5)) {
      lossDiff <- (FederatedLearning:::.negLogLikMean(eta + step, x, y) -
        FederatedLearning:::.negLogLikMean(eta - step, x, y)) / (2 * step)
      gradDiff <- (FederatedLearning:::.logisticNegGradient(eta + step, x, y) -
        FederatedLearning:::.logisticNegGradient(eta - step, x, y)) / (2 * step)
      expect_equal(g / lossDiff, 1, tolerance = 1e-6)
      expect_equal(h / gradDiff, 1, tolerance = 1e-6)
    }
  }
})

test_that("compiled outer surrogate loops also use unclipped derivatives", {
  x <- Matrix::sparseMatrix(i = 1, j = 1, x = 10)
  b <- 3
  g <- -10 * plogis(-30)
  h <- 100 * plogis(30) * plogis(-30)
  full <- FederatedLearning:::adapFullSurrogateFitCpp(
    x, 1, b, b, g, matrix(h + 1), g, matrix(h), lambda = 0,
    maxOuter = 1L, maxInner = 5L, tol = 1e-15)
  diagonal <- FederatedLearning:::adapDiagSurrogateFitCpp(
    x, 1, b, b, g, h + 1, g, h, lambda = 0,
    maxOuter = 1L, maxInner = 5L, tol = 1e-15)
  first <- FederatedLearning:::adapFirstSurrogateFitCpp(
    x, 1, b, b, 0, g, lambda = 0,
    maxOuter = 1L, maxInner = 5L, tol = 1e-15)
  for (fit in list(full, diagonal, first)) {
    expect_lt(abs(fit$beta - b), 1e-11)
  }
  # Explicit clipping replays the old behavior and must fail the same bound.
  clipped <- FederatedLearning:::adapFullSurrogateFitCpp(
    x, 1, b, b, g, matrix(h + 1), g, matrix(h), lambda = 0,
    maxOuter = 1L, maxInner = 5L, tol = 1e-15, eps = 1e-8)
  expect_gt(abs(clipped$beta - b), 1e-8)
})

test_that("federated evaluation and ADAP AUC never reverse event direction", {
  y <- c(0, 0, 1, 1)
  eta <- c(2, 1, -1, -2)
  x <- matrix(eta, ncol = 1)
  metrics <- FederatedLearning:::.evaluateBinaryMetrics(y, plogis(eta), 1)
  expect_equal(metrics$auc, 0)
  expect_equal(FederatedLearning:::.adapCvMetric(1, x, y, "auc"), 0)
  expect_equal(FederatedLearning:::.adapCvMetric(0, x, y, "auc"), 0.5)
  expect_true(is.na(FederatedLearning:::.adapCvMetric(1, x, rep(0, 4), "auc")))
})

test_that("PDA-style CV diagnostics do not alter failed-candidate selection", {
  calls <- list()
  local_mocked_bindings(
    .fitPdaAdapPdaProx = function(..., lambda, betaInit, returnDetails) {
      expect_true(returnDetails)
      calls[[length(calls) + 1L]] <<- betaInit
      # The weak candidate predicts perfectly but is not a converged fit.
      fit <- list(beta = c(0, if (lambda < 0.05) 2 else 0.2),
        converged = lambda >= 0.05, outerIterations = 1L)
      if (returnDetails) fit else fit$beta
    },
    .package = "FederatedLearning"
  )
  x <- cbind(1, rep(c(-1, 1), 15))
  y <- as.numeric(x[, 2] > 0)
  args <- list(xDesign = x, y = y, betaBar = c(0, 0),
    globalGrad = c(0, 0), globalHess = diag(2), lambdaSeq = c(.1, .01, .2),
    foldsK = 3L, seed = 17L)
  quiet <- do.call(FederatedLearning:::.pdaAdapPdaLeadCv, args)
  verbose <- do.call(FederatedLearning:::.pdaAdapPdaLeadCv,
    c(args, list(collectDiagnostics = TRUE)))
  expect_equal(quiet$lambda, verbose$lambda)
  expect_equal(quiet$scores, verbose$scores)
  expect_equal(quiet$lambda, .2)
  expect_false(is.finite(quiet$scores[2]))
  expect_equal(nrow(verbose$diagnostics), 9L)
  expect_true(all(verbose$diagnostics$fitFailed[verbose$diagnostics$lambda == .01] == 1))
  # Failed fits must not seed the next candidate in either logging mode.
  expect_true(all(vapply(calls, function(b) all(abs(b) < 1), logical(1))))
  args$lambdaSeq <- .01
  for (diagnostics in c(FALSE, TRUE)) {
    expect_error(do.call(FederatedLearning:::.pdaAdapPdaLeadCv,
      c(args, list(collectDiagnostics = diagnostics))), "no valid", ignore.case = TRUE)
  }
})

test_that("PDA-style CV diagnostics leave real fits unchanged", {
  set.seed(109)
  x <- cbind(1, matrix(rnorm(120), ncol = 2))
  y <- rep(0:1, 30)
  beta <- c(-.2, .1, .1)
  terms <- FederatedLearning:::.logisticNegGradientHessian(beta, x, y)
  for (full in c(TRUE, FALSE)) {
    h <- terms$hessian + diag(.5, 3)
    args <- list(xDesign = x, y = y, betaBar = beta,
      globalGrad = terms$gradient, globalHess = if (full) h else diag(h),
      lambdaSeq = c(.1, .02), useFull = full, foldsK = 3L,
      seed = 11L, maxIter = 500L, tol = 1e-8)
    quiet <- do.call(FederatedLearning:::.pdaAdapPdaLeadCv, args)
    verbose <- do.call(FederatedLearning:::.pdaAdapPdaLeadCv,
      c(args, list(collectDiagnostics = TRUE)))
    expect_equal(quiet$lambda, verbose$lambda)
    expect_equal(quiet$scores, verbose$scores, tolerance = 1e-12)
    expect_true(all(is.finite(quiet$scores)))
    expect_true(all(verbose$diagnostics$fitFailed == 0))
  }
})

test_that("frozen-point CV removal matches direct retained-row recomputation", {
  set.seed(110)
  x <- cbind(1, matrix(rnorm(105), ncol = 3))
  y <- rep(0:1, length.out = nrow(x))
  beta <- c(-.3, .1, -.2, .05)
  lead <- 1:12
  validation <- c(2L, 5L, 8L)
  retained <- setdiff(seq_len(nrow(x)), validation)
  terms <- function(rows) {
    FederatedLearning:::.logisticNegGradientHessian(beta, x[rows, , drop = FALSE], y[rows])
  }
  local_mocked_bindings(
    .pdaAdapSurrogateLeadCv = function(..., makeFoldInfo) {
      makeFoldInfo(list(idxVal = validation, idxTr = setdiff(lead, validation),
        nVal = length(validation)))
    },
    .package = "FederatedLearning"
  )
  global <- terms(seq_len(nrow(x)))
  remote <- terms(setdiff(seq_len(nrow(x)), lead))
  local <- terms(lead)
  weight <- length(lead) / nrow(x)
  expect_equal(global$gradient, weight * local$gradient + (1 - weight) * remote$gradient)
  expect_equal(global$hessian, weight * local$hessian + (1 - weight) * remote$hessian)
  fold <- FederatedLearning:::.pdaAdapLeadCv(x[lead, ], y[lead], beta, beta,
    global$gradient, global$hessian, lambdaSeq = .01, totalN = nrow(x))
  direct <- terms(retained)
  expect_equal(fold$gradTrainGlobal, direct$gradient, tolerance = 1e-12)
  expect_equal(fold$hessTrainGlobal, direct$hessian, tolerance = 1e-12)
})

test_that("negative correction curvature can coexist with a strict local minimum", {
  # Every feature/outcome cell is positive at both sites. Coefficients are shared.
  site <- function(prevalence) {
    counts <- as.integer(1000 * c(1 - prevalence, prevalence))
    list(x = cbind(1, rep(0:1, counts)),
      y = unlist(lapply(counts, function(n) rep(0:1, c(.98, .02) * n))))
  }
  lead <- site(.5)
  remote <- site(.1)
  beta <- c(qlogis(.02), 0)
  hl <- .02 * .98 * crossprod(lead$x) / nrow(lead$x)
  hr <- .02 * .98 * crossprod(remote$x) / nrow(remote$x)
  global <- (hl + hr) / 2
  correction <- global - hl
  eig <- eigen(correction, symmetric = TRUE)
  expect_equal(min(eigen(global, symmetric = TRUE)$values), .0037048464318529704)
  expect_equal(min(eig$values), -.006342693235899588)
  comp <- FederatedLearning:::.adapSurrogateComponents(beta, beta, lead$x, lead$y,
    globalGrad = c(0, 0), globalHess = global)
  expect_equal(comp$B, global, tolerance = 1e-12)
  expect_equal(as.numeric(comp$aTilde + comp$B %*% beta), c(0, 0), tolerance = 1e-12)
  expect_equal(comp$correction, correction, tolerance = 1e-12)
  v <- eig$vectors[, which.min(eig$values)]
  for (sign in c(-1, 1)) {
    t <- sign * 1e6
    b <- beta + t * v
    eta <- as.numeric(lead$x %*% b)
    loss <- mean(pmax(eta, 0) - lead$y * eta + log1p(exp(-abs(eta))))
    for (lambda in c(0, .1, 10)) {
      objective <- loss + sum((b - beta) * (correction %*% (b - beta))) / 2 + lambda * abs(b[2])
      expect_equal(objective / t^2, min(eig$values) / 2, tolerance = 2e-5, scale = 1)
    }
  }
  rejected <- FederatedLearning:::.fitPdaAdapSurrogate(
    lead$x, lead$y, beta, beta, c(0, 0), global, lambda = .1, returnDetails = TRUE)
  expect_equal(rejected$failureReason, "negative_C_eigenvalue")
})

test_that("a convex first-order surrogate need not have a finite minimum", {
  # Lead x is zero in 95% of entries, remote x in 5%. Both sites are nonseparated.
  x <- cbind(1, rep(0:1, c(190, 10)))
  y <- c(rep(0:1, c(152, 38)), rep(0:1, c(2, 8)))
  # At beta=0: lead gradient=(.27,-.015), remote=(-.27,-.285).
  globalGrad <- c(0, -.15)
  localGrad <- c(.27, -.015)
  correction <- globalGrad - localGrad
  lambda <- .01
  direction <- c(0, 1)
  projected <- as.numeric(x %*% direction)
  recession <- mean(pmax(projected, 0) - y * projected) +
    sum(correction * direction) + lambda * sum(abs(direction[-1]))
  expect_lt(recession, 0)
  comp <- FederatedLearning:::.adapFirstOrderSurrogateComponents(
    c(0, 0), c(0, 0), x, y, globalGrad)
  expect_gt(min(eigen(comp$B, symmetric = TRUE)$values), 0)
  b <- 1e5 * direction
  objective <- FederatedLearning:::.negLogLikMean(b, x, y) +
    sum(correction * b) + lambda * sum(abs(b[-1]))
  expect_equal(objective / 1e5, recession, tolerance = 1e-5, scale = 1)
})
