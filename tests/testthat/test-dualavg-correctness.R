test_that("exact logistic gradients and loss preserve correctly classified tails", {
  eta <- c(-40, -100, 40, 100)
  y <- c(0, 0, 1, 1)
  x <- diag(length(y))
  sparse <- FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE))
  expected <- c(plogis(-40), plogis(-100), -plogis(-40), -plogis(-100)) / length(y)
  for (matrix in list(x, sparse)) {
    actual <- gradLogistic(eta, matrix, y)
    # Absolute tolerances alone would accept a zero result in these tails.
    expect_equal(actual / expected, rep(1, length(y)), tolerance = 1e-13)
  }
  fused <- FederatedLearning:::logisticObjectiveGradientCpp(sparse, eta, y, dualStats = TRUE)
  expect_equal(as.numeric(fused$gradient) / expected, rep(1, length(y)), tolerance = 1e-13)
  expect_equal(as.numeric(fused$dualResidual) / length(y) / expected,
    rep(1, length(y)), tolerance = 1e-13)
  exact <- FederatedLearning:::logisticGradientCpp(sparse, eta, y, eps = 0)
  expect_equal(as.numeric(exact) / expected, rep(1, length(y)), tolerance = 1e-13)
  expectedLoss <- sum(log1p(exp(-abs(eta))))
  expect_equal(fused$loss / expectedLoss, 1, tolerance = 1e-13)
  expect_equal(FederatedLearning:::binaryLogLoss(eta, y, meanLoss = FALSE) / expectedLoss,
    1, tolerance = 1e-13)
  for (i in seq_along(eta)) {
    loss <- function(value) FederatedLearning:::binaryLogLoss(value, y[i])
    h <- 1e-4
    difference <- (loss(eta[i] + h) - loss(eta[i] - h)) / (2 * h)
    expect_equal(difference / (expected[i] * length(y)), 1, tolerance = 1e-7)
  }
})

test_that("PDA-specific clipping stays explicit and separate from exact gradients", {
  x <- diag(2)
  sparse <- FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE))
  beta <- c(-40, 40)
  y <- c(0, 1)
  clipped <- (pmin(pmax(plogis(beta), 1e-8), 1 - 1e-8) - y) / 2
  expect_equal(as.numeric(FederatedLearning:::logisticGradientCpp(sparse, beta, y)), clipped)
  for (matrix in list(x, sparse)) {
    expect_equal(FederatedLearning:::.logisticNegGradient(beta, matrix, y), clipped)
    expect_gt(max(abs(clipped - gradLogistic(beta, matrix, y))), 1e-9)
  }
})

test_that("DualAvg updates use true tail residuals rather than a probability floor", {
  x <- diag(rep(1e12, 2))
  sparse <- FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE))
  y <- c(0, 1)
  z <- c(-40, 40) / 1e12
  data <- list(xMatrix = sparse, yLabels = y, n = 2L)
  state <- list(z = z, r = 0L)
  config <- list(k = 1L, etaClient = 1, etaServer = 1, lambda = 0, intercept = FALSE)
  expected <- -c(plogis(-40), -plogis(-40)) * 1e12 / 2
  # Large X makes the tiny true residual visible in the dual update itself.
  for (update in list(clientUpdateDA, clientUpdateDualAveragingCpp)) {
    actual <- as.numeric(update(data, state, config)$delta)
    expect_equal(actual / expected, c(1, 1), tolerance = 1e-12)
  }
})

test_that("a coefficient restart evaluates its first gradient at the previous model", {
  x <- cbind(1, c(0, 0.2, 0.5, 1), c(0, 1, 0, 1))
  y <- c(0, 0, 1, 1)
  previous <- c(-2, 0, 0.3)
  data <- list(xMatrix = FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE)),
               yLabels = y, n = length(y))
  for (lambda in c(0, 0.01, 1)) {
    cfg <- list(p = 2L, intercept = TRUE, initialZ = previous, k = 1L,
                etaClient = 0.4, etaServer = 1.5, lambda = lambda, aggregation = "sampleSize")
    state <- serverInitDualAveragingCpp(cfg)
    state$r <- 0L
    g <- as.numeric(crossprod(x, plogis(as.numeric(x %*% previous)) - y)) / length(y)
    report <- clientUpdateDualAveragingCpp(data, state, cfg)
    expect_equal(as.numeric(report$delta), -cfg$etaClient * g, tolerance = 1e-12)
    result <- serverRoundDualAveragingCpp(state, list(report), cfg)
    z <- previous - cfg$etaClient * cfg$etaServer * g
    threshold <- cfg$etaClient * cfg$etaServer * lambda
    expected <- c(z[1], sign(z[-1]) * pmax(abs(z[-1]) - threshold, 0))
    expect_equal(as.numeric(result$report$w), expected, tolerance = 1e-12)
    expect_equal(as.numeric(result$report$w)[1], z[1], tolerance = 1e-12)
  }
})

test_that("DualAvg refuses stale counts or incompatible rows before updating", {
  x <- FederatedLearning:::.asDgCMatrix(Matrix::Matrix(cbind(1, c(-1, 1)), sparse = TRUE))
  data <- list(xMatrix = x, yLabels = c(0, 1), n = 2L)
  state <- list(z = c(0, 0), r = 0L)
  config <- list(k = 1L, etaClient = 0.1, etaServer = 1, lambda = 0.01, intercept = TRUE)
  for (update in list(clientUpdateDA, clientUpdateDualAveragingCpp)) {
    for (count in list(999, 0, -1, NA_real_, Inf, c(2, 2), numeric(), "2")) {
      bad <- data
      bad$n <- count
      expect_error(update(bad, state, config), "clientData\\$n must equal")
    }
    withoutCount <- data
    withoutCount$n <- NULL
    expect_equal(update(withoutCount, state, config)$n, 2)
    expect_equal(update(data, state, config)$n, 2)
    bad <- data
    bad$yLabels <- 0
    expect_error(update(bad, state, config), "conformable")
    expect_error(update(data, list(z = 0, r = 0L), config), "conformable")
    empty <- list(xMatrix = x[integer(), , drop = FALSE], yLabels = numeric(), n = 0)
    expect_error(update(empty, state, config), "nonempty")
  }
})

test_that("K=1 whole trajectories are invariant to unequal site partitions", {
  x <- cbind(1, c(-2, -1, 0, 1, 2, 3, 0.5), c(0, 1, 0, 0, 1, 0, 1))
  y <- c(0, 0, 0, 1, 1, 0, 1)
  n <- length(y)
  partitions <- list(list(seq_len(n)), list(1, 2:n), list(1:2, 3:5, 6:7), as.list(seq_len(n)))
  cfg <- list(p = 2L, intercept = TRUE, k = 1L, etaClient = 0.3, etaServer = 1.7,
    lambda = 0.07, aggregation = "sampleSize")
  retrieve <- function(z, threshold) c(z[1], sign(z[-1]) * pmax(abs(z[-1]) - threshold, 0))
  for (cpp in c(FALSE, TRUE)) {
    update <- if (cpp) clientUpdateDualAveragingCpp else clientUpdateDA
    aggregate <- if (cpp) serverRoundDualAveragingCpp else serverRoundDA
    for (ids in partitions) {
      clients <- lapply(ids, function(i) list(
        xMatrix = FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x[i, , drop = FALSE], sparse = TRUE)),
        yLabels = y[i], n = length(i)))
      state <- list(z = c(qlogis(mean(y)), 0, 0))
      reference <- state$z
      for (r in 0:11) {
        state$r <- r
        threshold <- cfg$etaClient * cfg$etaServer * r * cfg$lambda
        beta <- retrieve(reference, threshold)
        gradient <- as.numeric(crossprod(x, plogis(as.numeric(x %*% beta)) - y)) / n
        reference <- reference - cfg$etaClient * cfg$etaServer * gradient
        expected <- retrieve(reference, cfg$etaClient * cfg$etaServer * (r + 1) * cfg$lambda)
        reports <- lapply(clients, update, serverBroadcast = state, config = cfg)
        result <- aggregate(state, reports, cfg)
        expect_equal(as.numeric(result$state$z), reference, tolerance = 1e-12)
        expect_equal(as.numeric(result$report$w), expected, tolerance = 1e-12)
        state <- result$state
      }
    }
  }
})

test_that("heterogeneous local steps can have a nonzero pooled KKT residual at a fixed point", {
  a <- c(1, 2)
  rates <- c(0.2, 0.8)
  lambda <- 0.05
  siteScore <- function(beta) a * (plogis(a * beta) - rates) + lambda
  optimum <- uniroot(function(beta) mean(siteScore(beta)), c(0, 1), tol = 1e-14)$root
  localEndpoint <- function(beta, step, k) {
    w <- rep(beta, 2)
    for (i in seq_len(k)) w <- w - step * siteScore(w)
    w
  }
  fixed <- uniroot(function(beta) mean(localEndpoint(beta, 0.05, 2)) - beta,
    c(0, 1), tol = 1e-14)$root
  expect_equal(optimum, 0.16117462, tolerance = 1e-7)
  expect_equal(fixed, 0.15527779, tolerance = 1e-7)
  expect_equal(abs(mean(siteScore(fixed))), 0.00360832, tolerance = 1e-7)
  clients <- lapply(seq_along(a), function(m) {
    x <- cbind(1, rep(c(a[m], -a[m]), each = 10))
    cases <- as.integer(10 * rates[m])
    y <- c(rep(1, cases), rep(0, 10 - cases), rep(1, 10 - cases), rep(0, cases))
    list(xMatrix = FederatedLearning:::.asDgCMatrix(Matrix::Matrix(x, sparse = TRUE)),
      yLabels = y, n = length(y))
  })
  for (k in c(1L, 2L)) {
    beta <- if (k == 1L) optimum else fixed
    for (serverStep in c(1, 3)) {
      config <- list(k = k, etaClient = 0.05, etaServer = serverStep,
        lambda = lambda, intercept = TRUE, aggregation = "sampleSize")
      state <- list(z = c(0, beta))
      for (r in 0:3) {
        state$r <- r
        reports <- lapply(clients, clientUpdateDualAveragingCpp,
          serverBroadcast = state, config = config)
        result <- serverRoundDualAveragingCpp(state, reports, config)
        expect_equal(as.numeric(result$report$w), c(0, beta), tolerance = 1e-12)
        state <- result$state
      }
    }
  }
})
