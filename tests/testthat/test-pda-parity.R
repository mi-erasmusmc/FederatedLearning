skip_if_no_pda_reference <- function() {
  skip_if_not(requireNamespace("pda", quietly = TRUE), "pda is not installed")
  pdaNs <- asNamespace("pda")
  required <- c(
    "grad_nll",
    "hess_full_nll",
    "hess_diag_nll",
    "nll",
    "ADAP.initialize",
    "ADAP.derive",
    "ADAP.estimate",
    "ODAL.derive"
  )
  missing <- required[!vapply(required, exists, logical(1), envir = pdaNs, inherits = FALSE)]
  skip_if(
    length(missing) > 0L,
    paste("installed pda does not expose reference ADAP helpers:", paste(missing, collapse = ", "))
  )
}

test_that("pda ADAP primitives match local logistic derivatives", {
  skip_if_no_pda_reference()

  set.seed(101)
  xRaw <- matrix(rnorm(36), nrow = 12)
  xDesign <- cbind(1, xRaw)
  y <- rbinom(12, 1, 0.4)
  beta <- c(-0.1, 0.2, -0.05, 0.03)
  pdaNs <- asNamespace("pda")

  expect_equal(
    FederatedLearning:::.logisticNegGradient(beta, Matrix::Matrix(xDesign, sparse = TRUE), y),
    get("grad_nll", pdaNs)(beta, xDesign, y),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::.logisticNegHessian(beta, Matrix::Matrix(xDesign, sparse = TRUE)),
    get("hess_full_nll", pdaNs)(beta, xDesign),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::.logisticNegHessianDiag(beta, Matrix::Matrix(xDesign, sparse = TRUE)),
    get("hess_diag_nll", pdaNs)(beta, xDesign),
    tolerance = 1e-12
  )
  expect_equal(
    FederatedLearning:::.negLogLikMean(beta, Matrix::Matrix(xDesign, sparse = TRUE), y),
    get("nll", pdaNs)(beta, xDesign, y),
    tolerance = 1e-12
  )
})

test_that("pda ADAP derive matches local phase-one summaries", {
  skip_if_no_pda_reference()

  set.seed(102)
  xRaw <- matrix(rnorm(45), nrow = 15)
  xDesign <- cbind(1, xRaw)
  y <- rbinom(15, 1, 0.45)
  beta0 <- c(0.1, -0.05, 0.03, 0.08)
  ipdata <- data.frame(status = y, xDesign)
  clientData <- list(
    xMatrix = Matrix::Matrix(xRaw, sparse = TRUE),
    yLabels = y,
    n = length(y)
  )
  pdaNs <- asNamespace("pda")

  pdaFull <- get("ADAP.derive", pdaNs)(
    ipdata = ipdata,
    control = list(beta0 = beta0, hessian_full = TRUE),
    config = list(site_id = "site1")
  )
  oursFull <- FederatedLearning:::.clientUpdatePdaAdap(
    clientData,
    serverBroadcast = list(phase = 1L, betaBar = beta0),
    config = list(intercept = FALSE)
  )
  expect_equal(oursFull$grad, pdaFull$logL_D1, tolerance = 1e-12)
  expect_equal(unname(oursFull$Hess), unname(pdaFull$logL_D2), tolerance = 1e-12)

  pdaDiag <- get("ADAP.derive", pdaNs)(
    ipdata = ipdata,
    control = list(beta0 = beta0, hessian_full = FALSE),
    config = list(site_id = "site1")
  )
  oursDiag <- FederatedLearning:::.clientUpdatePdaAdapReduced(
    clientData,
    serverBroadcast = list(phase = 1L, betaBar = beta0, adapReducedMode = "diag"),
    config = list(intercept = FALSE)
  )
  expect_equal(oursDiag$grad, pdaDiag$logL_D1, tolerance = 1e-12)
  expect_equal(oursDiag$HessDiag, pdaDiag$logL_D2, tolerance = 1e-12)
})

test_that("pda ADAP initialize matches local glmnet initialization when settings match", {
  skip_if_no_pda_reference()

  set.seed(103)
  xRaw <- matrix(rnorm(120), nrow = 40)
  y <- rbinom(40, 1, stats::plogis(0.2 + 0.5 * xRaw[, 1]))
  ipdata <- data.frame(status = y, cbind(1, xRaw))
  clientData <- list(
    xMatrix = Matrix::Matrix(xRaw, sparse = TRUE),
    yLabels = y,
    n = length(y)
  )
  pdaNs <- asNamespace("pda")

  set.seed(104)
  pdaInit <- get("ADAP.initialize", pdaNs)(
    ipdata = ipdata,
    control = list(),
    config = list(site_id = "site1")
  )
  set.seed(104)
  ours <- FederatedLearning:::.clientUpdatePdaAdap(
    clientData,
    serverBroadcast = list(phase = 0L),
    config = list(intercept = FALSE)
  )

  expect_equal(ours$bhat, pdaInit$bhat_i, tolerance = 1e-8)
  expect_equal(ours$n, pdaInit$site_size)
})

test_that("pda ODAL derivatives match local negative-log-likelihood sign convention", {
  skip_if_no_pda_reference()

  set.seed(105)
  xDesign <- cbind(1, matrix(rnorm(40), nrow = 10))
  y <- rbinom(10, 1, 0.5)
  beta0 <- c(-0.1, 0.2, 0.05, -0.03, 0.08)
  ipdata <- data.frame(status = y, xDesign)
  pdaNs <- asNamespace("pda")

  pdaDeriv <- get("ODAL.derive", pdaNs)(
    ipdata = ipdata,
    control = list(beta_init = beta0),
    config = list(site_id = "site1")
  )
  xSparse <- Matrix::Matrix(xDesign, sparse = TRUE)

  expect_equal(
    FederatedLearning:::.logisticNegGradient(beta0, xSparse, y),
    -as.numeric(pdaDeriv$logL_D1),
    tolerance = 1e-12
  )
  expect_equal(
    unname(FederatedLearning:::.logisticNegHessian(beta0, xSparse)),
    unname(-as.matrix(pdaDeriv$logL_D2)),
    tolerance = 1e-12
  )
})

test_that("ADAP_PDA final lead-site estimate matches public PDA full-CD surrogate equations", {
  skip_if_no_pda_reference()

  set.seed(106)
  n1 <- 24
  n2 <- 20
  p <- 4
  x1 <- matrix(rnorm(n1 * p), nrow = n1)
  x2 <- matrix(rnorm(n2 * p), nrow = n2)
  y1 <- rbinom(n1, 1, stats::plogis(0.1 + 0.4 * x1[, 1] - 0.3 * x1[, 2]))
  y2 <- rbinom(n2, 1, stats::plogis(-0.2 + 0.5 * x2[, 1]))
  ip1 <- data.frame(status = y1, cbind(1, x1))
  ip2 <- data.frame(status = y2, cbind(1, x2))
  beta0 <- rep(0, p + 1L)
  control <- list(
    beta0 = beta0,
    hessian_full = TRUE,
    sites = c("site1", "site2"),
    cv_seed = 107L,
    nfolds = 3L,
    maxIter = 80L,
    tol = 1e-7,
    hessian_ridge = 1e-4
  )
  pdaNs <- asNamespace("pda")
  d1 <- get("ADAP.derive", pdaNs)(ip1, control, list(site_id = "site1"))
  d2 <- get("ADAP.derive", pdaNs)(ip2, control, list(site_id = "site2"))
  tmp <- tempfile("pda-parity-")
  dir.create(tmp)
  pdaConfig <- list(site_id = "site1", dir = tmp)
  pda::pdaPut(d1, "site1_derive", pdaConfig, upload_without_confirm = TRUE, silent_message = TRUE, digits = 16)
  pda::pdaPut(d2, "site2_derive", pdaConfig, upload_without_confirm = TRUE, silent_message = TRUE, digits = 16)

  weights <- c(n1, n2) / (n1 + n2)
  globalGrad <- as.numeric(cbind(d1$logL_D1, d2$logL_D1) %*% weights)
  globalHess <- d1$logL_D2 * weights[1] + d2$logL_D2 * weights[2]
  xSparse <- Matrix::Matrix(cbind(1, x1), sparse = TRUE)
  lambdaSeq <- FederatedLearning:::.pdaAdapLambdaSeq(
    xDesign = xSparse,
    y = y1,
    betaLead = beta0,
    betaBar = beta0,
    globalGrad = globalGrad,
    globalHess = globalHess,
    gridLen = 100L
  )
  cv <- FederatedLearning:::.pdaAdapLeadCv(
    xDesign = xSparse,
    y = y1,
    betaLead = beta0,
    betaBar = beta0,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambdaSeq = lambdaSeq,
    totalN = n1 + n2,
    foldsK = control$nfolds,
    seed = control$cv_seed,
    maxOuter = control$maxIter,
    maxInner = 100L,
    tol = control$tol
  )
  expected <- FederatedLearning:::.fitPdaAdapSurrogate(
    xDesign = xSparse,
    y = y1,
    betaLead = beta0,
    betaBar = beta0,
    globalGrad = globalGrad,
    globalHess = globalHess,
    lambda = cv$lambda,
    maxOuter = control$maxIter,
    maxInner = 100L,
    tol = control$tol
  )

  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 1L)
  ours <- FederatedLearning:::.clientUpdatePdaAdap(
    clientData = list(
      xMatrix = Matrix::Matrix(x1, sparse = TRUE),
      yLabels = y1,
      n = n1
    ),
    serverBroadcast = list(
      phase = 2L,
      leadIndex = 1L,
      betaBar = beta0,
      betaLead = beta0,
      globalGrad = globalGrad,
      globalHess = globalHess,
      totalN = n1 + n2,
      adapSolveStyle = "fullQuadratic"
    ),
    config = list(
      intercept = FALSE,
      cvSeed = control$cv_seed,
      foldsK = control$nfolds,
      maxIter = control$maxIter,
      tol = control$tol,
      hessian_ridge = control$hessian_ridge
    )
  )

  expect_equal(ours$selectedLambda, cv$lambda, tolerance = 1e-12)
  expect_equal(ours$w, expected, tolerance = 1e-8)
})

test_that("ADAPDiag pda style matches pda ADAP.estimate with diagonal Hessian", {
  skip_if_no_pda_reference()

  set.seed(108)
  n1 <- 25
  n2 <- 22
  p <- 4
  x1 <- matrix(rnorm(n1 * p), nrow = n1)
  x2 <- matrix(rnorm(n2 * p), nrow = n2)
  y1 <- rbinom(n1, 1, stats::plogis(-0.1 + 0.35 * x1[, 1] + 0.25 * x1[, 3]))
  y2 <- rbinom(n2, 1, stats::plogis(0.2 + 0.45 * x2[, 1] - 0.15 * x2[, 2]))
  ip1 <- data.frame(status = y1, cbind(1, x1))
  ip2 <- data.frame(status = y2, cbind(1, x2))
  beta0 <- rep(0, p + 1L)
  control <- list(
    beta0 = beta0,
    hessian_full = FALSE,
    sites = c("site1", "site2"),
    cv_seed = 109L,
    nfolds = 3L,
    maxIter = 80L,
    tol = 1e-7,
    hessian_ridge = 1e-4
  )
  pdaNs <- asNamespace("pda")
  d1 <- get("ADAP.derive", pdaNs)(ip1, control, list(site_id = "site1"))
  d2 <- get("ADAP.derive", pdaNs)(ip2, control, list(site_id = "site2"))
  tmp <- tempfile("pda-diag-parity-")
  dir.create(tmp)
  pdaConfig <- list(site_id = "site1", dir = tmp)
  pda::pdaPut(d1, "site1_derive", pdaConfig, upload_without_confirm = TRUE, silent_message = TRUE, digits = 16)
  pda::pdaPut(d2, "site2_derive", pdaConfig, upload_without_confirm = TRUE, silent_message = TRUE, digits = 16)
  pdaOutput <- utils::capture.output(
    pdaFit <- get("ADAP.estimate", pdaNs)(ip1, control, pdaConfig)
  )
  expect_match(pdaOutput[length(pdaOutput)], "\\[ADAP\\]\\[site1\\]")

  weights <- c(n1, n2) / (n1 + n2)
  globalGrad <- as.numeric(cbind(d1$logL_D1, d2$logL_D1) %*% weights)
  globalHessDiag <- as.numeric(cbind(d1$logL_D2, d2$logL_D2) %*% weights)

  old <- getOption("FederatedLearning.localId")
  on.exit(options(FederatedLearning.localId = old), add = TRUE)
  options(FederatedLearning.localId = 1L)
  ours <- FederatedLearning:::.clientUpdatePdaAdapReduced(
    clientData = list(
      xMatrix = Matrix::Matrix(x1, sparse = TRUE),
      yLabels = y1,
      n = n1
    ),
    serverBroadcast = list(
      phase = 2L,
      adapReducedMode = "diag",
      adapDiagStyle = "pda",
      leadIndex = 1L,
      betaBar = beta0,
      betaLead = beta0,
      globalGrad = globalGrad,
      globalHessDiag = globalHessDiag,
      totalN = n1 + n2
    ),
    config = list(
      intercept = FALSE,
      cvSeed = control$cv_seed,
      foldsK = control$nfolds,
      maxIter = control$maxIter,
      tol = control$tol,
      hessian_ridge = control$hessian_ridge
    )
  )

  expect_equal(ours$selectedLambda, pdaFit$lambda, tolerance = 1e-12)
  expect_equal(ours$w, pdaFit$btilde, tolerance = 1e-8)
})
