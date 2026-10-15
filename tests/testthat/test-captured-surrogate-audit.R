loadCapturedAudit <- function() {
  path <- test_path("..", "..", "extras", "auditCapturedSurrogates.R")
  skip_if_not(file.exists(path), "Captured-input audit script is not available")
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  env
}

capturedAuditFixture <- function(root, kind = "full", method = "ADAP2") {
  site <- function(n, prevalence) {
    counts <- as.integer(n * c(1 - prevalence, prevalence))
    x <- Matrix::Matrix(cbind(1, rep(0:1, counts)), sparse = TRUE)
    y <- unlist(lapply(counts, function(n) rep(0:1, c(.98, .02) * n)))
    list(xMatrix = x, yLabels = y, n = length(y), rowId = seq_along(y) + 90000L)
  }
  lead <- site(2000, .5)
  remote <- site(1000, .1)
  bar <- c(qlogis(.02), 0)
  terms <- lapply(list(lead, remote), function(s) {
    t <- .logisticNegGradientHessian(bar, s$xMatrix, s$yLabels)
    list(n = s$n, grad = t$gradient, Hess = t$hessian, HessDiag = diag(t$hessian))
  })
  g <- (2 * terms[[1]]$grad + terms[[2]]$grad) / 3
  h <- (2 * terms[[1]]$Hess + terms[[2]]$Hess) / 3
  config <- .prepareSurrogateCapture(list(surrogateCaptureDirectory = root,
    mapping = data.frame(covariateId = 1002, columnId = 1L), intercept = TRUE), method)
  state <- list(phase = 1L, betaBar = bar, leadIndex = 1L, totalN = 3000L, odalVariant = "second")
  capture <- .captureSurrogateLead(config, lead, state)
  .captureSurrogateRound(config, state, terms, 1L)
  inputs <- list(kind = kind, betaBar = bar, betaStart = bar, globalGrad = g,
    leadGrad = terms[[1]]$grad, linearCorrection = g - terms[[1]]$grad,
    quadraticCorrection = NULL, lambda = .01, leadWeight = 1, linearTermCentered = TRUE)
  if (kind %in% c("full", "second")) {
    inputs$globalHess <- h
    inputs$leadHess <- terms[[1]]$Hess
    inputs$quadraticCorrection <- h - terms[[1]]$Hess
  } else if (kind == "diag") {
    inputs$globalHessDiag <- diag(h)
    inputs$leadHessDiag <- diag(terms[[1]]$Hess)
    inputs$quadraticCorrection <- diag(h - terms[[1]]$Hess)
  }
  list(config = config, root = config$surrogateCaptureDirectory, capture = capture,
    data = lead, inputs = inputs, state = state, reports = terms)
}

writeAuditAttempt <- function(f, value = NULL) {
  exit <- .beginSurrogateCapture(f$capture, f$data$xMatrix, f$data$yLabels, f$inputs, list())
  if (!is.null(value)) .endSurrogateCapture(exit, value)
  exit
}

test_that("independent reference preserves extreme logistic derivatives and sparse equivalence", {
  a <- loadCapturedAudit()
  for (eta in c(-1000, -40, 0, 40, 1000)) {
    for (y in 0:1) {
      dense <- a$.captureAuditTerms(matrix(1, 1, 1), y, eta)
      sparse <- a$.captureAuditTerms(Matrix::Matrix(matrix(1, 1, 1), sparse = TRUE), y, eta)
      expect_equal(dense, sparse, tolerance = 1e-14)
      expected <- if (y == 1) -plogis(-eta) else plogis(eta)
      if (expected == 0) expect_equal(dense$gradient, 0) else expect_equal(dense$gradient / expected, 1)
      h <- plogis(eta) * plogis(-eta)
      if (h == 0) expect_equal(as.numeric(dense$hessian), 0) else expect_equal(as.numeric(dense$hessian) / h, 1)
      expect_true(is.finite(dense$loss))
    }
  }
})

test_that("audit distinguishes a local minimum from a negative quadratic tail", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (kind in c("full", "diag")) {
    f <- capturedAuditFixture(root, kind)
    writeAuditAttempt(f, list(beta = f$inputs$betaBar, converged = FALSE,
      failureReason = "negative_C_eigenvalue"))
    before <- tools::md5sum(list.files(f$root, full.names = TRUE))
    report <- a$auditCapturedSurrogates(f$root)
    expect_identical(tools::md5sum(names(before)), before)
    expect_lt(report$attempts$correctionMin, 0)
    expect_true(all(report$states$hessianMin > 0))
    expect_true(all(report$states$stationary1e8))
    expect_true(all(report$checks$within1e10))
    expect_false(report$attempts$solverConverged)
    expect_equal(report$attempts$exitStatus, "returned")
    tails <- subset(report$rays, step == 1e6 & direction == "negative_correction")
    expect_true(all(tails$objective < 0))
    expect_equal(tails$objectiveOverStepSquared, tails$quadraticRate, tolerance = 1e-5, scale = 1)
    expect_lt(max(report$finiteDifferences$gradientError), 1e-8)
    expect_lt(max(report$finiteDifferences$hessianVectorError), 1e-8)
    expect_false(any(report$attempts$globalRowsRecomputed))
  }
})

test_that("audit preserves ODAL affine constants and never penalizes the intercept", {
  a <- loadCapturedAudit()
  data <- list(xDesign = cbind(1, c(-1, 0, 1, 2)), y = c(0, 1, 0, 1))
  input <- list(kind = "first", betaBar = c(-1, .3), linearCorrection = c(.1, .2),
    quadraticCorrection = NULL, lambda = .05, leadWeight = 1,
    linearTermCentered = TRUE, penalize = c(FALSE, TRUE))
  beta <- c(2, -.7)
  centered <- a$.captureAuditObjective(data, input, beta)
  input$linearTermCentered <- FALSE
  actual <- a$.captureAuditObjective(data, input, beta)
  expect_equal(actual$objective - centered$objective, sum(input$linearCorrection * input$betaBar))
  expect_equal(actual$gradient, centered$gradient)
  expect_equal(actual$penalty, .05 * .7)
  state <- a$.captureAuditState(data, input, beta)
  expect_equal(state$kktMaxAbs, max(abs(actual$gradient + c(0, -.05))))
  input$lambda <- 0
  state <- a$.captureAuditState(data, input, beta)
  expect_true(is.finite(state$newtonStepL2))
  expect_true(is.finite(state$newtonDecrementSquared))
  input$kind <- "full"
  input$quadraticCorrection <- -diag(2)
  state <- a$.captureAuditState(data, input, c(1e200, 0))
  expect_false(state$finite)
  expect_equal(state$objective, -Inf)
  expect_true(is.finite(state$leadLoss))
})

test_that("captured corrections are audited rather than repaired", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root)
  f$inputs$leadGrad <- f$inputs$leadGrad + .1
  f$inputs$linearCorrection <- f$inputs$linearCorrection - .1
  writeAuditAttempt(f)
  report <- a$auditCapturedSurrogates(f$root)
  checks <- report$checks
  expect_true(checks$within1e10[checks$check == "stored_linear_correction_identity"])
  expect_false(checks$within1e10[checks$check == "stored_lead_gradient_vs_exact"])
  expect_false(checks$within1e10[checks$check == "frozen_expansion_gradient_vs_global"])
  expect_equal(report$attempts$exitStatus, "missing_exit")
  expect_setequal(report$states$state, c("expansion", "start"))
})

test_that("real CV snapshots verify unequal-site weighting and validation removal", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root, "first", "ADAP1")
  result <- .pdaAdapFirstLeadCv(f$data$xMatrix, f$data$yLabels,
    betaLead = f$inputs$betaBar, betaBar = f$inputs$betaBar, globalGrad = f$inputs$globalGrad,
    lambdaSeq = .1, totalN = 3000, foldsK = 3L, search = "grid", cvMaxRows = 120,
    maxOuter = 100L, capture = f$capture)
  expect_equal(result$lambda, .1)
  report <- a$auditCapturedSurrogates(f$root)
  expect_equal(nrow(report$attempts), 3)
  expect_true(all(report$attempts$n == 80))
  expect_true(all(report$attempts$phase == "cv"))
  expect_true(all(report$checks$within1e10))
  expect_true(all(report$attempts$solverConverged))
  selected <- a$auditCapturedSurrogates(f$root, attempts = 2)
  expect_equal(as.list(selected$attempts), as.list(report$attempts[2, ]))
  expect_equal(selected$attempts$attempt, 2)
  expect_error(a$auditCapturedSurrogates(f$root, attempts = c(1, 1)), "selection")
  expect_error(a$auditCapturedSurrogates(f$root, attempts = 4), "selection")
})

test_that("actual ODAL and ADAP1 final returns are audited without refitting", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  optionsBefore <- options(FederatedLearning.localId = 1L)
  on.exit(options(optionsBefore), add = TRUE)
  f <- capturedAuditFixture(root, "second", "ODAL2")
  state <- f$state
  state$phase <- 2L
  state$otherGrad <- f$inputs$globalGrad
  state$otherHess <- f$inputs$globalHess
  fit <- .clientUpdateODAL(f$data, state, f$config)
  report <- a$auditCapturedSurrogates(f$root)
  expect_equal(report$attempts$optimCode, fit$convergence)
  expect_equal(report$states$objective[report$states$state == "returned"], fit$value)
  expect_true(all(report$checks$within1e10))

  f <- capturedAuditFixture(root, "first", "ADAP1")
  fit <- .fitPdaAdapFirstOrderSurrogate(f$data$xMatrix, f$data$yLabels,
    f$inputs$betaBar, f$inputs$betaBar, f$inputs$globalGrad,
    lambda = .1, returnDetails = TRUE, capture = f$capture)
  report <- a$auditCapturedSurrogates(f$root)
  expect_equal(report$attempts$solverConverged, fit$converged)
  expect_true(report$states$stationary1e8[report$states$state == "returned"])
})

test_that("server rejection is audited without inventing an optimizer attempt", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root, "second", "ODAL2")
  f$config$odalCurvatureAction <- "fail"
  expect_error(.serverRoundODAL(f$state, f$reports, f$config), "indefinite")
  report <- a$auditCapturedSurrogates(f$root)
  expect_equal(report$attempts$exitStatus, "rejected_before_solver")
  expect_equal(report$states$state, "expansion")
  expect_true(all(report$checks$within1e10))
  expect_equal(report$attempts$lambda, 0)
})

test_that("rejected full and diagonal CV captures retain frozen removal identities", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (kind in c("full", "diag")) {
    f <- capturedAuditFixture(root, kind)
    args <- list(xDesign = f$data$xMatrix, y = f$data$yLabels,
      betaLead = f$inputs$betaBar, betaBar = f$inputs$betaBar, globalGrad = f$inputs$globalGrad,
      lambdaSeq = .1, totalN = 3000, foldsK = 3L, search = "grid", capture = f$capture)
    if (kind == "full") {
      args$globalHess <- f$inputs$globalHess
      fitter <- .pdaAdapLeadCv
    } else {
      args$globalHessDiag <- f$inputs$globalHessDiag
      fitter <- .pdaAdapDiagLeadCv
    }
    expect_error(do.call(fitter, args), "failed|valid", ignore.case = TRUE)
    report <- a$auditCapturedSurrogates(f$root)
    expect_equal(nrow(report$attempts), 3)
    expect_true(all(report$checks$within1e10))
    expect_true(all(nzchar(report$attempts$failureReason)))
    if (kind == "full") {
      inputs <- lapply(list.files(f$root, "^input-", full.names = TRUE), readRDS)
      expected <- vapply(inputs, function(i) min(eigen(i$inputs$globalHess, symmetric = TRUE)$values), numeric(1))
      expect_equal(report$attempts$storedGlobalHessianMin, expected)
    } else expect_true(all(is.na(report$attempts$storedGlobalHessianMin)))
  }
})

test_that("missing reports are unavailable and audit never invokes a fitter", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root, "first", "ADAP1")
  writeAuditAttempt(f)
  unlink(file.path(f$root, "round-1.rds"))
  local_mocked_bindings(optim = function(...) stop("must not fit"), .package = "stats")
  report <- a$auditCapturedSurrogates(f$root)
  check <- subset(report$checks, check == "global_reports_unavailable")
  expect_false(check$available)
  expect_true(is.na(check$within1e10))
})

test_that("first-order recession witnesses and failed returned states remain explicit", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root, "first", "ADAP1")
  f$inputs$linearCorrection <- c(0, -2)
  f$inputs$globalGrad <- f$inputs$leadGrad + f$inputs$linearCorrection
  exit <- writeAuditAttempt(f, list(beta = c(0, 1e5), converged = FALSE,
    failureReason = "max_outer_no_convergence"))
  report <- a$auditCapturedSurrogates(f$root)
  expect_true(any(report$rays$firstOrderRecession < 0))
  expect_false(report$attempts$solverConverged)
  saveRDS(list(status = "returned", value = list(beta = c(Inf, 0))), exit)
  report <- a$auditCapturedSurrogates(f$root)
  expect_false(report$states$finite[report$states$state == "returned"])
  saveRDS(list(status = "unwound_without_return", value = NULL), exit)
  report <- a$auditCapturedSurrogates(f$root)
  expect_equal(report$attempts$exitStatus, "unwound_without_return")
  expect_false("returned" %in% report$states$state)
})

test_that("malformed hashes, rows, dimensions and filenames fail explicitly", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root)
  exit <- writeAuditAttempt(f)
  path <- sub("/exit-", "/input-", exit)
  input <- readRDS(path)
  expect_error(a$auditCapturedSurrogates(f$root, maxColumns = 1), "maxColumns")
  corrupt <- input
  corrupt$dataHash <- "wrong"
  saveRDS(corrupt, path)
  expect_error(a$auditCapturedSurrogates(f$root), "fingerprint")
  corrupt <- input
  corrupt$context$validationRows <- 1L
  saveRDS(corrupt, path)
  expect_error(a$auditCapturedSurrogates(f$root), "overlap validation")
  corrupt <- input
  corrupt$inputs$betaStart <- 1
  saveRDS(corrupt, path)
  expect_error(a$auditCapturedSurrogates(f$root), "invalid captured")
  corrupt <- input
  corrupt$dataFile <- "../other.rds"
  saveRDS(corrupt, path)
  expect_error(a$auditCapturedSurrogates(f$root), "filename")
  corrupt <- input
  corrupt$context$rows <- rev(corrupt$context$rows)
  saveRDS(corrupt, path)
  expect_error(a$auditCapturedSurrogates(f$root), "do not match")
})

test_that("export requires approval and includes only scalar summary tables", {
  a <- loadCapturedAudit()
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  f <- capturedAuditFixture(root)
  writeAuditAttempt(f)
  output <- file.path(root, "aggregate-review")
  expect_error(a$auditCapturedSurrogates(f$root, output), "disclosureApproved")
  expect_false(dir.exists(output))
  report <- a$auditCapturedSurrogates(f$root, output, disclosureApproved = TRUE)
  expect_true(all(vapply(report, is.data.frame, logical(1))))
  expect_true(all(vapply(unlist(lapply(report, unclass), recursive = FALSE), is.atomic, logical(1))))
  expect_true(all(c("attempts.csv", "states.csv", "checks.csv", "README.txt") %in% list.files(output)))
  expect_error(a$auditCapturedSurrogates(f$root, output, disclosureApproved = TRUE), "already exists")
  text <- paste(unlist(lapply(list.files(output, full.names = TRUE), readLines)), collapse = "\n")
  expect_false(grepl(f$root, text, fixed = TRUE))
  expect_false(grepl("90001", text, fixed = TRUE))
  second <- file.path(root, "reviewed-export")
  expect_error(a$writeCapturedSurrogateAudit(report, second), "disclosureApproved")
  a$writeCapturedSurrogateAudit(report, second, disclosureApproved = TRUE)
  expect_equal(utils::read.csv(file.path(second, "states.csv")),
    utils::read.csv(file.path(output, "states.csv")))
  expect_error(a$writeCapturedSurrogateAudit(readRDS(file.path(f$root, "lead.rds")),
    file.path(root, "bad-export"), disclosureApproved = TRUE), "not raw captures")
})
