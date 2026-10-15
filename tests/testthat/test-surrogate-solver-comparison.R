loadSolverComparison <- function() {
  env <- new.env(parent = globalenv())
  for (name in c("auditCapturedSurrogates.R", "compareSurrogateSolvers.R")) {
    path <- test_path("..", "..", "extras", name)
    skip_if_not(file.exists(path), "Solver comparison scripts are not available")
    sys.source(path, env)
  }
  env
}

solverComparisonPda <- function(env) {
  if (!requireNamespace("pda", quietly = TRUE)) {
    if (nzchar(Sys.getenv("FEDERATEDLEARNING_PDA_REF"))) stop("Pinned PDA is required in CI")
    skip("pda is not installed")
  }
  env$.solverPdaCd()
}

test_that("bounded controls have independently verifiable penalized optima", {
  e <- loadSolverComparison()
  controls <- e$surrogateSolverControls()
  for (name in names(controls)[grepl("^bounded", names(controls))]) {
    f <- controls[[name]]
    t <- e$.captureAuditObjective(f$data, f$inputs, f$oracle)
    expect_lt(e$.solverKkt(f$oracle, t$gradient, f$inputs$lambda), 1e-12)
    expect_gt(min(eigen(t$hessian, symmetric = TRUE)$values), 0)
    expect_lt(e$.captureAuditObjective(f$data, f$inputs, f$oracle)$objective,
      e$.captureAuditObjective(f$data, f$inputs, f$oracle + .2)$objective)
  }
  expect_equal(e$.solverKkt(c(3, 0, -1), c(.01, .02, .03), .03), .01)
})

test_that("analytic and numerical BFGS optimize the same frozen smooth objective", {
  e <- loadSolverComparison()
  for (name in c("bounded_full", "bounded_first", "bounded_rare_near")) {
    f <- e$surrogateSolverControls()[[name]]
    before <- rlang::hash(f)
    r <- e$compareSurrogateSolvers(f$data, f$inputs, c("bfgs_numeric", "bfgs_analytic"))
    expect_identical(rlang::hash(f), before)
    target <- e$.captureAuditObjective(f$data, f$inputs, f$oracle)$objective
    expect_lt(max(abs(r$summary$objective - target)), 1e-9)
    expect_lt(max(r$summary$kktMaxAbs), 1e-5)
    betas <- split(r$coefficients$beta, r$coefficients$solver)
    expect_equal(betas$bfgs_numeric, betas$bfgs_analytic, tolerance = 1e-4)
    expect_gt(r$summary$functionCalls[1], r$summary$functionCalls[2])
    expect_true(all(r$summary$optimCode == 0))
    expect_false(any(r$evaluations$sentinel))
    expect_equal(r$evaluations$rawObjective, r$evaluations$suppliedObjective)
    expect_equal(r$summary$auditKktPassed, r$summary$kktMaxAbs <= 1e-7)
  }
})

test_that("current and original PDA CD reach common KKT targets on bounded controls", {
  e <- loadSolverComparison()
  solverComparisonPda(e)
  for (name in c("bounded_full", "bounded_lasso", "bounded_diagonal_lasso", "bounded_first", "bounded_first_lasso")) {
    f <- e$surrogateSolverControls()[[name]]
    r <- e$compareSurrogateSolvers(f$data, f$inputs, c("current_cd", "pda_cd"))
    expect_true(all(r$summary$auditKktPassed), info = name)
    expect_true(all(r$summary$status == "kkt_gate"))
    expect_lt(max(abs(r$coefficients$beta - rep(f$oracle, 2))), 1e-4)
    expect_lt(max(r$steps$objectiveChange), 1e-10)
    expect_lt(max(r$steps$quadraticChange), 1e-10)
    expect_identical(r$settings$pdaRef, "ee90de152f6886bcba1404cce9cde11cb31f3777")
    if (f$inputs$lambda > 0) {
      expect_equal(r$coefficients$beta[r$coefficients$column == 3], c(0, 0))
      expect_true(all(r$coefficients$beta[r$coefficients$column == 1] != 0))
    }
  }
})

test_that("common-scaffold first steps match the actual compiled surrogate fitters", {
  e <- loadSolverComparison()
  for (name in c("bounded_lasso", "bounded_first_lasso", "bounded_diagonal_lasso", "indefinite_near", "bounded_rare_poor")) {
    f <- e$surrogateSolverControls()[[name]]
    i <- f$inputs
    args <- list(x = as(Matrix::Matrix(f$data$xDesign, sparse = TRUE), "dgCMatrix"),
      y = f$data$y, betaStart = i$betaStart, betaBar = i$betaBar,
      globalGrad = i$globalGrad, gradBar = i$leadGrad, lambda = i$lambda,
      maxOuter = 1L, maxInner = 1000L, tol = 1e-10)
    if (i$kind == "full") {
      args$globalHess <- i$globalHess
      args$hBar <- i$leadHess
      fitter <- FederatedLearning:::adapFullSurrogateFitCpp
    } else if (i$kind == "diag") {
      args$globalHessDiag <- i$globalHessDiag
      args$hBarDiag <- i$leadHessDiag
      fitter <- FederatedLearning:::adapDiagSurrogateFitCpp
    } else fitter <- FederatedLearning:::adapFirstSurrogateFitCpp
    compiled <- do.call(fitter, args)
    r <- e$compareSurrogateSolvers(f$data, i, "current_cd", maxOuter = 1L)
    expect_equal(r$coefficients$beta, as.numeric(compiled$beta), tolerance = 1e-10)
    expect_equal(r$steps$objectiveAfter,
      e$.captureAuditObjective(f$data, i, compiled$beta)$objective, tolerance = 1e-12)
  }
})

test_that("a solved quadratic can worsen a bounded nonlinear objective for both kernels", {
  e <- loadSolverComparison()
  solverComparisonPda(e)
  f <- e$surrogateSolverControls()$bounded_rare_poor
  r <- e$compareSurrogateSolvers(f$data, f$inputs, c("current_cd", "pda_cd"), maxOuter = 2L)
  expect_true(all(r$steps$quadraticChange < -100))
  expect_true(all(r$steps$quadraticKkt < 1e-10))
  expect_true(all(r$steps$objectiveChange > 40))
  expect_true(all(r$summary$status == "kernel_error"))
  expect_false(any(r$summary$auditKktPassed))
  expect_equal(r$coefficients$beta[1], r$coefficients$beta[2], tolerance = 1e-10)
  expect_equal(r$summary$kktMaxAbs, c(.02, .02), tolerance = 1e-10)
})

test_that("negative fixed curvature permits a local solution without a global minimum", {
  e <- loadSolverComparison()
  solverComparisonPda(e)
  for (name in c("indefinite_near", "indefinite_near_lasso")) {
    f <- e$surrogateSolverControls()[[name]]
    r <- e$compareSurrogateSolvers(f$data, f$inputs, c("current_cd", "pda_cd"))
    expect_true(all(r$summary$auditKktPassed))
    expect_true(all(r$summary$correctionMin < 0))
    expect_true(all(r$summary$hessianMin > 0))
    expect_lt(max(abs(r$coefficients$beta - rep(f$localStationaryPoint, 2))), 1e-4)
    direction <- e$.captureAuditSpectrum(f$inputs$quadraticCorrection)$direction
    tail <- e$.captureAuditObjective(f$data, f$inputs, f$inputs$betaBar + 1e5 * direction)
    expect_lt(tail$objective, -1e6)
  }
  f <- e$surrogateSolverControls()$indefinite_far
  r <- e$compareSurrogateSolvers(f$data, f$inputs, c("current_cd", "pda_cd"))
  expect_true(all(r$summary$status == "kernel_error"))
  expect_false(any(r$summary$auditKktPassed))
  expect_true(all(r$summary$outerSteps == 0))
})

test_that("sparse matrices and affine constants leave solver comparisons unchanged", {
  e <- loadSolverComparison()
  f <- e$surrogateSolverControls()$bounded_full
  solvers <- c("bfgs_numeric", "bfgs_analytic", "current_cd")
  dense <- e$compareSurrogateSolvers(f$data, f$inputs, solvers)
  f$data$xDesign <- Matrix::Matrix(f$data$xDesign, sparse = TRUE)
  sparse <- e$compareSurrogateSolvers(f$data, f$inputs, solvers)
  expect_equal(dense$coefficients, sparse$coefficients, tolerance = 1e-7)
  expect_equal(dense$summary$objective, sparse$summary$objective, tolerance = 1e-12)
  f$inputs$linearTermCentered <- FALSE
  uncentered <- e$compareSurrogateSolvers(f$data, f$inputs, "current_cd")
  expect_equal(uncentered$coefficients, subset(sparse$coefficients, solver == "current_cd"),
    ignore_attr = TRUE, tolerance = 1e-10)
  expect_equal(uncentered$summary$objective - sparse$summary$objective[3],
    sum(f$inputs$linearCorrection * f$inputs$betaBar), tolerance = 1e-12)
})

test_that("diagnostic budgets and solver errors are not recorded as convergence", {
  e <- loadSolverComparison()
  f <- e$surrogateSolverControls()$indefinite_far
  r <- e$compareSurrogateSolvers(f$data, f$inputs, "bfgs_analytic", betaLimit = 150)
  expect_equal(r$summary$status, "error_or_diagnostic_limit")
  expect_equal(r$summary$message, "diagnostic_beta_limit")
  expect_equal(r$summary$stateOrigin, "no_return")
  expect_false(r$summary$auditKktPassed)
  expect_true(all(is.na(r$coefficients$beta)))
  expect_true(nrow(r$evaluations) > 0)
  f <- e$surrogateSolverControls()$bounded_full
  r <- e$compareSurrogateSolvers(f$data, f$inputs, "current_cd", maxOuter = 1L)
  expect_equal(r$summary$status, "max_outer")
  expect_false(r$summary$auditKktPassed)
})

test_that("finite sentinels cannot make an overflowed objective pass the audit", {
  e <- loadSolverComparison()
  f <- e$surrogateSolverControls()$indefinite_far
  f$inputs$betaStart <- c(0, 1e200)
  r <- e$compareSurrogateSolvers(f$data, f$inputs, "bfgs_numeric",
    betaLimit = 1e250, optimMaxit = 5L)
  expect_equal(r$summary$optimCode, 0L)
  expect_true(all(r$evaluations$sentinel))
  expect_true(all(is.finite(r$evaluations$suppliedObjective)))
  expect_equal(r$summary$objective, -Inf)
  expect_false(r$summary$finite)
  expect_false(r$summary$auditKktPassed)
})

test_that("upstream provenance is enforced and nested helper bodies are unchanged", {
  e <- loadSolverComparison()
  cd <- solverComparisonPda(e)
  original <- as.list(body(get("ADAP.estimate", asNamespace("pda"))))[-1L]
  definition <- Filter(function(expr) is.call(expr) &&
    identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name("coordi")), original)[[1]]
  expect_identical(body(cd), definition[[3]][[3]])
  local_mocked_bindings(packageDescription = function(...) list(RemoteSha = "unverified"),
    .package = "utils")
  expect_error(e$.solverPdaCd(), "not the pinned")
})

test_that("invalid comparisons fail rather than silently change the objective", {
  e <- loadSolverComparison()
  f <- e$surrogateSolverControls()$bounded_lasso
  expect_error(e$compareSurrogateSolvers(f$data, f$inputs, "bfgs_analytic"), "lambda = 0")
  expect_error(e$compareSurrogateSolvers(f$data, f$inputs, "current_cd", maxOuter = 0), "controls")
  expect_error(e$compareSurrogateSolvers(f$data, f$inputs, "current_cd", betaLimit = .01), "Start")
  f$inputs$quadraticCorrection[1, 2] <- .9
  expect_error(e$compareSurrogateSolvers(f$data, f$inputs, "current_cd"), "Asymmetric")
  f$inputs$penalize[1] <- TRUE
  expect_error(e$compareSurrogateSolvers(f$data, f$inputs, "current_cd"), "invalid")
})
