# Diagnostic experiments only. No production fitter or rejection policy is changed.
# source("extras/auditCapturedSurrogates.R")
# source("extras/compareSurrogateSolvers.R")
# results <- runSurrogateSolverControls()
# Inputs/returned coefficients may be sensitive when using non-synthetic data.

.solverPdaCd <- function() {
  ref <- "ee90de152f6886bcba1404cce9cde11cb31f3777"
  if (!requireNamespace("pda", quietly = TRUE)) stop("Install the pinned Penncil/pda reference")
  description <- utils::packageDescription("pda")
  if (!identical(description$RemoteSha, ref) ||
      !identical(tolower(description$RemoteUsername), "penncil") ||
      !identical(description$RemoteRepo, "pda")) stop("Installed PDA is not the pinned Penncil/pda reference")
  estimate <- get("ADAP.estimate", asNamespace("pda"))
  env <- new.env(parent = environment(estimate))
  for (name in c("soft", "coordi")) {
    definitions <- Filter(function(expr) is.call(expr) &&
      as.character(expr[[1]]) %in% c("<-", "=") && identical(expr[[2]], as.name(name)),
      as.list(body(estimate))[-1L])
    if (length(definitions) != 1L) stop("Cannot extract upstream helper: ", name)
    eval(definitions[[1]], env)
  }
  # Original definitions, including the fixed 100 sweeps / 1e-4 stopping rule.
  env$coordi
}

.solverKkt <- function(beta, gradient, lambda) {
  residual <- abs(gradient)
  active <- seq_along(beta) > 1L & beta != 0
  inactive <- seq_along(beta) > 1L & beta == 0
  residual[active] <- abs(gradient[active] + lambda * sign(beta[active]))
  residual[inactive] <- pmax(residual[inactive] - lambda, 0)
  max(residual)
}

compareSurrogateSolvers <- function(data, inputs,
    solvers = c("bfgs_numeric", "bfgs_analytic", "current_cd", "pda_cd"),
    maxOuter = 200L, maxInner = 1000L, innerTolerance = 1e-10,
    kktTolerance = 1e-7, betaLimit = 1e6, optimMaxit = 1000L,
    optimReltol = 1e-12, ndeps = 1e-3) {
  if (!exists(".captureAuditObjective", mode = "function")) stop("Source auditCapturedSurrogates.R first")
  .captureAuditValidateData(data, maxColumns = 500L)
  .captureAuditValidateInputs(inputs, ncol(data$xDesign))
  solvers <- match.arg(solvers, c("bfgs_numeric", "bfgs_analytic", "current_cd", "pda_cd"), several.ok = TRUE)
  if (inputs$lambda != 0 && any(grepl("bfgs", solvers))) stop("BFGS comparisons require lambda = 0")
  positive <- c(maxOuter, maxInner, innerTolerance, kktTolerance, betaLimit, optimMaxit, optimReltol, ndeps)
  if (length(positive) != 8L || any(!is.finite(positive) | positive <= 0) ||
      any(c(maxOuter, maxInner, optimMaxit) %% 1 != 0)) stop("Invalid solver controls")
  correction <- inputs$quadraticCorrection
  p <- length(inputs$betaBar)
  if (is.null(correction)) correction <- matrix(0, p, p)
  if (inputs$kind == "diag") correction <- diag(correction, p)
  if (max(abs(correction - t(correction))) > 1e-12) stop("Asymmetric correction is not supported")
  if (max(abs(inputs$betaStart)) > betaLimit) stop("Start exceeds diagnostic beta limit")
  pdaCd <- if ("pda_cd" %in% solvers) .solverPdaCd() else NULL
  objective <- function(beta, derivatives = TRUE) .captureAuditObjective(data, inputs, beta, derivatives)
  summaries <- steps <- evaluations <- coefficients <- list()

  for (solver in solvers) {
    beta <- inputs$betaStart
    trace <- calls <- list()
    status <- "max_outer"
    message <- ""
    optimCode <- NA_integer_
    outerSteps <- if (grepl("bfgs", solver)) NA_integer_ else 0L
    stateOrigin <- if (grepl("bfgs", solver)) "optim_return" else "last_completed_outer_iterate"
    fnCalls <- grCalls <- 0L
    beforeTime <- proc.time()[["elapsed"]]
    if (grepl("bfgs", solver)) {
      fn <- function(b) {
        z <- objective(b, FALSE)
        fnCalls <<- fnCalls + 1L
        sentinel <- !is.finite(z$objective)
        used <- if (sentinel) .Machine$double.xmax / 1e100 else z$objective
        calls[[length(calls) + 1L]] <<- data.frame(solver = solver, evaluation = fnCalls,
          rawObjective = z$objective, suppliedObjective = used, sentinel = sentinel,
          betaMaxAbs = max(abs(b)))
        # A diagnostic budget stops the experiment, never supplies a fake optimum.
        if (any(!is.finite(b)) || max(abs(b)) > betaLimit) stop("diagnostic_beta_limit")
        used
      }
      gr <- function(b) {
        grCalls <<- grCalls + 1L
        objective(b)$gradient
      }
      fit <- tryCatch(stats::optim(beta, fn,
        gr = if (solver == "bfgs_analytic") gr else NULL, method = "BFGS",
        control = list(maxit = optimMaxit, reltol = optimReltol, ndeps = rep(ndeps, p))),
        error = function(e) e)
      if (inherits(fit, "error")) {
        stateOrigin <- "no_return"
        beta <- rep(NA_real_, p)
        status <- "error_or_diagnostic_limit"
        message <- conditionMessage(fit)
      } else {
        beta <- fit$par
        optimCode <- fit$convergence
        status <- paste0("optim_code_", optimCode)
      }
    } else {
      # Both kernels see the same stable derivatives, fixed corrections, and full
      # quadratic step. KKT is a shared audit gate, not upstream's stopping rule.
      for (iteration in seq_len(maxOuter)) {
        z <- objective(beta)
        if (any(!is.finite(c(z$objective, z$gradient, z$hessian)))) {
          status <- "nonfinite_state"
          break
        }
        if (.solverKkt(beta, z$gradient, inputs$lambda) <= kktTolerance) {
          status <- "kkt_gate"
          break
        }
        B <- z$hessian
        a <- z$gradient - as.numeric(B %*% beta)
        q <- function(b) sum(a * b) + sum(b * as.numeric(B %*% b)) / 2 +
          inputs$lambda * sum(abs(b[-1]))
        kernelMessage <- ""
        candidate <- tryCatch({
          if (solver == "current_cd") {
            FederatedLearning:::quadraticLassoCdCpp(a, B, beta, inputs$lambda,
              maxIter = as.integer(maxInner), tol = innerTolerance,
              penalizeNullable = inputs$penalize)
          } else {
            invisible(utils::capture.output(fit <- pdaCd(a, B, beta, inputs$lambda)))
            kernelMessage <- if (is.na(fit$message)) "upstream_inner_limit" else fit$message
            as.numeric(fit$betainit)
          }
        }, error = function(e) e)
        if (inherits(candidate, "error")) {
          status <- "kernel_error"
          message <- conditionMessage(candidate)
          break
        }
        nextState <- .captureAuditState(data, inputs, candidate)
        trace[[length(trace) + 1L]] <- data.frame(solver = solver, iteration = iteration,
          objectiveBefore = z$objective, objectiveAfter = nextState$objective,
          objectiveChange = nextState$objective - z$objective,
          quadraticChange = q(candidate) - q(beta),
          quadraticKkt = .solverKkt(candidate, a + as.numeric(B %*% candidate), inputs$lambda),
          nonlinearKkt = nextState$kktMaxAbs,
          hessianBeforeMin = .captureAuditSpectrum(B)$min,
          hessianAfterMin = nextState$hessianMin, betaMaxAbs = max(abs(candidate)),
          stepMaxAbs = max(abs(candidate - beta)), kernelMessage = kernelMessage)
        beta <- candidate
        outerSteps <- iteration
        if (!nextState$finite) { status <- "nonfinite_state"; break }
        if (max(abs(beta)) > betaLimit) { status <- "diagnostic_beta_limit"; break }
        if (nextState$kktMaxAbs <= kktTolerance) { status <- "kkt_gate"; break }
      }
    }
    if (!grepl("bfgs", solver) && outerSteps == 0L) stateOrigin <- "initial_state"
    elapsed <- proc.time()[["elapsed"]] - beforeTime
    final <- .captureAuditState(data, inputs, beta)
    stepTable <- if (length(trace)) do.call(rbind, trace) else data.frame()
    summaries[[solver]] <- cbind(data.frame(solver = solver, stateOrigin = stateOrigin,
      status = status, message = message, optimCode = optimCode, outerSteps = outerSteps,
      functionCalls = fnCalls, gradientCalls = grCalls, seconds = elapsed,
      correctionMin = .captureAuditSpectrum(correction)$min,
      auditKktPassed = isTRUE(final$finite && final$kktMaxAbs <= kktTolerance),
      objectiveIncreases = if (nrow(stepTable)) sum(stepTable$objectiveChange > 1e-12, na.rm = TRUE) else NA_integer_), final)
    steps[[solver]] <- stepTable
    evaluations[[solver]] <- if (length(calls)) do.call(rbind, calls) else data.frame()
    coefficients[[solver]] <- data.frame(solver = solver, column = seq_len(p), beta = beta)
  }
  list(summary = do.call(rbind, summaries), steps = do.call(rbind, steps),
    evaluations = do.call(rbind, evaluations), coefficients = do.call(rbind, coefficients),
    settings = list(maxOuter = maxOuter, maxInner = maxInner, innerTolerance = innerTolerance,
      kktTolerance = kktTolerance, betaLimit = betaLimit, optimMaxit = optimMaxit,
      optimReltol = optimReltol, ndeps = ndeps, inputHash = rlang::hash(list(data, inputs)),
      pdaRef = if (!is.null(pdaCd)) utils::packageDescription("pda")$RemoteSha else NA_character_,
      pdaCdHash = if (!is.null(pdaCd)) rlang::hash(body(pdaCd)) else NA_character_))
}

surrogateSolverControls <- function() {
  if (!exists(".captureAuditTerms", mode = "function")) stop("Source auditCapturedSurrogates.R first")
  make <- function(data, bar, start, d, C, lambda = 0, kind = "full") {
    lead <- .captureAuditTerms(data$xDesign, data$y, bar)
    inputs <- list(kind = kind, betaBar = bar, betaStart = start,
      globalGrad = lead$gradient + d, leadGrad = lead$gradient, linearCorrection = d,
      quadraticCorrection = C, lambda = lambda, leadWeight = 1,
      linearTermCentered = TRUE, penalize = c(FALSE, rep(TRUE, length(bar) - 1L)),
      lossScale = "mean_negative_log_likelihood")
    if (kind == "diag") {
      inputs$leadHessDiag <- diag(lead$hessian)
      inputs$globalHessDiag <- diag(lead$hessian) + C
    } else if (kind != "first") {
      inputs$leadHess <- lead$hessian
      inputs$globalHess <- lead$hessian + C
    }
    list(data = data, inputs = inputs)
  }
  # Each covariate stratum has both labels, so the uncorrected controls have
  # finite optima. No random seeds or simulated outcome draws are required.
  grid <- expand.grid(age = c(.2, .6, 1), sex = 0:1, phenotype = 0:1)
  design <- cbind(1, as.matrix(grid))
  counts <- round(100 * stats::plogis(as.numeric(design %*% c(-2, .8, -.4, 1))))
  data <- list(xDesign = design[rep(seq_len(nrow(design)), each = 100), ],
    y = unlist(lapply(counts, function(n) rep(0:1, c(100 - n, n)))))
  bar <- c(-1, .3, -.1, .2)
  target <- c(-1.4, .8, 0, .5)
  C <- diag(c(.02, .04, .03, .02))
  C[1, 2] <- C[2, 1] <- .01
  # Construct a known stationary solution. Positive definite C makes it unique.
  known <- function(lambda, correction = C, kind = "full") {
    matrixC <- if (kind == "first") matrix(0, 4, 4) else
      if (kind == "diag") diag(correction) else correction
    d <- -.captureAuditTerms(data$xDesign, data$y, target)$gradient -
      as.numeric(matrixC %*% (target - bar)) - lambda * c(0, sign(target[-1]))
    f <- make(data, bar, bar + c(.2, -.1, .2, -.2), d, correction, lambda, kind)
    f$oracle <- target
    f
  }
  controls <- list(bounded_full = known(0), bounded_lasso = known(.02),
    bounded_diagonal_lasso = known(.02, diag(C), "diag"),
    bounded_first = known(0, NULL, "first"),
    bounded_first_lasso = known(.02, NULL, "first"))
  rare <- list(xDesign = matrix(1, 100, 1), y = rep(0:1, c(98, 2)))
  for (start in c("near", "poor")) {
    f <- make(rare, stats::qlogis(.02), if (start == "near") -3.8 else 8,
      0, NULL, kind = "first")
    f$oracle <- stats::qlogis(.02)
    controls[[paste0("bounded_rare_", start)]] <- f
  }
  site <- function(prevalence) {
    counts <- as.integer(1000 * c(1 - prevalence, prevalence))
    list(xDesign = cbind(1, rep(0:1, counts)),
      y = unlist(lapply(counts, function(n) rep(0:1, c(.98, .02) * n))))
  }
  lead <- site(.5)
  remote <- site(.1)
  bar <- c(stats::qlogis(.02), 0)
  l <- .captureAuditTerms(lead$xDesign, lead$y, bar)
  r <- .captureAuditTerms(remote$xDesign, remote$y, bar)
  C <- (r$hessian - l$hessian) / 2
  d <- (r$gradient - l$gradient) / 2
  direction <- .captureAuditSpectrum(C)$direction
  # Fix eigenvector orientation so the distant start is platform-independent.
  direction <- direction * sign(direction[which.max(abs(direction))])
  for (start in c("near", "far")) {
    for (lambda in c(0, .01)) {
      f <- make(lead, bar, if (start == "near") bar + c(.05, .03) else bar + 100 * direction,
        d, C, lambda)
      f$localStationaryPoint <- bar
      controls[[paste0("indefinite_", start, if (lambda > 0) "_lasso" else "")]] <- f
    }
  }
  controls
}

runSurrogateSolverControls <- function() {
  controls <- surrogateSolverControls()
  .solverPdaCd() # Fail before starting if the reference is not the verified pin.
  runs <- list()
  for (name in names(controls)) {
    f <- controls[[name]]
    for (representation in c("dense", "sparse")) {
      data <- f$data
      if (representation == "sparse") data$xDesign <- Matrix::Matrix(data$xDesign, sparse = TRUE)
      solvers <- c(if (f$inputs$lambda == 0) c("bfgs_numeric", "bfgs_analytic"), "current_cd", "pda_cd")
      run <- compareSurrogateSolvers(data, f$inputs, solvers)
      for (table in c("summary", "steps", "evaluations", "coefficients")) {
        if (nrow(run[[table]])) run[[table]] <- cbind(case = name, representation = representation, run[[table]])
      }
      oracle <- if (!is.null(f$oracle)) f$oracle else f$localStationaryPoint
      run$summary$referenceType <- if (!is.null(f$oracle)) "known_global_minimum" else
        if (!is.null(f$localStationaryPoint)) "known_local_stationary_point" else "none"
      run$summary$objectiveMinusReference <- if (is.null(oracle)) NA_real_ else
        run$summary$objective - .captureAuditObjective(data, f$inputs, oracle)$objective
      run$summary$betaMaxErrorFromReference <- vapply(run$summary$solver, function(solver) {
        if (is.null(oracle)) return(NA_real_)
        max(abs(run$coefficients$beta[run$coefficients$solver == solver] - oracle))
      }, numeric(1))
      runs[[paste(name, representation, sep = "_")]] <- run
    }
  }
  out <- lapply(c("summary", "steps", "evaluations", "coefficients"), function(table) {
    do.call(rbind, lapply(runs, `[[`, table))
  })
  names(out) <- c("summary", "steps", "evaluations", "coefficients")
  out$settings <- lapply(runs, `[[`, "settings")
  out$sessionInfo <- utils::sessionInfo()
  out
}
