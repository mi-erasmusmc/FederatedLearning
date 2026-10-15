# Run inside the approved data environment. Captures contain patient-level data.
# source("extras/auditCapturedSurrogates.R")
# report <- auditCapturedSurrogates("private/surrogate-captures/ADAP2-...")
# Inspect report, then export without repeating the calculations:
# writeCapturedSurrogateAudit(report, "reviewed-audit", disclosureApproved = TRUE)
# No fitting, worker communication, uploads, or changes to captured files occur.

.captureAuditTerms <- function(x, y, beta, derivatives = TRUE) {
  eta <- as.numeric(x %*% beta)
  out <- list(loss = mean(log1p(exp(-abs(eta))) + ifelse(y == 1, pmax(-eta, 0), pmax(eta, 0))), eta = eta)
  if (!derivatives) return(out)
  residual <- ifelse(y == 1, -stats::plogis(-eta), stats::plogis(eta))
  weight <- stats::plogis(eta) * stats::plogis(-eta)
  out$gradient <- as.numeric(Matrix::crossprod(x, residual)) / length(y)
  out$hessian <- as.matrix(Matrix::crossprod(x, Matrix::Diagonal(x = weight) %*% x)) / length(y)
  out
}

.captureAuditObjective <- function(data, inputs, beta, derivatives = TRUE) {
  terms <- .captureAuditTerms(data$xDesign, data$y, beta, derivatives)
  delta <- beta - inputs$betaBar
  correction <- inputs$quadraticCorrection
  if (is.null(correction)) correction <- matrix(0, length(beta), length(beta))
  if (identical(inputs$kind, "diag")) correction <- diag(correction, length(beta))
  symmetric <- (correction + t(correction)) / 2
  linear <- sum(inputs$linearCorrection * if (inputs$linearTermCentered) delta else beta)
  quadratic <- sum(delta * as.numeric(correction %*% delta)) / 2
  penalty <- inputs$lambda * sum(abs(beta[inputs$penalize]))
  out <- list(leadLoss = terms$loss, weightedLeadLoss = inputs$leadWeight * terms$loss,
    linear = linear, quadratic = quadratic, penalty = penalty,
    smooth = inputs$leadWeight * terms$loss + linear + quadratic,
    objective = inputs$leadWeight * terms$loss + linear + quadratic + penalty,
    eta = terms$eta)
  if (derivatives) {
    out$gradient <- inputs$leadWeight * terms$gradient + inputs$linearCorrection +
      as.numeric(symmetric %*% delta)
    out$hessian <- inputs$leadWeight * terms$hessian + symmetric
  }
  out
}

.captureAuditSpectrum <- function(h) {
  eig <- eigen((h + t(h)) / 2, symmetric = TRUE)
  tolerance <- 1e-10 * max(1, max(abs(eig$values)))
  v <- eig$vectors[, length(eig$values)]
  list(min = min(eig$values), max = max(eig$values),
    negative = sum(eig$values < -tolerance), tolerance = tolerance,
    asymmetry = max(abs(h - t(h))), direction = v,
    eigenpairResidual = max(abs(as.numeric(h %*% v) - min(eig$values) * v)))
}

.captureAuditState <- function(data, inputs, beta) {
  out <- data.frame(finite = FALSE, leadLoss = NA_real_, weightedLeadLoss = NA_real_,
    linear = NA_real_, quadratic = NA_real_, penalty = NA_real_, objective = NA_real_,
    gradientMaxAbs = NA_real_, kktMaxAbs = NA_real_, kktAtZeroThreshold1e8 = NA_real_,
    stationary1e6 = NA, stationary1e8 = NA, hessianMin = NA_real_, hessianMax = NA_real_,
    betaMaxAbs = NA_real_, etaMaxAbs = NA_real_, probabilityZeroFraction = NA_real_,
    probabilityOneFraction = NA_real_, newtonStepL2 = NA_real_, newtonDecrementSquared = NA_real_)
  if (any(!is.finite(beta))) return(out)
  z <- .captureAuditObjective(data, inputs, beta)
  for (name in c("leadLoss", "weightedLeadLoss", "linear", "quadratic", "penalty", "objective")) {
    out[[name]] <- z[[name]]
  }
  out$betaMaxAbs <- max(abs(beta))
  out$etaMaxAbs <- max(abs(z$eta))
  if (any(!is.finite(c(z$objective, z$gradient, z$hessian, z$eta)))) return(out)
  eig <- .captureAuditSpectrum(z$hessian)
  kkt <- function(threshold) {
    r <- abs(z$gradient)
    active <- inputs$penalize & abs(beta) > threshold
    inactive <- inputs$penalize & !active
    r[active] <- abs(z$gradient[active] + inputs$lambda * sign(beta[active]))
    r[inactive] <- pmax(r[inactive] - inputs$lambda, 0)
    max(r)
  }
  out$finite <- TRUE
  out$gradientMaxAbs <- max(abs(z$gradient))
  out$kktMaxAbs <- kkt(0)
  out$kktAtZeroThreshold1e8 <- kkt(1e-8)
  out$stationary1e6 <- out$kktMaxAbs <= 1e-6
  out$stationary1e8 <- out$kktMaxAbs <= 1e-8
  out$hessianMin <- eig$min
  out$hessianMax <- eig$max
  p <- stats::plogis(z$eta)
  out$probabilityZeroFraction <- mean(p == 0)
  out$probabilityOneFraction <- mean(p == 1)
  if (inputs$lambda == 0 && eig$min > eig$tolerance) {
    step <- solve(z$hessian, z$gradient)
    out$newtonStepL2 <- sqrt(sum(step^2))
    out$newtonDecrementSquared <- sum(z$gradient * step)
  }
  out
}

.captureAuditRead <- function(directory, name) {
  if (length(name) != 1L || is.na(name) || !grepl("^[A-Za-z0-9_.-]+\\.rds$", name)) {
    stop("Invalid capture filename")
  }
  path <- normalizePath(file.path(directory, name), winslash = "/", mustWork = TRUE)
  if (!identical(dirname(path), directory)) stop("Capture file resolves outside its directory")
  readRDS(path)
}

.captureAuditValidateData <- function(data, maxColumns) {
  x <- data$xDesign
  y <- data$y
  if (!(is.matrix(x) || inherits(x, "Matrix")) || nrow(x) != length(y) ||
      !length(y) || ncol(x) < 1L || ncol(x) > maxColumns || anyNA(y) ||
      any(!y %in% 0:1) || any(!is.finite(x)) || any(x[, 1] != 1)) {
    stop("Invalid capture matrix/labels/intercept, or maxColumns exceeded")
  }
}

.captureAuditValidateInputs <- function(inputs, p, allowMissingStart = FALSE) {
  vectorOk <- function(x) is.numeric(x) && is.null(dim(x)) && length(x) == p && all(is.finite(x))
  if (length(inputs$kind) != 1L || !inputs$kind %in% c("first", "second", "full", "diag") ||
      !identical(inputs$lossScale, "mean_negative_log_likelihood") ||
      !identical(inputs$penalize, c(FALSE, rep(TRUE, p - 1L))) ||
      !vectorOk(inputs$betaBar) || !vectorOk(inputs$globalGrad) ||
      !vectorOk(inputs$leadGrad) || !vectorOk(inputs$linearCorrection) ||
      !(allowMissingStart && is.null(inputs$betaStart) || vectorOk(inputs$betaStart)) ||
      length(inputs$lambda) != 1L || !is.finite(inputs$lambda) || inputs$lambda < 0 ||
      !identical(inputs$leadWeight, 1) ||
      !is.logical(inputs$linearTermCentered) || length(inputs$linearTermCentered) != 1L ||
      is.na(inputs$linearTermCentered)) stop("Unsupported or invalid captured surrogate inputs")
  if (inputs$kind == "first") {
    if (!is.null(inputs$quadraticCorrection)) stop("First-order capture has a quadratic correction")
  } else if (inputs$kind == "diag") {
    if (!all(vapply(inputs[c("quadraticCorrection", "globalHessDiag", "leadHessDiag")], vectorOk, logical(1)))) {
      stop("Invalid diagonal curvature dimensions")
    }
  } else {
    matrixOk <- function(x) is.matrix(x) && identical(dim(x), c(p, p)) && all(is.finite(x))
    if (!all(vapply(inputs[c("quadraticCorrection", "globalHess", "leadHess")], matrixOk, logical(1)))) {
      stop("Invalid full curvature dimensions")
    }
  }
}

.captureAuditAttempt <- function(data, snapshot, exit, round, lead, run, raySteps) {
  input <- snapshot$inputs
  p <- ncol(data$xDesign)
  checks <- list()
  compare <- function(name, actual, expected) {
    available <- !is.null(actual) && !is.null(expected)
    error <- if (available) max(abs(actual - expected)) else NA_real_
    scale <- if (available) max(1, abs(actual), abs(expected)) else NA_real_
    checks[[length(checks) + 1L]] <<- data.frame(check = name, available = available,
      maxAbsError = error, scaledError = error / scale,
      within1e10 = if (available) error / scale <= 1e-10 else NA)
  }
  exact <- .captureAuditTerms(data$xDesign, data$y, input$betaBar)
  atBar <- .captureAuditObjective(data, input, input$betaBar)
  compare("stored_lead_gradient_vs_exact", input$leadGrad, exact$gradient)
  compare("stored_linear_correction_identity", input$linearCorrection, input$globalGrad - input$leadGrad)
  compare("frozen_expansion_gradient_vs_global", atBar$gradient, input$globalGrad)
  if (input$kind %in% c("full", "second")) {
    compare("stored_lead_hessian_vs_exact", input$leadHess, exact$hessian)
    compare("stored_quadratic_correction_identity", input$quadraticCorrection, input$globalHess - input$leadHess)
    compare("frozen_expansion_hessian_vs_global", atBar$hessian, input$globalHess)
  } else if (input$kind == "diag") {
    compare("stored_lead_diagonal_vs_exact", input$leadHessDiag, diag(exact$hessian))
    compare("stored_diagonal_correction_identity", input$quadraticCorrection, input$globalHessDiag - input$leadHessDiag)
    compare("frozen_expansion_diagonal_vs_global", diag(atBar$hessian), input$globalHessDiag)
  }

  # Only the lead rows can be recomputed. Remote derivatives remain stored reports.
  globalMin <- if (input$kind %in% c("full", "second")) .captureAuditSpectrum(input$globalHess)$min else NA_real_
  siteMin <- NA_real_
  if (!is.null(round)) {
    reports <- round$reports
    ns <- vapply(reports, `[[`, numeric(1), "n")
    if (any(!is.finite(ns) | ns <= 0) || any(ns != floor(ns))) stop("Invalid captured site sample counts")
    for (report in reports) {
      if (length(report$grad) != p || any(!is.finite(report$grad))) stop("Invalid captured site gradient")
    }
    compare("report_expansion_point", input$betaBar, round$state$betaBar)
    n <- sum(ns)
    g <- as.numeric(do.call(cbind, lapply(reports, `[[`, "grad")) %*% (ns / n))
    h <- NULL
    if (input$kind != "first") {
      field <- if (input$kind == "diag") "HessDiag" else "Hess"
      hs <- lapply(reports, `[[`, field)
      validH <- function(h) if (input$kind == "diag") {
        is.numeric(h) && length(h) == p && all(is.finite(h))
      } else is.matrix(h) && identical(dim(h), c(p, p)) && all(is.finite(h))
      if (!all(vapply(hs, validH, logical(1)))) stop("Invalid captured site curvature")
      h <- Reduce(`+`, Map(function(h, weight) h * weight, hs, ns / n))
      if (input$kind != "diag") {
        siteMin <- min(vapply(hs, function(h) .captureAuditSpectrum(h)$min, numeric(1)))
      }
    }
    leadIndex <- round$state$leadIndex
    if (length(leadIndex) != 1L || is.na(leadIndex) || !leadIndex %in% seq_along(reports)) stop("Invalid lead index")
    compare("lead_sample_count", ns[leadIndex], nrow(lead$xDesign))
    leadTerms <- .captureAuditTerms(lead$xDesign, lead$y, input$betaBar)
    compare("lead_report_gradient_vs_exact", reports[[leadIndex]]$grad, leadTerms$gradient)
    if (!is.null(h)) compare("lead_report_curvature_vs_exact", hs[[leadIndex]],
      if (input$kind == "diag") diag(leadTerms$hessian) else leadTerms$hessian)
    val <- snapshot$context$validationRows
    adjustment <- run$config$lambdaCvGlobalAdjustment
    if (is.null(adjustment)) adjustment <- "leaveValOut"
    if (!adjustment %in% c("leaveValOut", "pda")) stop("Unknown captured CV adjustment")
    if (length(val) && adjustment == "leaveValOut") {
      v <- .captureAuditTerms(lead$xDesign[val, , drop = FALSE], lead$y[val], input$betaBar)
      if (length(val) >= n) stop("Validation count consumes the global training set")
      g <- (n * g - length(val) * v$gradient) / (n - length(val))
      if (!is.null(h)) h <- (n * h - length(val) *
        if (input$kind == "diag") diag(v$hessian) else v$hessian) / (n - length(val))
    }
    compare("global_gradient_vs_weighted_reports_and_cv_removal", input$globalGrad, g)
    if (!is.null(h)) compare("global_curvature_vs_weighted_reports_and_cv_removal",
      if (input$kind == "diag") input$globalHessDiag else input$globalHess, h)
  } else compare("global_reports_unavailable", NULL, NULL)

  correction <- input$quadraticCorrection
  if (is.null(correction)) correction <- matrix(0, p, p)
  if (input$kind == "diag") correction <- diag(correction, p)
  eig <- .captureAuditSpectrum(correction)
  states <- list(expansion = input$betaBar)
  if (!is.null(input$betaStart)) states$start <- input$betaStart
  returned <- NULL
  if (identical(exit$status, "returned")) {
    value <- exit$value
    returned <- if (is.numeric(value)) value else if (!is.null(value$beta)) value$beta else value$w
    if (!is.numeric(returned) || length(returned) != p) stop("Invalid returned coefficient dimensions")
    states$returned <- returned
  }
  stateTable <- do.call(rbind, lapply(names(states), function(name) {
    cbind(state = name, .captureAuditState(data, input, states[[name]]))
  }))
  start <- if (is.null(input$betaStart)) input$betaBar else input$betaStart
  direction <- rep(c(1, -1), length.out = p) / sqrt(p)
  center <- .captureAuditObjective(data, input, start)
  differences <- do.call(rbind, lapply(c(1e-3, 1e-4, 1e-5, 1e-6), function(step) {
    plus <- .captureAuditObjective(data, input, start + step * direction)
    minus <- .captureAuditObjective(data, input, start - step * direction)
    analytic <- sum(center$gradient * direction)
    numeric <- (plus$smooth - minus$smooth) / (2 * step)
    data.frame(state = if (is.null(input$betaStart)) "expansion" else "start",
      step = step, analyticDirectionalGradient = analytic,
      finiteDifferenceGradient = numeric, gradientError = abs(analytic - numeric),
      hessianVectorError = max(abs(as.numeric(center$hessian %*% direction) -
        (plus$gradient - minus$gradient) / (2 * step))))
  }))
  directions <- list()
  if (eig$negative > 0L) directions$negative_correction <- eig$direction
  if (input$kind == "first") directions$linear_correction <- -input$linearCorrection
  if (!is.null(returned) && all(is.finite(returned))) directions$returned_displacement <- returned - input$betaBar
  rays <- list()
  for (name in names(directions)) {
    v <- directions[[name]]
    if (!any(v != 0)) next
    v <- v / max(abs(v))
    v <- v / sqrt(sum(v^2))
    for (sign in c(-1, 1)) {
      u <- sign * v
      projected <- as.numeric(data$xDesign %*% u)
      recession <- mean(ifelse(data$y == 1, pmax(-projected, 0), pmax(projected, 0))) +
        sum(input$linearCorrection * u) + input$lambda * sum(abs(u[input$penalize]))
      quadraticRate <- sum(u * as.numeric(correction %*% u)) / 2
      for (step in raySteps) {
        z <- .captureAuditObjective(data, input, input$betaBar + step * u, derivatives = FALSE)
        rays[[length(rays) + 1L]] <- data.frame(direction = name, sign = sign, step = step,
          objective = z$objective, objectiveOverStepSquared = z$objective / step^2,
          quadraticRate = quadraticRate,
          firstOrderRecession = if (input$kind == "first") recession else NA_real_)
      }
    }
  }
  value <- if (is.list(exit$value)) exit$value else list()
  reason <- if (is.null(value$failureReason)) "" else value$failureReason
  summary <- data.frame(kind = input$kind, phase = snapshot$context$phase,
    innerFold = snapshot$context$innerFold, n = length(data$y), outcomes = sum(data$y),
    columns = p, lambda = input$lambda, exitStatus = exit$status, failureReason = reason,
    solverConverged = if (is.null(value$converged)) NA else value$converged,
    optimCode = if (is.null(value$convergence)) NA_integer_ else value$convergence,
    correctionMin = eig$min, correctionMax = eig$max, correctionNegative = eig$negative,
    curvatureTolerance = eig$tolerance, correctionAsymmetry = eig$asymmetry,
    eigenpairResidual = eig$eigenpairResidual,
    negativeDiagonal = sum(diag(correction) < -eig$tolerance),
    storedGlobalHessianMin = globalMin, storedSiteHessianMin = siteMin,
    exactLeadHessianMin = .captureAuditSpectrum(exact$hessian)$min,
    globalRowsRecomputed = FALSE)
  list(attempts = summary, states = stateTable, checks = do.call(rbind, checks),
    finiteDifferences = differences, rays = if (length(rays)) do.call(rbind, rays) else data.frame())
}

# Pass one run directory, not the parent containing multiple fits. The returned
# tables contain scalars only, but still require local disclosure review.
auditCapturedSurrogates <- function(captureDirectory, outputDirectory = NULL,
                                    disclosureApproved = FALSE, maxColumns = 500L,
                                    raySteps = c(1, 100, 10000, 1000000), attempts = NULL) {
  if (!is.null(outputDirectory) && !isTRUE(disclosureApproved)) {
    stop("Inspect the aggregate report first. Export requires disclosureApproved = TRUE")
  }
  if (!is.null(outputDirectory) && file.exists(outputDirectory)) stop("Output directory already exists")
  if (length(maxColumns) != 1L || !is.finite(maxColumns) || maxColumns < 1 ||
      !length(raySteps) || any(!is.finite(raySteps) | raySteps <= 0)) stop("Invalid audit limits")
  directory <- normalizePath(captureDirectory, winslash = "/", mustWork = TRUE)
  read <- function(name) .captureAuditRead(directory, name)
  run <- read("run.rds")
  if (!identical(run$schemaVersion, 1L) || !identical(run$origin, "new_diagnostic_fit")) {
    stop("Unsupported capture schema or origin")
  }
  rawLead <- read("lead.rds")
  if (!identical(rawLead$fingerprint, rlang::hash(rawLead$clientData))) stop("Lead fingerprint mismatch")
  if (!identical(rawLead$mapping, run$config$mapping)) stop("Lead mapping disagrees with run settings")
  x <- rawLead$clientData$xMatrix
  if (!isTRUE(rawLead$clientData$n == nrow(x))) stop("Lead sample count mismatch")
  if (!is.null(rawLead$clientData$rowId) && length(rawLead$clientData$rowId) != nrow(x)) {
    stop("Lead row identifiers have the wrong length")
  }
  if (!isTRUE(run$config$intercept)) x <- cbind(Matrix::Matrix(1, nrow(x), 1, sparse = TRUE), x)
  if (!is.null(rawLead$mapping) && nrow(rawLead$mapping) != ncol(x) - 1L) stop("Lead mapping dimension mismatch")
  lead <- list(xDesign = x, y = rawLead$clientData$yLabels)
  .captureAuditValidateData(lead, maxColumns)
  rounds <- lapply(list.files(directory, "^round-[0-9]+\\.rds$"), read)
  rounds <- Filter(function(x) identical(x$state$phase, 1L), rounds)
  if (length(rounds) > 1L) stop("Multiple derivative rounds in a one-shot capture")
  round <- if (length(rounds)) rounds[[1L]] else NULL
  files <- list.files(directory, "^input-.*\\.rds$")
  snapshots <- lapply(files, read)
  exits <- lapply(sub("^input-", "exit-", files), function(file) {
    if (file.exists(file.path(directory, file))) read(file) else list(status = "missing_exit")
  })
  if (file.exists(file.path(directory, "server-rejection.rds"))) {
    rejection <- read("server-rejection.rds")
    if (is.null(round) || !identical(rejection$status, "rejected_before_solver")) {
      stop("Server rejection lacks its derivative reports")
    }
    state <- rejection$state
    local <- round$reports[[state$leadIndex]]
    snapshots[[length(snapshots) + 1L]] <- list(context = list(phase = "server_rejection",
      innerFold = NA_integer_, rows = seq_along(lead$y), validationRows = integer()),
      inputs = list(kind = "second", betaBar = state$betaBar, betaStart = NULL,
        globalGrad = state$otherGrad, globalHess = state$otherHess,
        leadGrad = local$grad, leadHess = local$Hess,
        linearCorrection = state$otherGrad - local$grad,
        quadraticCorrection = rejection$diagnostics$correction,
        lambda = 0, leadWeight = 1, linearTermCentered = FALSE,
        penalize = c(FALSE, rep(TRUE, ncol(x) - 1L)), lossScale = "mean_negative_log_likelihood"))
    exits[[length(exits) + 1L]] <- list(status = "rejected_before_solver",
      value = list(failureReason = rejection$reason))
    files <- c(files, "server-rejection.rds")
  }
  if (!length(snapshots)) stop("No captured solver inputs or server rejection")
  if (is.null(attempts)) attempts <- seq_along(snapshots)
  if (!is.numeric(attempts) || !length(attempts) || anyNA(attempts) || anyDuplicated(attempts) ||
      any(!attempts %in% seq_along(snapshots))) stop("Invalid attempt selection")
  snapshots <- snapshots[attempts]
  exits <- exits[attempts]
  files <- files[attempts]
  reports <- vector("list", length(snapshots))
  for (i in seq_along(snapshots)) {
    snapshot <- snapshots[[i]]
    rejected <- identical(exits[[i]]$status, "rejected_before_solver")
    if (!rejected && !identical(snapshot$schemaVersion, 1L)) stop("Unsupported input schema")
    data <- if (rejected) lead else read(snapshot$dataFile)
    if (!rejected && !identical(snapshot$dataHash, rlang::hash(data))) stop("Solver data fingerprint mismatch")
    .captureAuditValidateData(data, maxColumns)
    .captureAuditValidateInputs(snapshot$inputs, ncol(data$xDesign), rejected)
    rows <- snapshot$context$rows
    val <- snapshot$context$validationRows
    validRows <- function(rows) is.numeric(rows) && !anyNA(rows) &&
      all(rows == floor(rows) & rows >= 1 & rows <= length(lead$y)) && !anyDuplicated(rows)
    if (!validRows(rows) || !validRows(val) || length(rows) != length(data$y) ||
        length(intersect(rows, val)) || ncol(data$xDesign) != ncol(lead$xDesign) ||
        any(data$xDesign != lead$xDesign[rows, , drop = FALSE]) || any(data$y != lead$y[rows])) {
      stop("Captured solver rows/columns do not match the lead data or overlap validation")
    }
    if (!exits[[i]]$status %in% c("returned", "unwound_without_return", "missing_exit", "rejected_before_solver")) {
      stop("Unknown capture exit status")
    }
    report <- .captureAuditAttempt(data, snapshot, exits[[i]], round, lead, run, raySteps)
    reports[[i]] <- lapply(report, function(table) {
      if (!nrow(table)) return(table)
      cbind(attempt = attempts[i], table)
    })
  }
  tables <- lapply(names(reports[[1L]]), function(name) {
    pieces <- lapply(reports, `[[`, name)
    do.call(rbind, pieces[vapply(pieces, nrow, integer(1)) > 0L])
  })
  names(tables) <- names(reports[[1L]])
  auditFunctions <- list(auditCapturedSurrogates, .captureAuditRead, .captureAuditTerms,
    .captureAuditObjective, .captureAuditSpectrum, .captureAuditState,
    .captureAuditValidateData, .captureAuditValidateInputs, .captureAuditAttempt,
    writeCapturedSurrogateAudit)
  tables$provenance <- data.frame(attempt = attempts,
    inputMd5 = unname(tools::md5sum(file.path(directory, files))),
    runMd5 = unname(tools::md5sum(file.path(directory, "run.rds"))),
    origin = run$origin, captureDllMd5 = run$dllMd5,
    auditCodeHash = rlang::hash(lapply(auditFunctions, function(fn) list(formals(fn), body(fn)))),
    auditRVersion = as.character(getRversion()),
    auditMatrixVersion = as.character(utils::packageVersion("Matrix")),
    remoteRowsAvailable = FALSE, historicalIdentityVerified = FALSE)
  if (!is.null(outputDirectory)) {
    writeCapturedSurrogateAudit(tables, outputDirectory, disclosureApproved)
  }
  tables
}

writeCapturedSurrogateAudit <- function(report, outputDirectory, disclosureApproved = FALSE) {
  if (!isTRUE(disclosureApproved)) stop("Export requires disclosureApproved = TRUE after local review")
  if (file.exists(outputDirectory)) stop("Output directory already exists")
  expected <- c("attempts", "states", "checks", "finiteDifferences", "rays", "provenance")
  scalarTable <- function(x) is.null(x) || is.data.frame(x) && all(vapply(x, is.atomic, logical(1)))
  if (!identical(names(report), expected) || !all(vapply(report, scalarTable, logical(1)))) {
    stop("Expected aggregate audit tables, not raw captures")
  }
  if (!dir.create(outputDirectory, recursive = TRUE, mode = "0700")) stop("Could not create output directory")
  for (name in names(report)) {
    if (!is.null(report[[name]])) utils::write.csv(report[[name]],
      file.path(outputDirectory, paste0(name, ".csv")), row.names = FALSE)
  }
  writeLines(c(
    "Read-only audit of new diagnostic captures. No model was fitted and no original file was changed.",
    "Frozen corrections are used as recorded. Exact lead derivatives are recomputed independently in R.",
    "Remote rows are unavailable: weighted-report checks are NOT independent global derivative recomputation.",
    "CV removal is checked at the frozen expansion point, not with refitted fold-clean initialization.",
    "First-order methods match the global gradient only. Diagonal methods match only the Hessian diagonal.",
    "States are expansion, actual start and returned vector when available, NOT complete optimizer trajectories.",
    "A returned vector can belong to a failed fit. Termination status is not an optimality certificate.",
    "KKT uses exact nonzeros. The separate 1e-8 zero-threshold column is a sensitivity check only.",
    "Finite differences check the smooth part along one fixed direction, over several step sizes.",
    "Negative correction curvature implies an unbounded unrestricted objective, not inevitable optimizer escape.",
    "Nonnegative sampled recession directions do not establish boundedness or existence of a minimizer.",
    "ODAL's uncentered affine term is retained. Compare objective differences when changing conventions.",
    "No nonfinite objective is replaced with a sentinel. Calibration and predictive AUC are outside this audit.",
    "Tables contain aggregates only. Disclosure approval is supplied by the caller, not inferred by this script.",
    "Keep raw captures, row identifiers, coefficient vectors, Hessians and patient-level predictions private."
  ), file.path(outputDirectory, "README.txt"))
  invisible(outputDirectory)
}
