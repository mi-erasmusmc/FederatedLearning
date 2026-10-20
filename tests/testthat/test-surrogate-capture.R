captureFixture <- function(directory, method = "ADAP2") {
  config <- .prepareSurrogateCapture(list(surrogateCaptureDirectory = directory,
    mapping = data.frame(covariateId = 1002, columnId = 1L), intercept = TRUE), method)
  x <- Matrix::Matrix(cbind(1, rep(c(-1, 0, 1), each = 20)), sparse = TRUE)
  y <- rep(c(0, 1, 0, 1, 1, 1), 10)
  data <- list(xMatrix = x, yLabels = y, n = length(y), rowId = seq_along(y) + 1000L)
  list(config = config, x = x, y = y, data = data,
    capture = .captureSurrogateLead(config, data, list(betaBar = c(0, 0), totalN = length(y))))
}

captureInputs <- function(fixture) {
  lapply(list.files(fixture$config$surrogateCaptureDirectory, pattern = "^input-", full.names = TRUE), readRDS)
}

test_that("capture is opt-in, unique, non-overwriting, and does not consume the R RNG", {
  expect_identical(.prepareSurrogateCapture(list(p = 2L), "DualAvg"), list(p = 2L))
  expect_null(.beginSurrogateCapture(NULL, stop("must not force"), stop("must not force"), NULL, NULL))
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  set.seed(444)
  before <- .Random.seed
  a <- captureFixture(directory)
  b <- captureFixture(directory)
  expect_identical(.Random.seed, before)
  expect_false(identical(a$capture$directory, b$capture$directory))
  expect_error(.writeSurrogateCapture(NULL, file.path(a$capture$directory, "run.rds")), "overwrite")
  lead <- readRDS(file.path(a$capture$directory, "lead.rds"))
  expect_identical(lead$clientData, a$data)
  expect_identical(lead$fingerprint, rlang::hash(a$data))
  run <- readRDS(file.path(a$capture$directory, "run.rds"))
  expect_identical(run$origin, "new_diagnostic_fit")
  expect_match(run$dllMd5, "^[a-f0-9]{32}$")
  for (method in c("DualAvg", "ADAP_PDA", "C-ADAP")) {
    expect_error(.prepareSurrogateCapture(list(surrogateCaptureDirectory = directory), method), "supports")
  }
  expect_error(.prepareSurrogateCapture(list(surrogateCaptureDirectory = directory,
    adapDiagStyle = "pda"), "ADAPDiag"), "supports")
})

test_that("negative-curvature rejection preserves actual inputs and the failed return", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- captureFixture(directory)
  bar <- c(-0.2, 0.1)
  start <- c(0.3, -0.4)
  g <- .logisticNegGradient(bar, f$x, f$y)
  h <- .logisticNegHessian(bar, f$x)
  args <- list(xDesign = f$x, y = f$y, betaLead = c(0, 0), betaBar = bar,
    globalGrad = g, globalHess = h - diag(0.1, 2), lambda = 0.01,
    betaInit = start, returnDetails = TRUE)
  off <- do.call(.fitPdaAdapSurrogate, args)
  on <- do.call(.fitPdaAdapSurrogate, c(args, list(capture = f$capture)))
  expect_identical(on, off)
  expect_identical(on$failureReason, "negative_C_eigenvalue")
  saved <- captureInputs(f)[[1]]
  expect_identical(saved$inputs$betaStart, start)
  expect_identical(saved$inputs$betaBar, bar)
  expect_identical(saved$inputs$penalize, c(FALSE, TRUE))
  expect_identical(saved$inputs$lossScale, "mean_negative_log_likelihood")
  expect_equal(saved$inputs$quadraticCorrection, -diag(0.1, 2))
  expect_equal(saved$inputs$leadGrad, g)
  data <- readRDS(file.path(f$capture$directory, saved$dataFile))
  expect_identical(data, list(xDesign = f$x, y = f$y))
  expect_identical(saved$dataHash, rlang::hash(data))
  exit <- readRDS(list.files(f$capture$directory, "^exit-", full.names = TRUE))
  expect_identical(exit$status, "returned")
  expect_identical(exit$value, off)
  args$returnDetails <- FALSE
  expect_error(do.call(.fitPdaAdapSurrogate, c(args, list(capture = f$capture))), "negative_C")
  exits <- lapply(list.files(f$capture$directory, "^exit-", full.names = TRUE), readRDS)
  expect_true(any(vapply(exits, function(x) x$status == "unwound_without_return", logical(1))))
  expect_length(list.files(f$capture$directory, "^data-"), 1L)
})

test_that("full, first-order and diagonal fits are unchanged by private capture", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- captureFixture(directory)
  bar <- c(0, 0)
  g <- .logisticNegGradient(bar, f$x, f$y)
  h <- .logisticNegHessian(bar, f$x)
  common <- list(xDesign = f$x, y = f$y, betaLead = bar, betaBar = bar, globalGrad = g,
    lambda = 0.02, returnDetails = TRUE, maxOuter = 200L, tol = 1e-8)
  fits <- list(full = list(fn = .fitPdaAdapSurrogate, args = list(globalHess = h + diag(0.1, 2))),
    first = list(fn = .fitPdaAdapFirstOrderSurrogate, args = list()),
    diag = list(fn = .fitPdaAdapRemoteDiagSurrogate, args = list(globalHessDiag = diag(h) + 0.1)))
  for (fit in fits) {
    args <- c(common, fit$args)
    off <- do.call(fit$fn, args)
    on <- do.call(fit$fn, c(args, list(capture = f$capture)))
    expect_identical(on, off)
    expect_true(on$converged)
  }
  expect_setequal(vapply(captureInputs(f), function(x) x$inputs$kind, character(1)), c("full", "first", "diag"))
  expect_length(list.files(f$capture$directory, "^data-"), 1L)
})

test_that("CV captures validation exclusion, adjusted derivatives and actual warm starts", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  f <- captureFixture(directory, "ADAP1")
  bar <- c(0, 0)
  g <- .logisticNegGradient(bar, f$x, f$y)
  args <- list(xDesign = f$x, y = f$y, betaLead = bar, betaBar = bar, globalGrad = g,
    totalN = length(f$y), lambdaSeq = c(0.2, 0.1), foldsK = 3L,
    search = "grid", maxOuter = 200L, tol = 1e-7, collectDiagnostics = TRUE)
  off <- do.call(.pdaAdapFirstLeadCv, args)
  on <- do.call(.pdaAdapFirstLeadCv, c(args, list(capture = f$capture)))
  expect_identical(on, off)
  inputs <- captureInputs(f)
  expect_length(inputs, 6L)
  expect_length(list.files(f$capture$directory, "^data-"), 3L)
  for (input in inputs) {
    rows <- input$context$rows
    val <- input$context$validationRows
    expect_length(intersect(rows, val), 0L)
    expect_setequal(c(rows, val), seq_along(f$y))
    data <- readRDS(file.path(f$capture$directory, input$dataFile))
    expect_identical(data$xDesign, f$x[rows, , drop = FALSE])
    expect_identical(data$y, f$y[rows])
    expect_equal(input$inputs$globalGrad, .logisticNegGradient(bar, f$x[rows, , drop = FALSE], f$y[rows]))
    expect_equal(input$inputs$linearCorrection, c(0, 0), tolerance = 1e-14)
  }
  files <- list.files(f$capture$directory, "^input-", full.names = TRUE)
  for (fold in 1:3) {
    high <- which(vapply(inputs, function(x) x$context$innerFold == fold && x$inputs$lambda == 0.2, logical(1)))
    low <- which(vapply(inputs, function(x) x$context$innerFold == fold && x$inputs$lambda == 0.1, logical(1)))
    previous <- readRDS(file.path(dirname(files[high]), sub("^input-", "exit-", basename(files[high]))))
    expect_identical(inputs[[low]]$inputs$betaStart, .adapFitBeta(previous$value))
  }
})

test_that("ODAL capture leaves BFGS estimates and report fields unchanged", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(FederatedLearning.localId = 1L)
  on.exit(options(old), add = TRUE)
  for (variant in c("first", "second")) {
    f <- captureFixture(directory, "ODAL2")
    # The client writes lead.rds itself, so use a fresh run directory.
    cfg <- .prepareSurrogateCapture(list(surrogateCaptureDirectory = directory,
      mapping = f$config$mapping, intercept = TRUE), "ODAL2")
    bar <- c(0, 0)
    state <- list(phase = 2L, leadIndex = 1L, betaBar = bar, odalVariant = variant,
      otherGrad = .logisticNegGradient(bar, f$x, f$y),
      otherHess = if (variant == "second") .logisticNegHessian(bar, f$x) + diag(0.1, 2) else NULL)
    offCfg <- cfg
    offCfg$surrogateCaptureDirectory <- NULL
    off <- .clientUpdateODAL(f$data, state, offCfg)
    on <- .clientUpdateODAL(f$data, state, cfg)
    expect_identical(on, off)
    file <- list.files(cfg$surrogateCaptureDirectory, "^input-", full.names = TRUE)
    input <- readRDS(file)
    expect_false(input$inputs$linearTermCentered)
    expect_equal(input$inputs$lambda, 0)
    expect_identical(readRDS(sub("/input-", "/exit-", file))$value, on)
  }
})

test_that("ADAP client capture survives rejection of every CV candidate", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(FederatedLearning.localId = 1L)
  on.exit(options(old), add = TRUE)
  f <- captureFixture(directory)
  cfg <- .prepareSurrogateCapture(list(surrogateCaptureDirectory = directory,
    mapping = f$config$mapping, intercept = TRUE, foldsK = 3L, lambdaSearch = "grid",
    lambdaCvMaxRows = 30L, adapCurvatureAction = "fail"), "ADAP2")
  state <- list(phase = 2L, leadIndex = 1L, betaBar = c(0, 0), betaLead = c(0, 0),
    globalGrad = .logisticNegGradient(c(0, 0), f$x, f$y), globalHess = matrix(0, 2, 2),
    totalN = length(f$y), lambdaSeq = c(0.2, 0.1), adapMethodName = "ADAP2")
  expect_error(.clientUpdatePdaAdap(f$data, state, cfg), "lambda CV failed")
  inputs <- lapply(list.files(cfg$surrogateCaptureDirectory, "^input-", full.names = TRUE), readRDS)
  expect_length(inputs, 6L)
  for (input in inputs) {
    data <- readRDS(file.path(cfg$surrogateCaptureDirectory, input$dataFile))
    expect_identical(data$y, f$y[input$context$rows])
    expect_equal(length(input$context$rows), 20L)
    expect_equal(length(input$context$validationRows), 10L)
    expect_lt(min(eigen(input$inputs$quadraticCorrection, symmetric = TRUE)$values), 0)
  }
  exits <- lapply(list.files(cfg$surrogateCaptureDirectory, "^exit-", full.names = TRUE), readRDS)
  expect_true(all(vapply(exits, function(x) identical(x$value$failureReason, "negative_C_eigenvalue"), logical(1))))
})

test_that("row identifiers follow diagnostic downsampling only when capture is enabled", {
  x <- Matrix::Matrix(matrix(1:12, ncol = 1), sparse = TRUE)
  y <- c(rep(0L, 10), 1L, 1L)
  plp <- list(population = data.frame(rowId = 101:112, outcomeCount = y))
  local_mocked_bindings(toSparseM = function(...) list(dataMatrix = x), .package = "PatientLevelPrediction")
  cfg <- list(mapping = data.frame(covariateId = 1002, columnId = 1L), intercept = TRUE,
    diagnosticControlsPerCase = 1, diagnosticDownsampleSeed = 42L)
  off <- createClientMatrix(plp, cfg)
  on <- createClientMatrix(plp, c(cfg, list(surrogateCaptureDirectory = "private")))
  rows <- .diagnosticDownsampleRows(y, 1, 42L)
  expect_identical(on$rowId, plp$population$rowId[rows])
  expect_identical(on[names(off)], off)
  expect_null(off$rowId)
})

test_that("real workers preserve ODAL results and communication accounting with capture", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  cl <- clusterInit(rep("localhost", 2), rep("synthetic", 2), mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(cl, 1:2, function(i) {
    x <- Matrix::Matrix(cbind(1, rep(c(0, 1), each = 40)), sparse = TRUE)
    y <- c(rep(c(0, 0, 0, 1), 10), rep(c(0, 0, 1, 1), 10))
    keep <- if (i == 1) seq_along(y) else sort(c(seq(1, length(y), by = 4), seq(4, length(y), by = 4)))
    assign("clientData", list(xMatrix = x[keep, , drop = FALSE], yLabels = y[keep], n = length(keep)),
      envir = .GlobalEnv)
    NULL
  })
  local_mocked_bindings(clusterCreateMatrices = function(...) NULL, .package = "FederatedLearning")
  calls <- c(evaluate = 0L, export = 0L)
  originalEval <- parallel::clusterEvalQ
  originalExport <- parallel::clusterExport
  local_mocked_bindings(
    clusterEvalQ = function(cl, expr) {
      calls[["evaluate"]] <<- calls[["evaluate"]] + 1L
      call <- match.call()
      call[[1]] <- originalEval
      eval(call, parent.frame())
    },
    clusterExport = function(cl, varlist, envir = .GlobalEnv) {
      calls[["export"]] <<- calls[["export"]] + 1L
      originalExport(cl, varlist, envir)
    }, .package = "parallel")
  cfg <- list(mapping = data.frame(covariateId = 1002, columnId = 1L), p = 1L,
    intercept = TRUE, rounds = 3L, epsilon = 0, odalInit = "pda")
  off <- fitFederated(cl, "ODAL2", cfg, verbose = FALSE)
  offCalls <- calls
  calls[] <- 0L
  on <- fitFederated(cl, "ODAL2", c(cfg, list(surrogateCaptureDirectory = directory)), verbose = FALSE)
  expect_identical(on[setdiff(names(on), "config")], off[setdiff(names(off), "config")])
  expect_identical(calls, offCalls)
  expect_gt(calls[["evaluate"]], 0)
  root <- on$config$surrogateCaptureDirectory
  expect_length(list.files(root, "^round-"), 3L)
  round <- readRDS(file.path(root, "round-1.rds"))
  expect_equal(vapply(round$reports, `[[`, numeric(1), "n"), c(80, 40))
  lead <- readRDS(file.path(root, "lead.rds"))
  broadcast <- readRDS(file.path(root, "round-2.rds"))$state
  expect_identical(lead$broadcast$phase, 1L)
  expect_equal(lead$broadcast$betaBar, broadcast$betaBar)
  weights <- c(80, 40) / 120
  expect_equal(broadcast$otherGrad,
    as.numeric(do.call(cbind, lapply(round$reports, `[[`, "grad")) %*% weights))
  expect_equal(broadcast$otherHess,
    Reduce(`+`, Map(function(report, w) report$Hess * w, round$reports, weights)))
})

test_that("ODAL server rejection preserves lead data and the rejected objective", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  cl <- clusterInit(rep("localhost", 2), rep("synthetic", 2), mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(cl, 1:2, function(i) {
    n <- c(2000L, 1000L)[i]
    prevalence <- c(0.5, 0.1)[i]
    counts <- as.integer(n * c(1 - prevalence, prevalence))
    x <- Matrix::Matrix(cbind(1, rep(0:1, counts)), sparse = TRUE)
    y <- unlist(lapply(counts, function(size) rep(0:1, c(.98, .02) * size)))
    assign("clientData", list(xMatrix = x, yLabels = y, n = n, rowId = seq_len(n)),
      envir = .GlobalEnv)
    NULL
  })
  local_mocked_bindings(clusterCreateMatrices = function(...) NULL, .package = "FederatedLearning")
  cfg <- list(mapping = data.frame(covariateId = 1002, columnId = 1L), p = 1L,
    intercept = TRUE, rounds = 3L, epsilon = 0, odalCurvatureAction = "fail")
  expect_error(fitFederated(cl, "ODAL2", cfg, verbose = FALSE), "indefinite Hessian correction")
  expect_error(fitFederated(cl, "ODAL2", c(cfg, list(surrogateCaptureDirectory = directory)),
    verbose = FALSE), "indefinite Hessian correction")

  root <- list.dirs(directory, recursive = FALSE)
  expect_length(root, 1L)
  expect_true(file.exists(file.path(root, "lead.rds")))
  expect_true(file.exists(file.path(root, "server-rejection.rds")))
  lead <- readRDS(file.path(root, "lead.rds"))
  rejection <- readRDS(file.path(root, "server-rejection.rds"))
  round <- readRDS(file.path(root, "round-1.rds"))
  expect_identical(rejection$status, "rejected_before_solver")
  expect_identical(rejection$reason, "negative_C_eigenvalue")
  expect_identical(lead$broadcast$phase, 1L)
  expect_identical(lead$rowIdStatus, "captured")
  expect_identical(lead$clientData$rowId, seq_len(2000L))
  expect_equal(lead$clientData$n, 2000L)
  expect_equal(lead$broadcast$betaBar, rejection$state$betaBar)
  weights <- c(2, 1) / 3
  expect_equal(rejection$state$otherGrad,
    as.numeric(do.call(cbind, lapply(round$reports, `[[`, "grad")) %*% weights))
  expect_equal(rejection$state$otherHess,
    Reduce(`+`, Map(function(report, w) report$Hess * w, round$reports, weights)))
  terms <- .logisticNegGradientHessian(rejection$state$betaBar,
    lead$clientData$xMatrix, lead$clientData$yLabels)
  expect_equal(terms$gradient, round$reports[[1]]$grad)
  expect_equal(terms$hessian, round$reports[[1]]$Hess)
  expect_equal(rejection$diagnostics$correction, rejection$state$otherHess - terms$hessian)
  expect_lt(rejection$diagnostics$correctionEigenMin, 0)
  expect_length(list.files(root, "^round-"), 2L)
  # Rejection is recorded as a server decision, not an optimizer attempt.
  expect_length(list.files(root, "^input-"), 0L)
  expect_length(list.files(root, "^exit-"), 0L)
})

test_that("comparison runner keeps private captures outside results and rejects remote workers", {
  runner <- comparisonRunnerEnv()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  args <- list("surrogate-capture-directory" = file.path(directory, "private"))
  expect_error(runner$validateSurrogateCaptureArgs(args, directory, "ODAL2", "localhost"), "outside")
  expect_error(runner$validateSurrogateCaptureArgs(args, directory, "DualAvg", "localhost"), "only")
  expect_error(runner$validateSurrogateCaptureArgs(args, directory, "ODAL2", "remote.site"), "local")
  results <- file.path(directory, "results")
  dir.create(results)
  parsed <- runner$validateSurrogateCaptureArgs(args, results, c("ODAL2", "ADAP2"), "localhost")
  expect_identical(runner$methodConfig("ODAL2", "ageSex", parsed)$surrogateCaptureDirectory,
    normalizePath(args[[1]], winslash = "/"))
  expect_null(runner$methodConfig("ODAL2", "ageSex", list())$surrogateCaptureDirectory)
})

test_that("an exit write failure does not mask a solver exception", {
  path <- tempfile(fileext = ".rds")
  saveRDS("existing", path)
  on.exit(unlink(path), add = TRUE)
  fail <- function() {
    on.exit(.endSurrogateCapture(path, returnValue()), add = TRUE)
    stop("original solver error")
  }
  expect_message(expect_error(fail(), "original solver error"), "Capture exit was not saved")
  expect_identical(readRDS(path), "existing")
})

test_that("a rejection write failure preserves the ODAL rejection error", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "server-rejection.rds")
  saveRDS("existing", path)
  state <- list(phase = 1L, odalVariant = "second", leadIndex = 1L, betaBar = c(0, 0))
  reports <- list(list(n = 20, grad = c(0, 0), Hess = diag(.25, 2)),
    list(n = 10, grad = c(0, 0), Hess = diag(.05, 2)))
  cfg <- list(surrogateCaptureDirectory = directory, odalCurvatureAction = "fail")
  expect_message(expect_error(.serverRoundODAL(state, reports, cfg), "indefinite Hessian correction"),
    "Capture rejection was not saved")
  expect_identical(readRDS(path), "existing")
})
