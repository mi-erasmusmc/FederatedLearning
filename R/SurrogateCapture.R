.surrogateCaptureMethods <- c("ODAL", "ODAL1", "ODAL2", "ADAP", "ADAP2", "ADAP1", "ADAPDiag")

.writeSurrogateCapture <- function(object, path) {
  if (file.exists(path)) stop("Refusing to overwrite surrogate capture: ", path)
  temporary <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = 2)
  if (!file.rename(temporary, path)) stop("Could not write surrogate capture: ", path)
}

.prepareSurrogateCapture <- function(config, algorithm) {
  root <- config$surrogateCaptureDirectory
  if (is.null(root)) return(config)
  if (!is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    stop("surrogateCaptureDirectory must be a nonempty directory path")
  }
  if (!algorithm %in% .surrogateCaptureMethods || identical(config$adapSolveStyle, "pda") ||
      identical(config$adapDiagStyle, "pda") ||
      !identical(config$adapSurrogateVariant %||% "exact", "exact")) {
    stop("Surrogate capture supports ODAL1/2 and exact ADAP1/2/Diag, not PDA-style or stabilized solvers")
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  directory <- tempfile(paste0(algorithm, "-"), tmpdir = root)
  if (!dir.create(directory, mode = "0700")) stop("Could not create private surrogate capture directory")
  config$surrogateCaptureDirectory <- directory
  config$surrogateCaptureMethod <- algorithm
  dll <- getLoadedDLLs()[["FederatedLearning"]][["path"]]
  .writeSurrogateCapture(list(schemaVersion = 1L, origin = "new_diagnostic_fit",
    createdAt = format(Sys.time(), tz = "UTC", usetz = TRUE), workingDirectory = getwd(), config = config,
    sessionInfo = utils::sessionInfo(), dllMd5 = unname(tools::md5sum(dll)),
    rCodeMd5 = tools::md5sum(list.files(system.file("R", package = "FederatedLearning"), full.names = TRUE))),
    file.path(directory, "run.rds"))
  writeLines(c(
    "PRIVATE DIAGNOSTIC CAPTURE: contains patient-level matrices, labels and row identifiers.",
    "Keep this directory inside the approved data environment. Do not upload it or include it in a results bundle.",
    "These are inputs from a new diagnostic fit, not recovered inputs of a historical model.",
    "run.rds records settings and the current installation. round-*.rds contain existing client reports.",
    "lead.rds contains the lead data and broadcast when captured. ODAL captures this before derivative aggregation.",
    "data-*.rds contain the exact matrices/labels passed to solvers.",
    "server-rejection.rds records a rejected ODAL objective before any solver attempt.",
    "input-*.rds record each attempt before rejection/optimization. Matching exit-*.rds record returned values.",
    "An unwound exit means no normal return, not convergence. Missing exit files can indicate interrupted execution.",
    "Stored corrections are the actual inputs. Recomputed derivatives must be saved separately, never substituted.",
    "Hashes identify serialized content within this capture. They do not establish historical data identity."
  ), file.path(directory, "PRIVATE-README.txt"))
  message("PRIVATE surrogate capture (do not share): ", directory)
  config
}

.captureSurrogateRound <- function(config, state, reports, round) {
  if (is.null(config$surrogateCaptureDirectory)) return(invisible(NULL))
  .writeSurrogateCapture(list(state = state, reports = reports),
    file.path(config$surrogateCaptureDirectory, paste0("round-", round, ".rds")))
}

.captureSurrogateRejection <- function(config, state, diagnostics, reason) {
  if (is.null(config$surrogateCaptureDirectory)) return(invisible(NULL))
  tryCatch(.writeSurrogateCapture(list(status = "rejected_before_solver", reason = reason,
    state = state, diagnostics = diagnostics),
    file.path(config$surrogateCaptureDirectory, "server-rejection.rds")),
    error = function(e) message("Capture rejection was not saved: ", conditionMessage(e)))
  invisible(NULL)
}

.surrogateCaptureContext <- function(config, n) {
  if (is.null(config$surrogateCaptureDirectory)) return(NULL)
  list(directory = config$surrogateCaptureDirectory, phase = "final",
    innerFold = NA_integer_, rows = seq_len(n), validationRows = integer())
}

.captureSurrogateLead <- function(config, data, broadcast) {
  if (is.null(config$surrogateCaptureDirectory)) return(NULL)
  if (!isTRUE(data$n == length(data$yLabels)) || nrow(data$xMatrix) != data$n) {
    stop("Surrogate capture sample count does not match the lead matrix")
  }
  if (!is.null(data$rowId) && length(data$rowId) != data$n) {
    stop("Surrogate capture row identifiers do not match the lead matrix")
  }
  .writeSurrogateCapture(list(clientData = data, broadcast = broadcast,
    mapping = config$mapping, fingerprint = rlang::hash(data),
    rowIdStatus = if (is.null(data$rowId)) "not_available" else "captured"),
    file.path(config$surrogateCaptureDirectory, "lead.rds"))
  .surrogateCaptureContext(config, data$n)
}

.surrogateCaptureSubset <- function(capture, rows, validationRows, fold) {
  if (is.null(capture)) return(NULL)
  capture$phase <- "cv"
  capture$innerFold <- fold
  capture$rows <- rows
  capture$validationRows <- validationRows
  capture
}

.beginSurrogateCapture <- function(capture, xDesign, y, inputs, settings) {
  if (is.null(capture)) return(NULL)
  inputs$lossScale <- "mean_negative_log_likelihood"
  inputs$penalize <- c(FALSE, rep(TRUE, ncol(xDesign) - 1L))
  data <- list(xDesign = xDesign, y = y)
  hash <- rlang::hash(data)
  dataFile <- paste0("data-", hash, ".rds")
  path <- file.path(capture$directory, dataFile)
  if (!file.exists(path)) .writeSurrogateCapture(data, path)
  inputPath <- tempfile("input-", tmpdir = capture$directory, fileext = ".rds")
  .writeSurrogateCapture(list(schemaVersion = 1L, context = capture, dataFile = dataFile,
    dataHash = hash, inputs = inputs, settings = settings), inputPath)
  file.path(capture$directory, sub("^input-", "exit-", basename(inputPath)))
}

.endSurrogateCapture <- function(path, value) {
  if (is.null(path)) return(invisible(NULL))
  # Do not replace an original solver exception with an exit-file write error.
  tryCatch(.writeSurrogateCapture(list(
    status = if (is.null(value)) "unwound_without_return" else "returned",
    value = value), path), error = function(e) message("Capture exit was not saved: ", conditionMessage(e)))
  invisible(NULL)
}
