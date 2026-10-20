# Golden outputs of the comparison runner on the committed simulated fixture in
# fixtures/runner-golden. They pin results before the runner moves into R/.
# Workers load the installed package, so run against a fresh install:
#   R CMD INSTALL . && FEDERATEDLEARNING_GOLDEN=true Rscript -e 'testthat::test_local(filter = "runner-golden")'
# Regenerate deliberately with fixtures/runner-golden/make-golden.R.

goldenRunnerArgs <- function(resultDirectory) {
  c(
    "--data-root=data", "--tasks=goldenTask", "--goldenTask-risk-window-end=365",
    "--clients=3", "--client-ids=site1,site2,site3", "--folds=1,3",
    "--feature-sets=all,ageSex",
    paste0("--methods=PooledLasso,LocalAvgLasso,BiggestSiteLasso,LocalEnsembleLasso,",
      "LocalBestLasso,SparseLocalAvgLasso,DualAvg,ODAL1,ODAL2,ADAP2"),
    "--federated-preprocess=baseline", "--cyclops-cv=false", "--cyclops-variance=0.1",
    "--local-average-threshold=0.05", "--dualavg-tune-lambda=false", "--dualavg-lambda=0.01",
    "--dualavg-rounds=200", "--lambda-search=grid", "--lambda-grid-len=5", "--mirai=false",
    paste0("--result-directory=", resultDirectory)
  )
}

# Runs the runner in a copy of the fixture data and returns the result directory.
runGoldenRunner <- function(runner, fixture, workDirectory) {
  # Resolve paths before changing the working directory.
  runner <- normalizePath(runner, mustWork = TRUE)
  fixture <- normalizePath(fixture, mustWork = TRUE)
  dir.create(workDirectory, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(fixture, "data"), workDirectory, recursive = TRUE)
  libraries <- paste(.libPaths(), collapse = .Platform$path.sep)
  old <- setwd(workDirectory)
  on.exit(setwd(old), add = TRUE)
  log <- file.path(workDirectory, "runner.log")
  oldLibs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = libraries)
  on.exit(if (is.na(oldLibs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = oldLibs), add = TRUE)
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(runner), goldenRunnerArgs("out")),
    stdout = log, stderr = log)
  if (!identical(status, 0L)) stop("Runner failed; see ", log)
  file.path(workDirectory, "out")
}

# Removes timing fields at any depth, such as featureFiltering$elapsedSeconds.
dropGoldenTimings <- function(x) {
  if (is.data.frame(x)) return(x[, !grepl("lapsed", names(x)), drop = FALSE])
  if (!is.list(x) || is.null(names(x))) return(x)
  attributes <- attributes(x)
  keep <- !grepl("lapsed", names(x))
  out <- lapply(x[keep], dropGoldenTimings)
  attributes$names <- names(out)
  attributes(out) <- attributes
  out
}

# Deterministic content of a result directory: timings, temporary file names,
# session information and code provenance are excluded.
goldenRunnerSnapshot <- function(directory) {
  volatile <- function(names) grepl("lapsed|^modelFile$|^modelId$", names)
  stable <- function(rows) {
    rows <- rows[, !volatile(names(rows)), drop = FALSE]
    if ("lambdaPathFile" %in% names(rows)) {
      rows$lambdaPathFile <- ifelse(is.na(rows$lambdaPathFile), NA_character_, basename(rows$lambdaPathFile))
    }
    keys <- intersect(c("task", "fold", "featureSet", "method", "clientId", "client"), names(rows))
    rows <- rows[do.call(order, unname(as.list(rows[keys]))), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  readRows <- function(name) stable(utils::read.csv(file.path(directory, name), stringsAsFactors = FALSE))
  artifacts <- lapply(list.files(file.path(directory, "models"), "\\.rds$", full.names = TRUE), function(path) {
    artifact <- readRDS(path)
    artifact[c("modelId", "createdAt", "sessionInfo", "provenance")] <- NULL
    artifact$results <- stable(artifact$results)
    artifact
  })
  names(artifacts) <- vapply(artifacts, function(a) paste(a$task, a$fold, a$featureSet, a$method, sep = "/"), "")
  artifacts <- artifacts[order(names(artifacts))]
  lambdaFiles <- sort(list.files(directory, "^lambda_.*\\.csv$"))
  preprocessingFiles <- sort(list.files(file.path(directory, "preprocessing")))
  dropGoldenTimings(list(
    results = readRows("comparison_results.csv"),
    summary = readRows("summary_by_method.csv"),
    diagnostics = readRows("diagnostics.csv"),
    lambdaPaths = stats::setNames(lapply(lambdaFiles, function(f) utils::read.csv(file.path(directory, f))), lambdaFiles),
    preprocessing = stats::setNames(lapply(preprocessingFiles, function(f) {
      path <- file.path(directory, "preprocessing", f)
      if (grepl("\\.rds$", f)) readRDS(path) else utils::read.csv(path, stringsAsFactors = FALSE)
    }), preprocessingFiles),
    preprocessingSettings = readRDS(file.path(directory, "preprocessing_settings.rds")),
    artifacts = artifacts
  ))
}
