# Run manifest: one row per runComparison() call in <outputDirectory>/run_manifest.csv,
# with the full settings and session in manifest/<manifestId>.rds.

.manifestPackages <- c("FederatedLearning", "FederatedLassoStudy", "PatientLevelPrediction",
  "Cyclops", "glmnet", "Matrix", "Rcpp", "RcppEigen", "mirai", "nanonext")

.manifestPackageVersions <- function(packages = .manifestPackages) {
  vapply(packages, function(package) {
    description <- suppressWarnings(utils::packageDescription(package))
    if (!is.list(description)) return(NA_character_)
    sha <- description$RemoteSha %||% description$GithubSHA1
    if (is.null(sha)) description$Version else paste0(description$Version, "@", substr(sha, 1, 12))
  }, character(1))
}

# MD5 of the serialized settings, so identical settings give identical hashes.
.settingsHash <- function(settings) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(settings, path, version = 3, compress = FALSE)
  unname(tools::md5sum(path))
}

.startRunManifest <- function(outputDirectory, settings) {
  dir.create(file.path(outputDirectory, "manifest"), recursive = TRUE, showWarnings = FALSE)
  startedAt <- Sys.time()
  manifestId <- paste0(format(startedAt, "%Y%m%dT%H%M%OS3", tz = "UTC"), "_", Sys.getpid())
  versions <- .manifestPackageVersions()
  row <- data.frame(
    manifestId = manifestId,
    startedAt = format(startedAt, tz = "UTC", usetz = TRUE),
    finishedAt = NA_character_,
    task = settings$taskSettings$name,
    holdouts = paste(settings$holdouts, collapse = ","),
    featureSets = paste(names(settings$featureSettings), collapse = ","),
    methods = paste(vapply(settings$methodSettings, `[[`, "", "method"), collapse = ","),
    settingsHash = .settingsHash(settings),
    framework = versions[["FederatedLearning"]],
    study = versions[["FederatedLassoStudy"]],
    packages = paste(names(versions)[!is.na(versions)], versions[!is.na(versions)], sep = "=", collapse = "; "),
    r = R.version$version.string,
    platform = R.version$platform,
    os = utils::sessionInfo()$running %||% NA_character_,
    blas = .blasLabel(),
    cores = as.character(parallel::detectCores()),
    stringsAsFactors = FALSE
  )
  path <- file.path(outputDirectory, "run_manifest.csv")
  previous <- .readRunManifest(path)
  .warnManifestChanges(previous, row)
  saveRDS(list(manifest = row, settings = settings, sessionInfo = utils::sessionInfo(),
    laLibrary = La_library(), extSoftVersion = extSoftVersion()),
    file.path(outputDirectory, "manifest", paste0(manifestId, ".rds")))
  utils::write.csv(dplyr::bind_rows(previous, row), path, row.names = FALSE)
  manifestId
}

.finishRunManifest <- function(outputDirectory, manifestId) {
  path <- file.path(outputDirectory, "run_manifest.csv")
  rows <- .readRunManifest(path)
  rows$finishedAt[rows$manifestId == manifestId] <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  utils::write.csv(rows, path, row.names = FALSE)
  invisible(NULL)
}

.readRunManifest <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, colClasses = "character", na.strings = "NA")
}

# Windows' reference BLAS reports no name; R ships it as Rblas.
.blasLabel <- function() {
  blas <- extSoftVersion()[["BLAS"]]
  if (nzchar(blas)) return(basename(blas))
  lapack <- La_library()
  if (.Platform$OS.type == "windows" && nzchar(lapack) && grepl("Rlapack", lapack, fixed = TRUE)) {
    return("Rblas (R reference)")
  }
  NA_character_
}

# A resumed run with other code or settings mixes results from both; say so.
.warnManifestChanges <- function(previous, row) {
  if (is.null(previous) || !nrow(previous)) return(invisible(NULL))
  first <- previous[1, , drop = FALSE]
  sameTask <- previous[previous$task == row$task, , drop = FALSE]
  changed <- c(
    framework = !identical(as.character(first$framework), row$framework),
    study = !identical(as.character(first$study), row$study),
    settings = nrow(sameTask) > 0L && !identical(as.character(sameTask$settingsHash[[1]]), row$settingsHash)
  )
  if (any(changed)) {
    warning(sprintf(
      "This result directory was started on %s with different %s; resumed results will mix both. See run_manifest.csv.",
      first$startedAt, paste(names(changed)[changed], collapse = " and ")), call. = FALSE)
  }
  invisible(NULL)
}
