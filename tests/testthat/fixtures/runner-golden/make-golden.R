# Regenerates the runner golden fixture. Run from the package root against a fresh
# install of the code whose outputs should become the reference:
#   R CMD INSTALL . && Rscript tests/testthat/fixtures/runner-golden/make-golden.R
# Site data are created only when absent, so regenerating the golden outputs
# keeps the committed patients unchanged.
fixture <- file.path("tests", "testthat", "fixtures", "runner-golden")
source(file.path("tests", "testthat", "helper-runner-golden.R"))

for (k in 1:3) {
  site <- file.path(fixture, "data", "goldenTask", paste0("site", k))
  if (!dir.exists(site)) {
    data("simulationProfile", package = "PatientLevelPrediction", envir = environment())
    set.seed(100 + k)
    plpData <- PatientLevelPrediction::simulatePlpData(simulationProfile, n = c(400, 600, 800)[k])
    PatientLevelPrediction::savePlpData(plpData, site)
  }
}

runner <- normalizePath(file.path("extras", "runComparisonMatrix.R"))
work <- Sys.getenv("GOLDEN_WORK_DIRECTORY", tempfile("golden-"))
first <- goldenRunnerSnapshot(runGoldenRunner(runner, fixture, file.path(work, "a")))
second <- goldenRunnerSnapshot(runGoldenRunner(runner, fixture, file.path(work, "b")))
if (!identical(first, second)) {
  differing <- names(first)[!mapply(identical, first, second)]
  for (part in differing) print(all.equal(first[[part]], second[[part]]))
  stop("Two runner executions differ in: ", paste(differing, collapse = ", "),
    ". Outputs are kept in ", work)
}
saveRDS(first, file.path(fixture, "golden.rds"), version = 2)
message("Saved ", nrow(first$results), " result rows and ", length(first$artifacts), " artifacts")
