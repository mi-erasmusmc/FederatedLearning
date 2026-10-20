test_that("comparison runner reproduces its golden outputs on the simulated fixture", {
  if (!identical(Sys.getenv("FEDERATEDLEARNING_GOLDEN"), "true")) {
    skip("Set FEDERATEDLEARNING_GOLDEN=true after installing the package under test")
  }
  runner <- test_path("..", "..", "extras", "runComparisonMatrix.R")
  if (!file.exists(runner)) skip("Extras scripts unavailable")
  skip_if_not_installed("Cyclops")
  fixture <- test_path("fixtures", "runner-golden")
  golden <- readRDS(file.path(fixture, "golden.rds"))
  work <- tempfile("golden-")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  actual <- goldenRunnerSnapshot(runGoldenRunner(runner, fixture, work))

  expect_identical(names(actual$results), names(golden$results))
  expect_identical(actual$results[c("task", "fold", "featureSet", "method", "error")],
    golden$results[c("task", "fold", "featureSet", "method", "error")])
  expect_identical(actual$results$nonzeroPredictors, golden$results$nonzeroPredictors)
  expect_identical(names(actual$artifacts), names(golden$artifacts))
  expect_identical(names(actual$lambdaPaths), names(golden$lambdaPaths))
  expect_identical(names(actual$preprocessing), names(golden$preprocessing))
  # Allows BLAS and compiler differences; exact on the platform that made the reference.
  for (part in names(golden)) {
    expect_equal(actual[[part]], golden[[part]], tolerance = 1e-7, label = part)
  }
})
