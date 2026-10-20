test_that("PSOCK loads study packages after applying library paths", {
  skip_if_not_installed("PatientLevelPrediction")
  installed <- find.package("FederatedLearning")
  skip_if_not(file.exists(file.path(installed, "Meta", "package.rds")), "Needs an installed package")
  library <- tempfile("worker-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)
  expect_true(file.copy(installed, library, recursive = TRUE))
  old <- .libPaths()
  on.exit(.libPaths(old), add = TRUE)
  .libPaths(c(library, old))
  cl <- clusterInit("localhost", "synthetic", mirai = FALSE)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  worker <- parallel::clusterCall(cl, function() list(
    package = find.package("FederatedLearning"), attached = search()))[[1L]]
  expect_identical(normalizePath(worker$package, winslash = "/"),
    normalizePath(file.path(library, "FederatedLearning"), winslash = "/"))
  expect_true(all(c("package:FederatedLearning", "package:PatientLevelPrediction") %in% worker$attached))
})
