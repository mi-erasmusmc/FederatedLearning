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

test_that("mirai workers keep their own site state across calls", {
  skip_if_not_installed("PatientLevelPrediction")
  installed <- find.package("FederatedLearning")
  skip_if_not(file.exists(file.path(installed, "Meta", "package.rds")), "Needs an installed package")
  cl <- clusterInit(rep("localhost", 3), rep("synthetic", 3), mirai = TRUE)
  stopped <- FALSE
  on.exit(if (!stopped) parallel::stopCluster(cl), add = TRUE)
  parallel::clusterApply(cl, 1:3, function(i) {
    assign("site", i, envir = .GlobalEnv)
    NULL
  })
  # A node whose calls can reach another node's process would return its site.
  for (i in 1:20) {
    expect_identical(unlist(parallel::clusterEvalQ(cl, site)), 1:3)
  }
  expect_identical(unlist(parallel::clusterEvalQ(cl[2:3], site)), 2:3)
  attached <- parallel::clusterEvalQ(cl, search())
  expect_true(all(vapply(attached, function(x) "package:FederatedLearning" %in% x, logical(1))))
  parallel::stopCluster(cl)
  stopped <- TRUE
  expect_error(parallel::clusterEvalQ(cl, site))
})
