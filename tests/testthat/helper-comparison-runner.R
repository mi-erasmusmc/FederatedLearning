# The comparison runner's functions, copied into a child of the package namespace.
# A test can replace one runner function for all runner callers, as it could when
# the runner was a sourced script. Package functions outside the runner stay in the
# namespace, where local_mocked_bindings(.package = "FederatedLearning") replaces them.
comparisonRunnerEnv <- function() {
  path <- testthat::test_path("..", "..", "R", "ComparisonRunner.R")
  if (!file.exists(path)) testthat::skip("Comparison runner source is unavailable")
  definitions <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]),
    as.list(parse(path, keep.source = FALSE)))
  namespace <- asNamespace("FederatedLearning")
  env <- new.env(parent = namespace)
  for (name in vapply(definitions, function(e) as.character(e[[2]]), "")) {
    value <- get(name, envir = namespace)
    if (is.function(value)) environment(value) <- env
    assign(name, value, envir = env)
  }
  # Package functions the script used to alias stay reachable as env$name, looked up
  # in the namespace at each use so namespace mocks still apply.
  for (name in c("matchingCombination", "successfulRows", "safeFilePart", "coefficientTable",
                 "exactNonzero", "saveModelArtifact", "readModelArtifact", "baselineFeatureSelection")) {
    local({
      target <- name
      makeActiveBinding(target, function() get(target, envir = namespace), env)
    })
  }
  env
}
