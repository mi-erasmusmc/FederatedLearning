with_namespace_bindings <- function(replacements, code) {
  ns <- asNamespace("FederatedLearning")
  names <- names(replacements)
  old <- mget(names, envir = ns, inherits = FALSE)
  for (name in names) {
    unlockBinding(name, ns)
    assign(name, replacements[[name]], envir = ns)
    lockBinding(name, ns)
  }
  on.exit({
    for (name in names) {
      unlockBinding(name, ns)
      assign(name, old[[name]], envir = ns)
      lockBinding(name, ns)
    }
  }, add = TRUE)
  force(code)
}

test_that("unimodal lambda search proposes positive lambdas around a peak", {
  search <- unimodalSearchInit(stdStep = 2, firstCut = 10, stopByY = 1, init = 1)
  search$try(1, 10, 0)
  expect_equal(search$step()$nextX, 2)
  search$try(2, 9, 0)
  expect_equal(search$step()$nextX, 0.5)
  search$try(0.5, 9, 0)

  step <- search$step()
  expect_true(is.logical(step$continue))
  expect_true(is.finite(step$nextX))
  expect_true(step$nextX > 0)
  expect_equal(search$bestX(), 1)
  expect_equal(search$bestY(), 10)
})

test_that("inner-CV scoring uses finite AUC only", {
  ev <- data.frame(
    auc = c(0.6, 0.7),
    logLoss = c(0.2, 0.4)
  )

  expect_equal(FederatedLearning:::.innerCvScoreFromEvaluation(ev), 0.65)
  expect_true(is.na(FederatedLearning:::.innerCvScoreFromEvaluation(data.frame(auc = NA_real_))))
  expect_true(is.na(FederatedLearning:::.innerCvScoreFromEvaluation(data.frame(auc = c(0.7, NA_real_)))))
  expect_true(is.na(FederatedLearning:::.innerCvScoreFromEvaluation(data.frame(logLoss = 0.2))))
  expect_true(is.na(FederatedLearning:::.innerCvScoreMean(c(0.7, NA_real_))))
  expect_true(is.na(FederatedLearning:::.innerCvScoreSd(c(0.7, NA_real_))))
})

test_that("tuneLambda warm-starts DualAvg path fits with previous dual state", {
  calls <- new.env(parent = emptyenv())
  calls$configs <- list()

  fakeSubsetCluster <- function(cl, ids) {
    list(ids = ids)
  }
  fakeFitFederated <- function(cl, algorithm, config, verbose = TRUE) {
    calls$configs[[length(calls$configs) + 1L]] <- config
    list(
      w = c(config$lambda),
      z = c(config$lambda, length(calls$configs)),
      config = config,
      roundsCompleted = config$rounds
    )
  }
  fakeClusterCreateMatrices <- function(cl, config) {
    invisible(NULL)
  }
  fakeClusterEvaluateModel <- function(cl, w) {
    data.frame(auc = 10 - abs(log(w[[1]])))
  }

  strategy <- list(
    seed = function(context) 1,
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )

  res <- with_namespace_bindings(
    list(
      subsetCluster = fakeSubsetCluster,
      fitFederated = fakeFitFederated,
      clusterCreateMatrices = fakeClusterCreateMatrices,
      clusterEvaluateModel = fakeClusterEvaluateModel
    ),
    FederatedLearning:::tuneLambda(
      cl = list(1, 2),
      algorithm = "DualAvg",
      configBase = list(warmStartLambdaPath = TRUE),
      trainIds = 1:2,
      rounds = 3L,
      clientFrac = 1,
      epsilon = 1e-6,
      lambdaStrategy = strategy,
      lambdaDefault = 1,
      totalPopSize = 20,
      globalMap = data.frame(covariateId = 1:2, columnId = 1:2),
      stdStep = 2,
      stopByY = 1,
      firstCut = 10,
      verbose = FALSE
    )
  )

  expect_true(is.finite(res$bestLambda))
  expect_gte(length(calls$configs), 4L)
  warmStarted <- vapply(calls$configs, function(config) {
    !is.null(config$initialZ) && !is.null(config$roundOffset)
  }, logical(1))
  expect_false(any(warmStarted[seq_len(2)]))
  expect_true(any(warmStarted[-seq_len(2)]))
  expect_true(all(vapply(calls$configs[warmStarted], function(config) {
    length(config$initialZ) == 2L && config$roundOffset > 0
  }, logical(1))))
})

test_that("tuneLambda errors when validation AUC is undefined", {
  fakeSubsetCluster <- function(cl, ids) {
    list(ids = ids)
  }
  fakeFitFederated <- function(cl, algorithm, config, verbose = TRUE) {
    list(
      w = c(config$lambda),
      config = config,
      roundsCompleted = config$rounds
    )
  }
  fakeClusterCreateMatrices <- function(cl, config) {
    invisible(NULL)
  }
  fakeClusterEvaluateModel <- function(cl, w) {
    data.frame(auc = NA_real_, logLoss = abs(log(w[[1]])))
  }

  strategy <- list(
    seed = function(context) 1,
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )

  expect_error(
    with_namespace_bindings(
      list(
        subsetCluster = fakeSubsetCluster,
        fitFederated = fakeFitFederated,
        clusterCreateMatrices = fakeClusterCreateMatrices,
        clusterEvaluateModel = fakeClusterEvaluateModel
      ),
      FederatedLearning:::tuneLambda(
        cl = list(1, 2),
        algorithm = "DualAvg",
        configBase = list(warmStartLambdaPath = FALSE),
        trainIds = 1:2,
        rounds = 3L,
        clientFrac = 1,
        epsilon = 1e-6,
        lambdaStrategy = strategy,
        lambdaDefault = 1,
        totalPopSize = 20,
        globalMap = data.frame(covariateId = 1:2, columnId = 1:2),
        stdStep = 2,
        stopByY = 1,
        firstCut = 10,
        verbose = FALSE
      )
    ),
    "finite inner-CV AUC"
  )
})
