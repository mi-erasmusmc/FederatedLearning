.algRegistry <- list()

.laplaceVarianceToLambda <- function(variance, n) {
  if (!is.numeric(variance) || length(variance) != 1L ||
      !is.finite(variance) || variance <= 0) {
    stop("Laplace prior variance must be a positive finite scalar")
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n <= 0) {
    stop("Training row count must be a positive finite scalar")
  }
  # Cyclops uses summed NLL; DualAvg uses mean NLL + lambda * ||beta||_1.
  lambda <- sqrt(2) / sqrt(variance) / n
  if (!is.finite(lambda) || lambda <= 0) {
    stop("Laplace variance conversion produced an unrepresentable lambda")
  }
  lambda
}

.lambdaStrategyDefault <- function() {
  list(
    seed = NULL,
    scale = "priorVariance",
    initial = function(lambda, totalPopSize, context) {
      lambda
    },
    fit = function(lambda, totalPopSize, context) {
      .laplaceVarianceToLambda(lambda, totalPopSize)
    },
    final = function(lambda, totalPopSize, context) {
      .laplaceVarianceToLambda(lambda, totalPopSize)
    }
  )
}

.registerAlgorithm <- function(name,
                               serverInit,
                               clientInit,
                               clientUpdate, 
                               serverRound,
                               lambdaStrategy = NULL,
                               supportsClientSampling = FALSE) {
  if (is.null(lambdaStrategy)) {
    lambdaStrategy <- .lambdaStrategyDefault()
  }
  .algRegistry[[name]] <<- list(
    serverInit = serverInit,
    clientInit = clientInit,
    clientUpdate = clientUpdate,
    serverRound = serverRound,
    lambdaStrategy = lambdaStrategy,
    supportsClientSampling = supportsClientSampling
  )
}

.getAlgorithm <- function(name) .algRegistry[[name]]
