.algRegistry <- list()

.lambdaStrategyDefault <- function() {
  list(
    seed = NULL,
    initial = function(lambda, totalPopSize, context) {
      lambda / (3 * totalPopSize / 5)
    },
    final = function(lambda, totalPopSize, context) {
      lambda * 3 / 4
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
