.algRegistry <- list()
.registerAlgorithm <- function(name,
                               serverInit,
                               clientInit,
                               clientUpdate, 
                               serverRound) {
  .algRegistry[[name]] <<- list(
    serverInit = serverInit,
    clientInit = clientInit,
    clientUpdate = clientUpdate,
    serverRound = serverRound
  )
}

.getAlgorithm <- function(name) .algRegistry[[name]]
