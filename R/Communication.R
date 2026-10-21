# Communication accounting for federated fits.
#
# An algorithm may register `protocol(serverState)`, returning the exchange
# label, the server-state fields sites need (`broadcast`), which sites receive
# them (`recipients`: "all" active sites or the "lead"), and the reply fields
# that belong to the protocol (`reply`). Diagnostics outside these fields are
# not counted. Without a protocol the whole state and reply are counted
# ("transport"). Values count numeric and logical elements; bytes are R
# serialization sizes. Site setup (settings, feature map) is not counted.

.communicationColumns <- c("stage", "exchange", "direction", "exchanges",
  "messages", "values", "bytes", "maxMessageValues")

.payloadValues <- function(x) {
  if (is.null(x)) return(0)
  if (is.list(x)) return(sum(vapply(x, .payloadValues, numeric(1))))
  if (is.numeric(x) || is.logical(x)) return(as.numeric(length(x)))
  0
}

.payloadBytes <- function(x) as.numeric(length(serialize(x, NULL)))

.hasPayload <- function(x) !is.null(x) && length(x) > 0L

.newCommunicationLog <- function() {
  log <- new.env(parent = emptyenv())
  log$rows <- list()
  log
}

.addMessages <- function(log, exchange, direction, payloads, trips) {
  payloads <- Filter(.hasPayload, payloads)
  key <- paste(exchange, direction, sep = "\r")
  row <- log$rows[[key]] %||% list(exchange = exchange, direction = direction,
    counts = c(exchanges = 0, messages = 0, values = 0, bytes = 0, maxMessageValues = 0))
  values <- vapply(payloads, .payloadValues, numeric(1))
  bytes <- vapply(payloads, .payloadBytes, numeric(1))
  row$counts <- row$counts + c(trips, length(payloads), sum(values), sum(bytes), 0)
  row$counts[["maxMessageValues"]] <- max(row$counts[["maxMessageValues"]], values)
  log$rows[[key]] <- row
  invisible(log)
}

# One round trip: `down` and `up` hold one payload per site (NULL = no message).
.recordExchange <- function(log, exchange, down, up) {
  .addMessages(log, exchange, "server to site", down, 1)
  .addMessages(log, exchange, "site to server", up, 1)
}

.communicationTable <- function(log, stage = "fit") {
  if (!length(log$rows)) return(.emptyCommunication())
  out <- do.call(rbind, lapply(unname(log$rows), function(row) {
    data.frame(stage = stage, exchange = row$exchange, direction = row$direction,
      as.list(row$counts), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

.emptyCommunication <- function() {
  data.frame(stage = character(), exchange = character(), direction = character(),
    exchanges = numeric(), messages = numeric(), values = numeric(), bytes = numeric(),
    maxMessageValues = numeric(), stringsAsFactors = FALSE)
}

# Sums tables by stage, exchange and direction, keeping first-seen order.
.combineCommunication <- function(tables) {
  tables <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, tables)
  if (!length(tables)) return(.emptyCommunication())
  all <- do.call(rbind, lapply(tables, `[`, .communicationColumns))
  keys <- paste(all$stage, all$exchange, all$direction, sep = "\r")
  out <- all[!duplicated(keys), , drop = FALSE]
  index <- match(keys, unique(keys))
  for (column in c("exchanges", "messages", "values", "bytes")) {
    out[[column]] <- as.numeric(tapply(all[[column]], index, sum))
  }
  out$maxMessageValues <- as.numeric(tapply(all$maxMessageValues, index, max))
  rownames(out) <- NULL
  out
}

.communicationTotals <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) {
    return(list(exchanges = 0, messages = 0, values = 0, bytes = 0))
  }
  down <- table[table$direction == "server to site", , drop = FALSE]
  list(exchanges = sum(down$exchanges), messages = sum(table$messages),
    values = sum(table$values), bytes = sum(table$bytes))
}

# Records a protocol round of `fitFederated()`; `report` is ordered as `activeClients`.
.recordProtocolRound <- function(log, algo, serverState, activeClients, report) {
  protocol <- if (is.function(algo$protocol)) algo$protocol(serverState) else NULL
  if (is.null(protocol)) {
    payload <- serverState
    .recordExchange(log, "round", rep(list(payload), length(activeClients)), report)
    return(invisible(log))
  }
  payload <- serverState[intersect(protocol$broadcast, names(serverState))]
  lead <- identical(protocol$recipients %||% "all", "lead")
  down <- lapply(activeClients, function(i) {
    if (lead && !isTRUE(i == serverState$leadIndex)) NULL else payload
  })
  up <- lapply(report, function(x) if (is.null(x)) NULL else x[intersect(protocol$reply, names(x))])
  .recordExchange(log, protocol$exchange, down, up)
}

.dualAvgProtocol <- function(serverState) {
  list(exchange = "dual averaging update", broadcast = c("z", "r"), reply = c("delta", "n"))
}

# ODAL and ADAP share three phases: local estimates, derivatives at the
# averaged estimate, and the lead site's surrogate fit, whose inputs differ.
.pdaProtocol <- function(serverState, leadFields) {
  phase <- serverState$phase %||% 0L
  if (phase == 0L) {
    return(list(exchange = "local estimates", broadcast = "phase",
      reply = c("bhat", "n", "lambdaLocal")))
  }
  if (phase == 1L) {
    firstOrder <- identical(serverState$odalVariant, "first") ||
      identical(serverState$adapReducedMode, "first")
    exchange <- if (firstOrder) {
      "gradients"
    } else if (identical(serverState$adapReducedMode, "diag")) {
      "gradients and Hessian diagonals"
    } else {
      "gradients and Hessians"
    }
    return(list(exchange = exchange, broadcast = c("phase", "betaBar", "odalVariant"),
      reply = c("grad", "Hess", "HessDiag", "n")))
  }
  if (phase == 2L) {
    return(list(exchange = "lead surrogate fit", recipients = "lead",
      broadcast = c("phase", "betaBar", leadFields), reply = "w"))
  }
  NULL
}

.odalProtocol <- function(serverState) {
  .pdaProtocol(serverState, c("odalVariant", "otherGrad", "otherHess"))
}

.adapProtocol <- function(serverState) {
  .pdaProtocol(serverState, c("betaLead", "globalGrad", "globalHess", "globalHessDiag",
    "lambdaSeq", "leadWeight", "leadWeightMin", "totalN", "adapSolveStyle",
    "adapSurrogateVariant", "adapMethodName", "adapReducedMode", "adapDiagStyle"))
}

# Sending a model to validation sites and receiving their summed log loss and size.
.validationCommunication <- function(w, sites = 1L) {
  log <- .newCommunicationLog()
  .recordExchange(log, "validation loss", rep(list(as.numeric(w)), sites),
    rep(list(list(logLoss = 0, n = 0)), sites))
  .communicationTable(log, "tuning")
}

.asTuningCommunication <- function(table) {
  if (is.null(table) || !nrow(table)) return(.emptyCommunication())
  table$stage <- "tuning"
  table
}
