# Centring and flipping of the training design, for solvers that converge slowly when a
# predictor is nearly collinear with the intercept (for example age per 100 years in an
# older population, or a binary predictor present in almost every entry).
#
# Non-binary columns are centred at their pooled training mean, and binary columns present
# in more than half of the pooled training entries are replaced by their absence. With an
# unpenalized intercept this changes neither the L1-penalized objective nor its solution:
# centring shifts the intercept, and flipping changes the sign of the coefficient, which
# leaves its absolute value unchanged. The pooled means come from each site's row count and
# column sums, which the shared feature filter already exchanges.

# On a worker: row count, column sums and whether each column holds only 0 and 1.
.workerDesignSummary <- function(intercept) {
  x <- .asDgCMatrix(clientData$xMatrix)
  columns <- seq_len(ncol(x))
  if (isTRUE(intercept)) columns <- columns[-1L]
  binary <- vapply(columns, function(j) {
    entries <- if (x@p[j] < x@p[j + 1L]) (x@p[j] + 1L):x@p[j + 1L] else integer()
    all(x@x[entries] == 1)
  }, logical(1))
  list(n = nrow(x), sums = as.numeric(Matrix::colSums(x)[columns]), binary = binary)
}

# On a worker: keep the original design and replace it by the centred and flipped one.
.workerReparametrizeDesign <- function(center, centerValues, flip) {
  assign(".reparametrizedOriginalX", clientData$xMatrix, envir = .GlobalEnv)
  x <- .asDgCMatrix(clientData$xMatrix)
  for (k in seq_along(center)) x[, center[[k]]] <- x[, center[[k]]] - centerValues[[k]]
  for (j in flip) x[, j] <- 1 - x[, j]
  clientData$xMatrix <- Matrix::drop0(x)
  assign("clientData", clientData, envir = .GlobalEnv)
  invisible(NULL)
}

# On a worker: put the original design back.
.workerRestoreDesign <- function() {
  if (!exists(".reparametrizedOriginalX", envir = .GlobalEnv, inherits = FALSE)) return(invisible(FALSE))
  clientData$xMatrix <- get(".reparametrizedOriginalX", envir = .GlobalEnv)
  assign("clientData", clientData, envir = .GlobalEnv)
  rm(".reparametrizedOriginalX", envir = .GlobalEnv)
  invisible(TRUE)
}

# Pools the site summaries and reparametrizes every worker. Returns the transformation and
# the exchanged payloads, for the communication log.
.reparametrizeCluster <- function(cl, intercept) {
  if (!isTRUE(intercept)) {
    stop("centerPredictors requires an unpenalized intercept", call. = FALSE)
  }
  summaries <- parallel::clusterCall(cl, .workerDesignSummary, intercept)
  n <- sum(vapply(summaries, `[[`, numeric(1), "n"))
  sums <- Reduce(`+`, lapply(summaries, `[[`, "sums"))
  binary <- Reduce(`&`, lapply(summaries, `[[`, "binary"))
  means <- sums / n
  columns <- seq_along(sums) + 1L
  transform <- list(center = columns[!binary], centerValues = means[!binary],
    flip = columns[binary & means > 0.5])
  parallel::clusterCall(cl, .workerReparametrizeDesign, transform$center, transform$centerValues, transform$flip)
  list(transform = transform, summaries = summaries)
}

# Coefficients of the reparametrized design back on the original columns. The intercept is
# the first element.
.restoreReparametrizedCoefficients <- function(w, transform) {
  w <- as.numeric(w)
  if (length(transform$center)) {
    w[1L] <- w[1L] - sum(w[transform$center] * transform$centerValues)
  }
  if (length(transform$flip)) {
    w[1L] <- w[1L] + sum(w[transform$flip])
    w[transform$flip] <- -w[transform$flip]
  }
  w
}
