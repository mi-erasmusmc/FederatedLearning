#' Initialize a client (not strictly needed for Euclidean mirror)
#' @param w0 initial primal p-vector
#' @return named list with z = gradient h(w0)
#' z=w0)
#' @export
clientInit <- function(w0) {
  list(z = w0)
}
