#' Initialize a client (not strictly needed for Euclidean mirror)
#' @param w0 initial primal (p‐vector)
#' @return named list with z = ∇h(w0) (for Euclid h=½∥w∥² this is
#' z=w0)
#' @export
clientInit <- function(w0) {
  list(z = w0)
}
