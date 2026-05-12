#' C++ dual averaging helpers
#'
#' Low-level C++ implementations used by the registered `DualAvg` algorithm.
#' These functions are exported for testing and parity checks; most callers
#' should use [fitFederated()] with `algorithm = "DualAvg"`.
#'
#' @param config Algorithm configuration list.
#' @param clientData Local client matrix/list with `xMatrix`, `yLabels`, and
#'   optionally `n`.
#' @param serverBroadcast Server state broadcast to a client.
#' @param serverState Current server state.
#' @param clientReports List of client update reports.
#' @return A list containing updated algorithm state or client report fields.
#' @name dualAveragingCpp
#' @aliases serverInitDualAveragingCpp clientUpdateDualAveragingCpp
#'   serverRoundDualAveragingCpp
NULL
