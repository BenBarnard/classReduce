#' Title
#'
#' @param S
#' @param n
#'
#' @export
#'
#' @keywords internal
#'
tu <- function(S, n, p) {
  u <- p * det(S) ^ (1 / p) / tr(S)
  min((4 * (p ^ 2 - 1) / ((n - p - 2) * p ^ 2)), 1) * u ^ (1 / p)
}
