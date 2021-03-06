#' Covariance function Wrappper
#'
#' @param x data
#' @param ... parameters passed to cov
#' @keywords internal
#' @importFrom stats cov
#'
cov <- function(x, ...){
  ls <- list(...)
  ls$x <- x
  do.call(stats::cov, list(x))
}