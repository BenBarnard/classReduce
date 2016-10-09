#' Rank of Matrix
#'
#' Depricated use rankMatrix
#'
#' @param x a matrix
#'
#' @return The rank of the matrix x.
#' @export
#'
#' @examples rank(diag(rep(1, 5)))
rank <- function(x){
  qr(x)$rank
}
