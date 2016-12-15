#' Title
#'
#' @param matrix
#'
#' @return
#' @export
#'
#' @examples
logd <- function(matrix){
  matSVD <- svd(matrix)
  u <- matSVD$u
  v <- matSVD$v
  u %*% diag(log(matSVD$d), nrow = length(matSVD$d)) %*% t(v)
}
