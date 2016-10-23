#' Sum of Squares (helper function)
#'
#' @param matrix data matrix
#'
#' @keywords internal
#'
#' @export
#'
scatterMat <- function(matrix){
  row <- nrow(matrix)
  t(matrix) %*% (diag(rep(1, row)) - matrix(rep(1 / row, row ^ 2), nrow = row)) %*% matrix
}
