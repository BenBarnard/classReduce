#' The Woodbury Matrix Inverse Identity
#'
#' @param x a matrix
#'
#' @return
#' @export
#'
#' @examples
#'
woodburyInv <- function(x){
  useMethod("woodburyInv")
}



woodburyInv.matrix <- function(A_Inv, U, C_Inv, V){
  A_Inv - A_Inv %*% U %*% solve(C_Inv + V %*% A_Inv %*% U) %*% V %*% A_Inv
}
