# The Woodbury Matrix Inverse Identity
woodburyInv <- function(A_Inv, U, C_Inv, V) {
  A_Inv - A_Inv %*% U %*% solve(C_Inv + V %*% A_Inv %*% U) %*% V %*% A_Inv
}
