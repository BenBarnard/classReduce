#' Matrix Square Root Inverse
#'
#' @param matrix matrix
#'
#' @keywords internal
#'
matInvSqrt <- function (matrix) {
  svdMat <- svd(matrix)
  U <- svdMat$u
  rootDInv <- sqrt(solve(diag(svdMat$d, nrow = length(svdMat$d))))
  U %*% rootDInv %*% t(U)
}

#' Trace of Matrix
#'
#' Calculates the trace of a square matrix.
#'
#' @param x a matrix
#'
#' @return The trace of a square matrix x.
#' 
#' @keywords internal
tr <- function(x){
  if(!(nrow(x) == ncol(x))){
    stop("Not a square matrix")
  }
  sum(diag(x))
}

#' Log of the SVD values of a matrix
#'
#' @param matrix
#' 
#' @keywords internal
#'
logd <- function(matrix){
  matSVD <- svd(matrix)
  u <- matSVD$u
  v <- matSVD$v
  u %*% diag(log(matSVD$d), nrow = length(matSVD$d)) %*% t(v)
}