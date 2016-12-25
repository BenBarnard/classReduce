#' Matrix Square Root Inverse
#'
#' @param matrix matrix
#'
#' @keywords internal
#' @export
#'
matInvSqrt <- function (matrix) {
  svdMat <- svd(matrix)
  U <- svdMat$u
  rootDInv <- sqrt(solve(diag(svdMat$d, nrow = length(svdMat$d))))
  U %*% rootDInv %*% t(U)
}
