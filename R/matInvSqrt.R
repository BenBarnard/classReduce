#' Matrix Square Root Inverse
#'
#' @param matrix
#'
#' @return
#' @export
#'
#' @examples
matInvSqrt <- function (matrix) {
  svdMat <- svd(matrix)
  U <- svdMat$u
  rootDInv <- sqrt(solve(diag(svdMat$d)))
  U %*% rootDInv %*% t(U)
}
