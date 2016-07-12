##### SIR and SAVE Setup ######################################################
# A matrix inverse square root function
matInvSqrt <- function (matrix) {
  svdMat <- svd(matrix)
  U <- svdMat$u
  rootDInv <- diag(svdMat$d)  %>% solve  %>% sqrt
  U %*% rootDInv %*% t(U)
}
