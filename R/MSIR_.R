###### Sliced Inverse Regression ##############################################
MSIR_ <- function(data, ...){
  useMethod("MSIR")
}

MSIR_.data.frame <- function(data, ...){
  do.call(MSIR.matrix, dlply(data, .(Group), dataDftoMatrix))
}

MSIR_.matrix <- function(...){

}

MSIR_.default <- function(data_summary_ls, targetDim) {
  B <- data_summary_ls$S_B
  S <- data_summary_ls$S_W
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv

  svdM <- svd(M)
  rootGammaInv %*% (svdM$u[,1:targetDim])
}
