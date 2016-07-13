###### Sliced Inverse Regression ##############################################
MSIR <- function(data, ...){
  useMethod("MSIR")
}

MSIR.data.frame <- function(data, ...){
  do.call(MSIR.matrix,
          data %>%
            dlply(.(Group), dataDftoMatrix))
}

MSIR.matrix <- function(...){

}

MSIR.default <- function(data_summary_ls, targetDim) {
  B <- data_summary_ls$S_B
  S <- data_summary_ls$S_W
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv

  svdM <- svd(M)
  rootGammaInv %*% (svdM$u[,1:targetDim])
}
