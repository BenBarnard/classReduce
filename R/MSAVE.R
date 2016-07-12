###### Sliced Average Variance Estimation #####################################
MSAVE <- function(data_summary_ls, targetDim){
  B <- data_summary_ls$S_B
  S <- data_summary_ls$S_W
  covs_ls <- data_summary_ls$S
  S_hatGamma <- (1 / length(covs_ls)) * Reduce(`+`,
                                               lapply(1:length(covs_ls), function(population){
                                                 (covs_ls[[population]] - S) %*%
                                                   solve(B + S) %*%
                                                   (covs_ls[[population]] - S)
                                               }))
  rootGammaInv <- matInvSqrt(B + S)
  M <- (rootGammaInv %*% B %*% rootGammaInv) %*%
    (rootGammaInv %*% B %*% rootGammaInv) +
    (rootGammaInv %*% S_hatGamma %*% rootGammaInv)


  svdM <- svd(M)
  rootGammaInv %*% (svdM$u[,1:targetDim])
}
