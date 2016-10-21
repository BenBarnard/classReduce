##### New Young Method #########################################################
newSY <- function(data, ...){
  useMethod("newSY")
}

newSY.data.frame <- function(data, ...){
  do.call(newSY.matrix,
          data %>%
            dlply(.(Group), dataDftoMatrix))
}

newSY.matrix <- function(...){

}

newSY <- function(data_summary_ls, targetDim){
  means_ls <- data_summary_ls$xBars
  covs_ls <- data_summary_ls$Sigma
  invCovs_ls <- data_summary_ls$SigmaInv
  # We first find the projected mean differences using the Friedman RDA Pooled
  # Covariances. This will have the form SigmaInv_i * xbar_i - SigmaInv_1 *
  # xbar_1, for each population i in 2:m.
  projectedMeanDiffs <- sapply(2:length(means_ls), function(population){
    invCovs_ls[[population]] %*% means_ls[[population]] -
      invCovs_ls[[1]] %*% means_ls[[1]]
  })

  covsDiffs <- do.call(cbind, lapply(2:length(covs_ls), function(population){
    covs_ls[[population]] - covs_ls[[1]]
  }))
  M <- cbind(projectedMeanDiffs, covsDiffs)

  svdM <- svd(M)
  D_p <- svdM$d
  D_r <- diag(c(D_p[1:targetDim],rep(0,length(D_p) - targetDim)))
  F_mat <- (svdM$u)%*%D_r
  F_mat[,1:targetDim]
}
