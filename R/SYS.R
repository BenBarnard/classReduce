###### Young, Young, Nelson and Van Zyl (SYS) #################################
SYS <- function(data, ...){
  useMethod("SYS")
}

SYS.data.frame <- function(data, ...){
  do.call(SYS.matrix,
          data %>%
            dlply(.(Group), dataDftoMatrix))
}

SYS.matrix <- function(...){

}

SYS <- function(data_summary_ls, targetDim){

  means_ls <- data_summary_ls$xBars
  covs_ls <- data_summary_ls$S
  invCovs_ls <- data_summary_ls$SInv
  N <- data_summary_ls$N
  p <- nrow(covs_ls[[1]])

  # The population covariance estimates are shrunk by the following functions:
  tu <- function(S, n) {
    u <- p * det(S) ^ (1 / p) / tr(S)
    min((4 * (p ^ 2 - 1) / ((n - p - 2) * p ^ 2)), 1) * u ^ (1 / p)
  }
  StildeInv_ls <- lapply(1:length(covs_ls), function(population){
    (1 - tu(covs_ls[[population]], N[[population]])) *
      (N[[population]] - p - 2) * invCovs_ls[[population]] +
      ((tu(covs_ls[[population]], N[[population]]) *
          (N[[population]] * p - p - 2)) / tr(covs_ls[[population]])) *
      diag(1, p)
  })

  projectedMeanDiffs <- sapply(2:length(means_ls), function(population){
    StildeInv_ls[[population]] %*% means_ls[[population]] -
      StildeInv_ls[[1]] %*% means_ls[[1]]
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
