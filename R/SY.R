###### Ounpraseuth, Young, and Young (SY) ######################################
SY <- function(data_summary_ls, targetDim){
  # data_summary_ls is passed from 3Summary_Stats.R
  # targetDim is the dimesion to which we would like to reduce

  means_ls <- data_summary_ls$xBars
  covs_ls <- data_summary_ls$S
  invCovs_ls <- data_summary_ls$SInv
  # We first find the projected mean differences. This will have the form SInv_i
  # * xbar_i - SInv_1 * xbar_1, for each population i in 2:m.
  projectedMeanDiffs <- sapply(2:length(means_ls), function(population){
    invCovs_ls[[population]] %*% means_ls[[population]] -
      invCovs_ls[[1]] %*% means_ls[[1]]
  })

  # Now we create the m - 1 covariance differences, and store these matrices as
  # a single p * (m * p) matrix
  covsDiffs <- do.call(cbind, lapply(2:length(covs_ls), function(population){
    covs_ls[[population]] - covs_ls[[1]]
  }))

  # Now we have m vectors of dimension p * 1, and a p * (m * p) matric of the
  # covariance differences. We then append these vectors to a concatenated
  # matrix of all the covariance matrices. This will create a matrix M with
  # dimension p * (m + m * p).
  M <- cbind(projectedMeanDiffs, covsDiffs)

  # Finally, we take the Singular Value Decomposition of M
  svdM <- svd(M)
  # The diagonal matrix of the eigenvalues with small relative eigenvalues
  # replaced with 0.
  D_p <- svdM$d
  D_r <- diag(c(D_p[1:targetDim],rep(0,length(D_p) - targetDim)))
  # This creates an r-rank decomposition of M, which we then truncate to F from
  # pxp to pxr. This gives an rxp projection matrix (F^+) to project px1
  # observations into qx1 space.
  F_mat <- (svdM$u)%*%D_r
  F_mat[,1:targetDim]
}
