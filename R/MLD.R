###### The Loog and Duin Method ###############################################

MLD <- function(data_summary_ls, targetDim){
  prior <- data_summary_ls$Priors
  covs_ls <- data_summary_ls$S
  S_w <- data_summary_ls$S_W
  S_B <- data_summary_ls$S_B
  combn <- data_summary_ls$Combns
  mld_diff <- data_summary_ls$MLD_diff
  mld_pie <- data_summary_ls$MLD_pie



  # S_ij is a linear combination of the matrix S_i and S_j times their relative
  # sample sizes (the prior probability)
  Sij <- llply(1:length(combn), function(x, mld_pie, covs_ls, combns){
    combn <- combns[[x]]
    mld_pie[[x]][1] * covs_ls[[combn[1]]] +
      mld_pie[[x]][2] * covs_ls[[combn[2]]]
  }, mld_pie = mld_pie, covs_ls = covs_ls, combns = combn)


  # Using B_comp
  mld_fun  <- llply(1:length(combn), function(x, Sij, mld_pie, covs_ls, combns, prior, S_w, S_B, mld_diff){
    combn <- combns[[x]]
    someRootInv <- matInvSqrt(
      matInvSqrt(S_w) %*%
        Sij[[x]] %*%
        matInvSqrt(S_w)
    )

    prior[[combn[1]]] * prior[[combn[2]]] * solve(S_w) %*% solve(matInvSqrt(S_w)) %*%
      (
        someRootInv %*%
          matInvSqrt(S_w) %*%
          mld_diff[[x]] %*%
          matInvSqrt(S_w) %*%
          someRootInv +
          (1 / (mld_pie[[x]][1] * mld_pie[[x]][2])) * logd(matInvSqrt(S_w) %*%
                                                             Sij[[x]] %*%
                                                             matInvSqrt(S_w)) -
          mld_pie[[x]][1] *  logd(matInvSqrt(S_w) %*%
                                    covs_ls[[combn[1]]] %*%
                                    matInvSqrt(S_w)) -
          mld_pie[[x]][2] *  logd(matInvSqrt(S_w) %*%
                                    covs_ls[[combn[2]]] %*%
                                    matInvSqrt(S_w))

      ) %*%
      solve(matInvSqrt(S_w))
  }, Sij = Sij, mld_pie = mld_pie, covs_ls = covs_ls, combns = combn, prior = prior, S_w = S_w, S_B = S_B, mld_diff = mld_diff)

  M <- Reduce(`+`, mld_fun)

  svdM <- svd(M)
  D_p <- svdM$d
  D_r <- diag(c(D_p[1:targetDim],rep(0,length(D_p) - targetDim)))
  F_mat <- (svdM$u)%*%D_r
  F_mat[,1:targetDim]
}
