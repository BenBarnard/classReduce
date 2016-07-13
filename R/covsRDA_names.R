##### The RDA Pooled Sigmas ###################################################
# For comments, see Functions_to_downdate_Sigmas.R
# Create a list of inverse covariances to update
# Remember, you need covs_ls and poolCovs_ls built first.
# For a given parameter touple, calculate a list of covariance matrices
covsRDA_names <- function(names, lambda, gamma) {
  Ip <- diag(1, nrow = ncol(covs_ls[[names]]))
  Sig_lambda <- (1 - lambda) * covs_ls[[names]] +
    lambda * poolCovs_ls[[names]]
  (1 - gamma) * Sig_lambda +
    gamma * (tr(Sig_lambda) / ncol(Ip)) ^ 2 * Ip
}
