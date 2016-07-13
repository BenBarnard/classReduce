# Create list of downdated covariance inverses
downDateCovs <- function(testdata, listOfInvCovs, positOfLabel) {
  V <- testdata %>% dplyr::select(-positOfLabel) %>% as.matrix
  U <- t(V)
  C <- diag(1, nrow = nrow(V))

  woodwrap <- function(x) woodburyInv(x, U = U, C = C, V = V)

  list <- listOfInvCovs %>% plyr::llply(.fun = woodwrap)
  names(list) <- names(covs_ls)
  list
}
