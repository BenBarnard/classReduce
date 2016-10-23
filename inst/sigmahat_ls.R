sigmaHat_ls <- function(lambda, gamma) {
  list <- lapply(names(covs_ls), function(x){
    covsRDA_names(x, lambda, gamma) %>%
      data.frame %>%
      as.matrix
  })
  names(list) <- names(covs_ls)
  list
}
