##### Classifying Functions ###################################################
# For comments, see Mahalanobis_Distance_Functions.R
Mahalanobis <- function(observation, meansList, invCovsList) {
  observation <- observation %>% as.numeric
  vec <- sapply(1:length(meansList), function(i) {
    t(observation - meansList[[i]]) %*%
      invCovsList[[i]] %*%
      (observation - meansList[[i]])
  })
  names(vec) <- names(invCovsList)
  t(vec)
}
