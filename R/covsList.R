##### Pooled Covariance Matrices Functions ####################################
# For commented versions, see Cosine_Metric_Tests.R
covsList <- function(screened_df, popColName) {
  popList <- dlply(screened_df, .variables = popColName, .fun = function(x) x)
  # This doesn't work for indicator (0/1) variables.
  covList <- llply(.data = popList, .fun = function(x) cov(x[sapply(x,is.numeric)]))
  list(PopData = popList, PopCovs = covList)
}
