# Combine the mean and varaince reduction methods into one
prescreen <- function(df, label, meanCols = 100, varCols = 25) {
  reduceXMean <- lmDimReduce(df, label, meanCols)
  reduceXVar <- bfDimReduce(df, label, varCols)
  merge(reduceXMean, reduceXVar) %>%
    tbl_df
}
