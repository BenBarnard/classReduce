# The reduced dimension data sets This will take in a training data frame, then
# multiply this data frame by the projection matrices to create a list of 6 (one
# for each method of inquiry) data frames. Each data frame will have N
# observatiosn projected into q dimensions.
reduceData <- function(training_df, methods_ls, popColName, targetDim, summary){
  # This is a 3Nx1 data frame of population labels
  labels <- training_df %>% dplyr::select(eval(as.name(popColName)))
  # This is a 3Nxp matrix of data
  data_mat <- training_df %>% dplyr::select(-eval(as.name(popColName))) %>% as.matrix



  # We now multiply the transposed MP Inverse of F (returned by
  # projectionMatrices in 4TheMethodsFile.R) by the transpose of the data
  # matrix. This yields a q by 3N matrix of reduced-dimension data. This takes
  # in a list of Fs, and returns a list of reduced data frames.
  reducedData_ls <- llply(methods_ls, function(method, summary, dim, data, labels){
    reducedData <- (t(projectionMatrix(method, summary, dim)) %*%
                      t(data)) %>% t()
    cbind(labels,reducedData)
  }, summary = summary, dim = targetDim, data = data_mat, labels = labels)
}
