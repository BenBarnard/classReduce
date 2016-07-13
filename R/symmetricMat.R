##### Function to Return Distance Matrix ######################################
# For commented versions, see Cosine_Metric_Tests.R
symmetricMat <- function(data, pop1, pop2, distance, factors) {
  factorCols <- sapply(data, is.factor)
  data[factorCols] <- lapply(data[factorCols], as.character)

  mat <- diag(x = 1, nrow = length(unique(factors)))
  rownames(mat) <- factors
  colnames(mat) <- factors

  for(i in 1:nrow(data)) {
    mat[data[i, pop1], data[i, pop2]] <- data[i, distance][[1]]
    mat[data[i, pop2], data[i, pop1]] <- data[i, distance][[1]]
  }

  mat
}
