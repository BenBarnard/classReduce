allPoolCov <- function(weightMat, listOfMatrices) {
  list <- lapply(1:nrow(weightMat), function(i) {
    poolCov(weightMat, listOfMatrices, row = i)
  })
  names(list) <- names(listOfMatrices)
  list
}
