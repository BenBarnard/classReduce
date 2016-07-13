poolCov <- function(weightMat, listOfMatrices, row) {
  prods <- lapply(1:length(listOfMatrices), function(i) {
    listOfMatrices[[i]] * weightMat[row, i]
  })
  Reduce("+", prods) / sum(weightMat[row,])
}
