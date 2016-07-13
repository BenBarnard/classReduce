# Calculate the list of population mean vectors
meanVecs <- function(dfs_ls) {
  llply(.data = dfs_ls, .fun = function(x){
    colMeans(x[sapply(x,is.numeric)])
  })
}
