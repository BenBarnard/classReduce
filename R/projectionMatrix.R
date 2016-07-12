##### Test Values ############################################################
# This function applies the desired arguments to a single method, then returns
# the transposed Moore-Penrose Inverse.
projectionMatrix <- function(method, data_summary_ls, targetDim){
  args <- list(
    data_summary_ls = data_summary_ls,
    targetDim = targetDim
  )
  do.call(eval(method), args)
}
