#'SYS
#'
#' @param x data
#' @param svdMethod svd function used for dimesion reduction by default 
#'                  svd in base is used
#' @param covEst covariance estimator used forming M matrix
#' @param ... options passed to covEst and svd methods
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix.
#' @export
#'
SYS <- function(x, svdMethod = svd, covEst = Haff_shrinkage, ...){
  matrix_ls <- x
  
  ls <- list(...)
  ls$data <- matrix_ls

  xbar <- lapply(matrix_ls, colMeans)
  covs <- lapply(matrix_ls, cov)
  invCovs <- lapply(covs, solve)

  StildeInv_ls <- lapply(matrix_ls, function(x, ls){
    ls$x <- x
    do.call(covEst, ls)
    }, ls = ls)

  projectedMeanDiffs <- Reduce(cbind, mapply(function(x, y){
    x %*% y - StildeInv_ls[[1]] %*% xbar[[1]]
  }, StildeInv_ls, xbar, SIMPLIFY = FALSE)[-1])

  covsDiffs <- Reduce(cbind, lapply(covs, function(x){x - covs[[1]]})[-1])

  M <- cbind(projectedMeanDiffs, covsDiffs)

  projection <- t(do.call(svdMethod, list(M))$u)

  projectedData <- lapply(matrix_ls, FUN = projection_func, proj = projection)

  object <- list(projectedData = projectedData,
                 projectionMatrix = projection,
                 M = M,
                 method = SYS,
                 covEst = covEst)
  object
}
