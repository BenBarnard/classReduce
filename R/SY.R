#'SY
#'
#' @inheritParams SYS
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix.
#' @export
#'
SY <- function(x, svdMethod = svd, ...){
  matrix_ls <- x

  xbar <- lapply(matrix_ls, colMeans)
  covs <- lapply(matrix_ls, cov)
  invCovs <- lapply(covs, solve)

  projectedMeanDiffs <- Reduce(cbind, mapply(function(x, y){
    x %*% y - invCovs[[1]] %*% xbar[[1]]
  }, invCovs, xbar, SIMPLIFY = FALSE)[-1])

  covsDiffs <- Reduce(cbind, lapply(covs, function(x){x - covs[[1]]})[-1])

  M <- cbind(projectedMeanDiffs, covsDiffs)

  projection <- t(do.call(svdMethod, list(M))$u)
  
  projectedData <- lapply(matrix_ls, FUN = projection_func, proj = projection)
  
  object <- list(projectedData = projectedData,
                 projectionMatrix = projection,
                 M = M,
                 method = SY)
  object
}
