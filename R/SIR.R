#'Sliced Inverse Regression
#'
#' @inheritParams SYS
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix.
#' @export
#'
SIR <- function(x, svdMethod = svd, ...){
  
  matrix_ls <- x

  prior <- prior(matrix_ls)
  xbar <- lapply(matrix_ls, colMeans)
  B <- S_B(prior, xbar)
  S <- S_W(prior, matrix_ls)
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv
  projection <- t(rootGammaInv %*% do.call(svdMethod, list(M))$u)

  projectedData <- lapply(matrix_ls, FUN = projection_func, proj = projection)
  
  object <- list(projectedData = projectedData,
                 projectionMatrix = projection,
                 M = M,
                 method = SIR)
  object
}
