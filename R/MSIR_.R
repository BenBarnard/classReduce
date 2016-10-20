###### Sliced Inverse Regression ##############################################
MSIR_ <- function(x, ...){
  UseMethod("MSIR_")
}

MSIR_.data.frame <- function(x, group, targetDim, ..., variables, samples, value, tidy = FALSE){
  if(tidy == TRUE){
    tidyDataDftoMatrix(data = x,
                       group = expr_find(group),
                       targetDim = targetDim,
                       variables = expr_find(variable),
                       samples = expr_find(samples),
                       value = expr_find(value),
                       test = expr_find(MSIR_.matrix))
  }else{
    dataDftoMatrix(data = x,
                   group = expr_find(group),
                   targetDim = targetDim,
                   test = expr_find(MSIR_.matrix))
  }
}

MSIR_.matrix <- function(...){
  ls <- list(...)
  matrix_ls <- ls[-length(ls)]
  targetDim <- ls$targetDim
  B <- S_B(matrix_ls)
  S <- S_W(matrix_ls)
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv

  MSIR_.default(rootGammaInv = rootGammaInv, M = M, targetDim = targetDim)
}

MSIR_.default <- function(rootGammaInv, M, targetDim) {
  svd_ <- svd(M)
  projection <- rootGammaInv %*% (svd_$u[, 1:targetDim])
  list(rootGammaInv = rootGammaInv,
       u = svd_$u,
       d = svd_$d,
       v = svd_$v,
       projection = projection)
}
