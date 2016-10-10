###### Sliced Inverse Regression ##############################################
MSIR_ <- function(x, ...){
  UseMethod("MSIR_")
}

MSIR_.data.frame <- function(x, group = Group, targetDim, ...){
  do.call(what = MSIR_.matrix,
          args = c(dlply(.data = x,
                         .variables = expr_find(group),
                         .fun = dataDftoMatrix),
                   targetDim = targetDim))
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
