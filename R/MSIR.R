###### Sliced Inverse Regression ##############################################
MSIR <- function(x, ...){
  useMethod("MSIR")
}

MSIR.data.frame <- function(x, group = Group, targetDim, ..., discrimFunc = MASS::qda){
  do.call(what = MSIR.matrix,
          args = c(dlply(.data = x,
                         .variables = expr_find(group),
                         .fun = dataDftoMatrix),
                   targetDim = targetDim,
                   discrimFunc = discrimFunc))
}

MSIR.matrix <- function(...){
  ls <- list(...)
  matrix_ls <- ls[-length(ls)]
  targetDim <- ls$targetDim
  B <- S_B(matrix_ls)
  S <- S_W(matrix_ls)
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv
  projection <- MSIR_.default(rootGammaInv = rootGammaInv,
                              M = M,
                              targetDim = targetDim)$projection
  n_ls <- llply(matrix_ls, nrow)
  names_ls <- names(matrix_ls)
  group <- Reduce(rbind, mlply(cbind(n_ls, names_ls), function(n_ls, names_ls){
    rep(names_ls, n_ls)
  }))
  reduced <- t(t(projection) %*% t(reduce(rbind, matrix_ls)))


}
