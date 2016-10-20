#'Sliced Inverse Regression
#'
#' @param x data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples MSIR(mcSamples(c(0,0,0), diag(1, 3), 10, 3, matrix = FALSE, tidy = TRUE), group = population, targetDim = 1, variables = variable, samples = samples, value = value, tidy = TRUE)
SIR <- function(x, ...){
  UseMethod("SIR")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#'
SIR.data.frame <- function(x, group, targetDim, ...,
                            variables, samples, value, tidy = FALSE,
                            svdMethod = svd){
  if(tidy == TRUE){
    tidyDataDftoMatrixDim(data = x,
                       group = expr_find(group),
                       targetDim = targetDim,
                       variables = expr_find(variable),
                       samples = expr_find(samples),
                       value = expr_find(value),
                       test = expr_find(SIR.matrix),
                       svdMethod = expr_find(svdMethod))
  }else{
    dataDftoMatrixDim(data = x,
                   group = expr_find(group),
                   targetDim = targetDim,
                   test = expr_find(MSIR.matrix),
                   svdMethod = expr_find(svdMethod))
  }
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr string_detect
#'
SIR.matrix <- function(...){
  browser()
  ls <- list(...)
  matrix_ls <- ls[str_detect(names(ls), "x.")]
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")
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
