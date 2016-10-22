#'Sliced Inverse Regression
#'
#' @param x data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples SIR(iris, group = Species, targetDim = 1)
SIR <- function(x, ...){
  UseMethod("SIR")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#'
SIR.data.frame <- function(x, group, targetDim, ..., svdMethod = svd){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    test = expr_find(SIR.matrix),
                    svdMethod = expr_find(svdMethod))
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#'
SIR.matrix <- function(...){
  ls <- list(...)
  matrix_ls <- ls[str_detect(names(ls), "x.")]
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  prior <- prior(matrix_ls)
  xbar <- xbar(matrix_ls)
  mu <- mu(matrix_ls)
  B <- S_B(prior, xbar)
  S <- S_W(prior, matrix_ls)
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv
  projection <- t(rootGammaInv %*% do.call(ls$svdMethod, list(M))$u[,1:ls$targetDim])
  originalData <- Reduce(rbind, matrix_ls)
  nameVec <- Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                    matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = cbind(as.data.frame(reducedData), nameVec),
                 projectionMatrix = projection,
                 origianlData = cbind(as.data.frame(originalData), nameVec))
  class(object) <- "reduced"
  object
}
