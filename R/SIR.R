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
#' @importFrom lazyeval expr_find
#'
SIR.grouped_df <- function(x, targetDim, ..., svdMethod = svd){
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    targetDim = targetDim,
                    test = expr_find(SIR.matrix),
                    svdMethod = expr_find(svdMethod))
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
SIR.matrix <- function(...){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  prior <- prior(matrix_ls)
  xbar <- lapply(matrix_ls, colMeans)
  B <- S_B(prior, xbar)
  S <- S_W(prior, matrix_ls)
  rootGammaInv <- matInvSqrt(B + S)
  M <- rootGammaInv %*% B %*% rootGammaInv
  projection <- t(rootGammaInv %*% do.call(lazy_eval(ls$svdMethod), list(M))$u[,1:lazy_eval(ls$targetDim)])

  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))
  originalData <- Reduce(rbind, matrix_ls)
  names(nameVec) <- paste(ls$group$expr)
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = group_by_(cbind(as.data.frame(reducedData), nameVec),
                                         paste(ls$group$expr)),
                 projectionMatrix = projection,
                 group = ls$group$expr,
                 discrimFunc = expr_find(qda))
  class(object) <- "reduced"
  object
}
