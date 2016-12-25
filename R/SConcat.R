#'Sample Covariances Concatenated
#'
#' @param x data
#' @param ... other variables
#'
#' @export
#' @keywords internal
#'
SConcat <- function(x, ...){
  UseMethod("SConcat")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#'
SConcat.data.frame <- function(x, group, targetDim, ..., svdMethod = svd){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    method = expr_find(SConcat.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom stats cov
#'
SConcat.matrix <- function(...){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  covs <- lapply(matrix_ls, cov)

  M <- Reduce(cbind, covs)
  projection <- t(do.call(lazy_eval(ls$svdMethod), list(M))$u[,1:lazy_eval(ls$targetDim)])

  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))
  originalData <- Reduce(rbind, matrix_ls)
  names(nameVec) <- paste(ls$group$expr)
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = cbind(as.data.frame(reducedData), nameVec),
                 projectionMatrix = projection,
                 group = ls$group$expr)
  class(object) <- "reduced"
  object
}
