#' Data Concatenated and Scatter Matrix
#'
#' @inheritParams SYS
#'
#' @keywords internal
#' @export
#'
DataConcatScatter <- function(x, ...){
  UseMethod("DataConcatScatter")
}

#' @keywords internal
#' @export
#' @importFrom lazyeval expr_find
DataConcatScatter.data.frame <- function(x, group, targetDim, ...){
  dataDftoMatrix(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    method = expr_find(DataConcatScatter.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @rdname DataConcatScatter
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
DataConcatScatter.resample <- function(x, targetDim, ...){
  x <- as.data.frame(x)
  dataDftoMatrix(data = x,
                    group = attributes(x)$vars[[1]],
                    targetDim = targetDim,
                    method = expr_find(DataConcatScatter.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
DataConcatScatter.matrix <- function(..., targetDim, svdMethod = svd){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  dataConcat <- Reduce(rbind, matrix_ls)

  M <- scatterMat(dataConcat)
  projection <- t(do.call(svdMethod, list(M))$u[,1:targetDim])

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
