#' Data Concatenated and Scatter Matrix
#'
#' @param x data
#' @param ... other options
#'
#' @keywords internal
#' @export
#'
DataConcatScatterBlock <- function(x, ...){
  UseMethod("DataConcatScatterBlock")
}

#' @keywords internal
#' @export
#' @importFrom lazyeval expr_find
DataConcatScatterBlock.data.frame <- function(x, group, targetDim, ...){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    method = expr_find(DataConcatScatterBlock.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
SAVE.resample <- function(x, targetDim, ...){
  x <- as.data.frame(x)
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    targetDim = targetDim,
                    method = expr_find(DataConcatScatterBlock.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
DataConcatScatterBlock.matrix <- function(..., targetDim, svdMethod = svd){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  dataConcat <- Reduce(cbind, matrix_ls)

  scatter <- scatterMat(dataConcat)

  M <- Reduce(cbind, matBlocks(scatter, rows = ncol(matrix_ls[[1]]), columns = ncol(matrix_ls[[1]])))
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

#' Helper for Data Concat Scatter Block
#'
#' @param x data
#' @param rows number of rows
#' @param columns number of columns
#'
#' @export
#' @keywords internal
#' 
matBlocks <- function(x, rows, columns){
  rg <- (row(x) - 1) %/% rows + 1
  cg <- (col(x) - 1) %/% columns + 1
  rci <- (rg - 1) * max(cg) + cg
  tri <- rci[upper.tri(rci, diag = TRUE)]
  N <- prod(dim(x)) / rows / columns
  cv <- lapply(unique(tri), function(y){
    mat <- x[rci == y]
    dim(mat) <- c(rows, columns)
    mat
  })
  cv
}


