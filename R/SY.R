#'SY
#'
#' @param x data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples SY(iris, group = Species, targetDim = 1)
SY <- function(x, ...){
  UseMethod("SY")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#'
SY.data.frame <- function(x, group, ...){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    method = expr_find(SY.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#'
SY.grouped_df <- function(x, ...){
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    method = expr_find(SY.matrix),
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
SY.matrix <- function(..., targetDim, svdMethod = svd){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")


  xbar <- lapply(matrix_ls, colMeans)
  covs <- lapply(matrix_ls, cov)
  invCovs <- lapply(covs, solve)

  projectedMeanDiffs <- Reduce(cbind, mapply(function(x, y){
    x %*% y - invCovs[[1]] %*% xbar[[1]]
  }, invCovs, xbar, SIMPLIFY = FALSE)[-1])

  covsDiffs <- Reduce(cbind, lapply(covs, function(x){x - covs[[1]]})[-1])

  M <- cbind(projectedMeanDiffs, covsDiffs)

  projection <- t(do.call(svdMethod, list(M))$u[,1:targetDim])
  originalData <- Reduce(rbind, matrix_ls)
  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))

  names(nameVec) <- paste(ls$group$expr)
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = group_by_(cbind(as.data.frame(reducedData), nameVec),
                                         paste(ls$group$expr)),
                 projectionMatrix = projection,
                 group = ls$group$expr,
                 discrimFunc = expr_find(qda),
                 M = M)
  class(object) <- "reduced"
  object
}
