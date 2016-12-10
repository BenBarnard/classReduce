#'SYS
#'
#' @param x data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples SYS(iris, group = Species, targetDim = 1)
SYS <- function(x, ...){
  UseMethod("SYS")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#'
SYS.data.frame <- function(x, group, targetDim, ..., shrinkage = Haff_shrinkage, svdMethod = svd){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    test = expr_find(SYS.matrix),
                    svdMethod = expr_find(svdMethod),
                    shrinkage = expr_find(shrinkage))
}

#' @keywords internal
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom covEst Haff_shrinkage
#'
SYS.matrix <- function(...){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  xbar <- lapply(matrix_ls, colMeans)
  covs <- lapply(matrix_ls, cov)
  invCovs <- lapply(covs, solve)

  StildeInv_ls <- lapply(matrix_ls, function(x, data){
    do.call(paste(lazy_eval(lazy_eval(ls$.dots.shrinkage))), c(x = list(x), data = list(data)))
    }, data = matrix_ls)

  projectedMeanDiffs <- Reduce(cbind, mapply(function(x, y){
    x %*% y - StildeInv_ls[[1]] %*% xbar[[1]]
  }, StildeInv_ls, xbar, SIMPLIFY = FALSE)[-1])

  covsDiffs <- Reduce(cbind, lapply(covs, function(x){x - covs[[1]]})[-1])

  M <- cbind(projectedMeanDiffs, covsDiffs)

  projection <- t(do.call(lazy_eval(ls$svdMethod), list(M))$u[,1:lazy_eval(ls$targetDim)])

  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))
  originalData <- Reduce(rbind, matrix_ls)
  names(nameVec) <- paste(ls$group$expr)
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = cbind(as.data.frame(reducedData), nameVec),
                 projectionMatrix = projection,
                 group = ls$group$expr,
                 discrimFunc = expr_find(qda))
  class(object) <- "reduced"
  object
}
