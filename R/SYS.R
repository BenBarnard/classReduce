#'SYS
#'
#' @param x data
#' @param targetDim target dimension to reduce the data to
#' @param svdMethod svd function used for dimesion reduction by default 
#'                  svd in base is used
#' @param ... other options such as group variable for 
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix.
#' @export
#'
#' @examples SYS(iris, group = Species, targetDim = 1)
SYS <- function(x, ...){
  UseMethod("SYS")
}

#' @keywords internal
#' @export
#' @rdname SYS
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#'
SYS.data.frame <- function(x, group, ...){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    method = expr_find(SYS.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @rdname SYS
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#'
SYS.grouped_df <- function(x, ...){
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    method = expr_find(SYS.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @rdname SYS
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#'
SYS.resample <- function(x, ...){
  x <- as.data.frame(x)
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    method = expr_find(SYS.matrix),
                    .dots = lazy_dots(...))
}

#' @keywords internal
#' @export
#' @rdname SYS
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom dplyr group_by_
#' @importFrom covEst Haff_shrinkage
#' @importFrom stats cov
#'
SYS.matrix <- function(..., group, targetDim, svdMethod = svd, shrinkage = Haff_shrinkage){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  xbar <- lapply(matrix_ls, colMeans)
  covs <- lapply(matrix_ls, cov)
  invCovs <- lapply(covs, solve)

  StildeInv_ls <- lapply(matrix_ls, function(x, data){
    do.call(shrinkage,
            c(x = list(x), data = list(data)))
    }, data = matrix_ls)

  projectedMeanDiffs <- Reduce(cbind, mapply(function(x, y){
    x %*% y - StildeInv_ls[[1]] %*% xbar[[1]]
  }, StildeInv_ls, xbar, SIMPLIFY = FALSE)[-1])

  covsDiffs <- Reduce(cbind, lapply(covs, function(x){x - covs[[1]]})[-1])

  M <- cbind(projectedMeanDiffs, covsDiffs)

  projection <- t(do.call(svdMethod, list(M))$u[,1:targetDim])

  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))
  originalData <- Reduce(rbind, matrix_ls)
  names(nameVec) <- paste(expr_find(group))
  reducedData <- t(projection %*% t(originalData))

  object <- list(reducedData = group_by_(cbind(as.data.frame(reducedData), nameVec),
                                        paste(expr_find(group))),
                 projectionMatrix = projection,
                 group = expr_find(group),
                 discrimFunc = expr_find(qda),
                 M = M)
  class(object) <- "reduced"
  object
}
