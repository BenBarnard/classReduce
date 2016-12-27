#'SAVE
#'
#' @param x data
#' @param group grouping variable
#' @param targetDim target dimension to reduce the data to
#' @param svdMethod svd function used for dimesion reduction by default 
#'                  svd in base is used
#' @param ... other options
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix.
#' @export
#'
#' @examples SAVE(iris, group = Species, targetDim = 1)
SAVE <- function(x, ...){
  UseMethod("SAVE")
}

#' @rdname SAVE
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
SAVE.data.frame <- function(x, group, targetDim, ...){
  dataDftoMatrix(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    method = expr_find(SAVE.matrix),
                    .dots = lazy_dots(...))
}

#' @rdname SAVE
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
SAVE.grouped_df <- function(x, targetDim, ...){
  dataDftoMatrix(data = x,
                    group = attributes(x)$vars[[1]],
                    targetDim = targetDim,
                    method = expr_find(SAVE.matrix),
                    .dots = lazy_dots(...))
}

#' @export
#' @rdname SAVE
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
SAVE.resample <- function(x, targetDim, ...){
  x <- as.data.frame(x)
  dataDftoMatrix(data = x,
                    group = attributes(x)$vars[[1]],
                    targetDim = targetDim,
                    method = expr_find(SAVE.matrix),
                    .dots = lazy_dots(...))
}

#' @rdname SAVE
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom stats cov
SAVE.matrix <- function(..., targetDim, svdMethod = svd){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  prior <- prior(matrix_ls)
  xbar <- lapply(matrix_ls, colMeans)
  B <- S_B(prior, xbar)
  S <- S_W(prior, matrix_ls)
  covs <- lapply(matrix_ls, cov)

  S_hatGamma <- (1 / length(covs)) *
    Reduce(`+`,
           lapply(covs, function(x){
             (x - S) %*%
               solve(B + S) %*%
               (x - S)
           })
    )

  rootGammaInv <- matInvSqrt(B + S)

  M <- (rootGammaInv %*% B %*% rootGammaInv) %*%
    (rootGammaInv %*% B %*% rootGammaInv) +
    (rootGammaInv %*% S_hatGamma %*% rootGammaInv)

  projection <- t(rootGammaInv %*% do.call(svdMethod, list(M))$u[,1:targetDim])

  nameVec <- as.data.frame(as.matrix(Reduce(c, mapply(function(x, y){rep(y, nrow(x))},
                                                      matrix_ls, names(matrix_ls), SIMPLIFY = FALSE))))
  originalData <- Reduce(rbind, matrix_ls)
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
