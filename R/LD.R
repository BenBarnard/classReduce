#'LD
#'
#' @param x data
#' @param ... other options such as group variable for 
#' @param targetDim target dimension to reduce the data to
#' @param svdMethod svd function used for dimesion reduction by default 
#'                  svd in base is used
#' @param group grouping variable
#'                  
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix and dimension reduction method used.
#' @export
#'
#' @examples LD(iris, group = Species, targetDim = 1)
LD <- function(x, ...){
  UseMethod("LD")
}

#' @rdname LD
#' @export
#' @keywords internal
#' 
#' @importFrom lazyeval expr_find
#'
LD.data.frame <- function(x, group, ...){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    method = expr_find(LD.matrix),
                    .dots = lazy_dots(...))
}

#' @rdname LD
#' @export
#' @keywords internal
#'
#' @importFrom lazyeval expr_find
#'
LD.grouped_df <- function(x, ...){
  dataDftoMatrixDim(data = x,
                    group = attributes(x)$vars[[1]],
                    method = expr_find(LD.matrix),
                    .dots = lazy_dots(...))
}

#' @rdname LD
#' @export
#' @keywords internal
#' 
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom stats cov
#' @importFrom utils combn
#'
LD.matrix <- function(..., targetDim, svdMethod = svd){
  ls <- lazy_dots(...)
  matrix_ls <- lazy_eval(ls[str_detect(names(ls), "x.")])
  names(matrix_ls) <- str_replace(names(matrix_ls), "x.", "")

  prior <- prior(matrix_ls)
  xbar <- lapply(matrix_ls, colMeans)
  B <- S_B(prior, xbar)
  S <- S_W(prior, matrix_ls)
  covs <- lapply(matrix_ls, cov)

  combns <- combn(length(matrix_ls), 2, simplify = FALSE)
  mld_diff <- lapply(combns, function(x){
    (xbar[[x[1]]] - xbar[[x[2]]]) %*% t(xbar[[x[1]]] - xbar[[x[2]]])
  })

  mld_pie <- lapply(combns, function(x){
    c(prior[[x[1]]] / (prior[[x[1]]] + prior[[x[2]]]),
      prior[[x[2]]] / (prior[[x[1]]] + prior[[x[2]]]))
  })

  Sij <- lapply(1:length(combns), function(x){
    combns <- combns[[x]]
    mld_pie[[x]][1] * covs[[combns[1]]] +
      mld_pie[[x]][2] * covs[[combns[2]]]
  })

  mld_fun  <- lapply(1:length(combns), function(x){
    combns <- combns[[x]]
    someRootInv <- matInvSqrt(
      matInvSqrt(S) %*%
        Sij[[x]] %*%
        matInvSqrt(S)
    )

    prior[[combns[1]]] * prior[[combns[2]]] * solve(S) %*% solve(matInvSqrt(S)) %*%
      (
        someRootInv %*%
          matInvSqrt(S) %*%
          mld_diff[[x]] %*%
          matInvSqrt(S) %*%
          someRootInv +
          (1 / (mld_pie[[x]][1] * mld_pie[[x]][2])) * logd(matInvSqrt(S) %*%
                                                             Sij[[x]] %*%
                                                             matInvSqrt(S)) -
          mld_pie[[x]][1] *  logd(matInvSqrt(S) %*%
                                    covs[[combns[1]]] %*%
                                    matInvSqrt(S)) -
          mld_pie[[x]][2] *  logd(matInvSqrt(S) %*%
                                    covs[[combns[2]]] %*%
                                    matInvSqrt(S))

      ) %*%
      solve(matInvSqrt(S))
  })

  M <- Reduce(`+`, mld_fun)

  projection <- t(do.call(svdMethod, list(M))$u[,1:targetDim])

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
                 M = M,
                 method = LD)
  class(object) <- "reduced"
  object
}
