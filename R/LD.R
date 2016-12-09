#'LD
#'
#' @param x data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples LD(iris, group = Species, targetDim = 1)
LD <- function(x, ...){
  UseMethod("LD")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#'
LD.data.frame <- function(x, group, targetDim, ..., svdMethod = svd){
  dataDftoMatrixDim(data = x,
                    group = expr_find(group),
                    targetDim = targetDim,
                    test = expr_find(LD.matrix),
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
LD.matrix <- function(...){
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
    c(prior[[x[1]]] / (prior[[x[1]]] + prior[[x[2]]]), prior[[x[2]]] / (prior[[x[1]]] + prior[[x[2]]]))
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
