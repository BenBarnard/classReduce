#'LD
#'
#' @inheritParams SYS
#'                  
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix and dimension reduction method used.
#' @export
#' 
#' @importFrom utils combn
#'
LD <-  function(x, svdMethod = svd, ...){

  matrix_ls <- x

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

  projection <- t(do.call(svdMethod, list(M))$u)

  projectedData <- lapply(matrix_ls, FUN = projection_func, proj = projection)
  
  object <- list(projectedData = projectedData,
                 projectionMatrix = projection,
                 M = M,
                 method = LD)
  object
}
