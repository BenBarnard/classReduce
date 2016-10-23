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
SIR.data.frame <- function(x, group, targetDim, ..., svdMethod = svd){
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

  prior <- data_summary_ls$Priors
  covs_ls <- data_summary_ls$S
  S_w <- data_summary_ls$S_W
  S_B <- data_summary_ls$S_B
  combn <- data_summary_ls$Combns
  mld_diff <- data_summary_ls$MLD_diff
  mld_pie <- data_summary_ls$MLD_pie



  # S_ij is a linear combination of the matrix S_i and S_j times their relative
  # sample sizes (the prior probability)
  Sij <- llply(1:length(combn), function(x, mld_pie, covs_ls, combns){
    combn <- combns[[x]]
    mld_pie[[x]][1] * covs_ls[[combn[1]]] +
      mld_pie[[x]][2] * covs_ls[[combn[2]]]
  }, mld_pie = mld_pie, covs_ls = covs_ls, combns = combn)


  # Using B_comp
  mld_fun  <- llply(1:length(combn), function(x, Sij, mld_pie, covs_ls, combns, prior, S_w, S_B, mld_diff){
    combn <- combns[[x]]
    someRootInv <- matInvSqrt(
      matInvSqrt(S_w) %*%
        Sij[[x]] %*%
        matInvSqrt(S_w)
    )

    prior[[combn[1]]] * prior[[combn[2]]] * solve(S_w) %*% solve(matInvSqrt(S_w)) %*%
      (
        someRootInv %*%
          matInvSqrt(S_w) %*%
          mld_diff[[x]] %*%
          matInvSqrt(S_w) %*%
          someRootInv +
          (1 / (mld_pie[[x]][1] * mld_pie[[x]][2])) * logd(matInvSqrt(S_w) %*%
                                                             Sij[[x]] %*%
                                                             matInvSqrt(S_w)) -
          mld_pie[[x]][1] *  logd(matInvSqrt(S_w) %*%
                                    covs_ls[[combn[1]]] %*%
                                    matInvSqrt(S_w)) -
          mld_pie[[x]][2] *  logd(matInvSqrt(S_w) %*%
                                    covs_ls[[combn[2]]] %*%
                                    matInvSqrt(S_w))

      ) %*%
      solve(matInvSqrt(S_w))
  }, Sij = Sij, mld_pie = mld_pie, covs_ls = covs_ls, combns = combn, prior = prior, S_w = S_w, S_B = S_B, mld_diff = mld_diff)

  M <- Reduce(`+`, mld_fun)

  svdM <- svd(M)
  D_p <- svdM$d
  D_r <- diag(c(D_p[1:targetDim],rep(0,length(D_p) - targetDim)))
  F_mat <- (svdM$u)%*%D_r
  F_mat[,1:targetDim]
}
