#' Priors for Groups
#'
#' @param x list of matrices for groups
#'
#' @keywords internal
prior <- function(x){
  n <- lapply(x, nrow)
  total <- Reduce(`+`, n)
  lapply(n, function(y){y / total})
}


#' Within group covarinace
#'
#' @param prior prior based on group samples
#' @param matrix_ls group sample covarinace matrices
#' 
#' @importFrom stats cov
#'
#' @keywords internal
#'
S_W <- function(prior, matrix_ls){
  Reduce(`+`,
         mapply(function(x, y){
           x * cov(y)
         }, prior, matrix_ls, SIMPLIFY = FALSE)
  )
}

#' Scatter Matrix Between
#'
#' @param prior prior based on sample sizes of groups
#' @param xbar sample means
#' @param mu overall mean 
#'
#' @keywords internal
#'
S_B <- function(prior, xbar){
  xbarbar <- Reduce(`+`,
                    mapply(function(x, y){x * y},
                           prior, xbar, SIMPLIFY = FALSE)
  )
  Reduce(`+`,
         mapply(function(x, y, z){
           (y - z) %*% t(y - z)
         }, prior, xbar, list(xbarbar = xbarbar), SIMPLIFY = FALSE)
  )
}
