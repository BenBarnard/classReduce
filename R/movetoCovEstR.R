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

#' Haff Shrinkage Precision Estimator
#'
#' @param x data matrix
#' @param ... other options (currently unused)
#'
#' @return Haff shrinkage precision estimator
#'
#' @details Given a matrix of observations, this function will calculate the
#'    Haff Shrinkage Estimator of the sample precision matrix, as discussed in
#'    \href{https://projecteuclid.org/euclid.aos/1176344845}{Haff (1979)}.
#'    Because this estimator relies on the existence of the sample precision
#'    matrix (and relatedly, a strictly positive covariance determinant), this
#'    estimator is ill-suited for high-dimensional cases (\eqn{N < p}).
#'
#' @export
#'
#' @importFrom stats cov
#'
#' @examples Haff_shrinkage(as.matrix(iris[-5]))
Haff_shrinkage <- function(x, ...){
  dots <- list(...)
  cov <- stats::cov(x)
  invCov <- solve(cov)
  n <- nrow(x)
  p <- ncol(x)
  tu <- tu(cov, n, p)
  (1 - tu) *
    (n - p - 2) *
    invCov + ((tu * (n * p - p - 2)) / tr(cov)) *
    diag(1, p)
}
