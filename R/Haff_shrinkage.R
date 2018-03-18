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
#' @importFrom lazyeval lazy_dots
#' @importFrom stats cov
#'
#' @examples Haff_shrinkage(as.matrix(iris[-5]))
Haff_shrinkage <- function(x, ...){
  dots <- lazy_dots(...)
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