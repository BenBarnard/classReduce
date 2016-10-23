#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Haff_shrinkage <- function(x){
  cov <- cov(x)
  invCov <- solve(cov)
  n <- nrow(x)
  p <- ncol(x)
  tu <- tu(cov, n, p)
  (1 - tu) *
    (n - p - 2) *
    invCov + ((tu * (n * p - p - 2)) / tr(cov)) *
    diag(1, p)
}
