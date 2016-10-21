#' Vector of sample means for the groups
#'
#' @param x
#'
#' @export
#'
xbar <- function(x){
  lapply(x, colMeans)
}
