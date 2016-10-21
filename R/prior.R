#' Priors for Groups
#'
#' @param x list of matrices for groups
#'
#' @export
#'
prior <- function(x){
  n <- lapply(x, nrow)
  total <- Reduce(`+`, n)
  lapply(n, function(y){y / total})
}
