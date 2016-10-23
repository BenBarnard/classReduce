#' Mu vector
#'
#' @param x
#'
#' @export
#'
mu <- function(x){
  n <- lapply(x, nrow)
  total <- Reduce(`+`, n)
  sums <- Reduce(`+`, lapply(x, colSums))
  sapply(sums, function(y){y / total})
}
