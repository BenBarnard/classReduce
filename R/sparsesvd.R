#' Title
#'
#' @param x
#'
#' @export
#'
#' @importFrom ssvd ssvd
#'
sparsesvd <- function(x){
  ssvd(x, method = "theory", r = nrow(x))
}
