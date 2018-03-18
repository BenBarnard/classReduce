#' Projection Function
#'
#' @param x data matrix
#' @param proj projection matrix
#'
#' @export
#' 
projection_func <- function(x, proj){
  t(proj %*% t(x))
}