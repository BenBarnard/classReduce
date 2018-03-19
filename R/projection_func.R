#' Projection Function
#'
#' @param x data matrix
#' @param proj projection matrix
#' @keywords internal
#' 
projection_func <- function(x, proj){
  t(proj %*% t(x))
}