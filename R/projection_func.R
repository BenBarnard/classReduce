#' Projection Function
#'
#' @param x data matrix
#' @param proj projection matrix
#'
#' 
projection_func <- function(x, proj){
  t(proj %*% t(x))
}