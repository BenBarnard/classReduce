#' Trace of Matrix
#'
#' Calculates the trace of a square matrix.
#'
#' @param x a matrix
#'
#' @return The trace of a square matrix x.
#' 
#' @keywords internal
tr <- function(x){
  if(!(nrow(x) == ncol(x))){
    stop("Not a square matrix")
    }
  sum(diag(x))
}
