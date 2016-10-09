#' Trace of Matrix
#'
#' Calculates the trace of a square matrix.
#'
#' @param x a matrix
#'
#' @return The trace of a square matrix x.
#' @export
#'
#' @examples tr(diag(rep(1, 5)))
tr <- function(x){
  if(!(nrow(x) == ncol(x))){
    stop("Not a square matrix")
    }
  sum(diag(x))
}
