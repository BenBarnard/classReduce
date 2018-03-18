#' Reduction
#' 
#' Given a linear reduction method reduction will produce a data reduction 
#' matrix.
#' 
#' @param x a list of matrices or a projection_matrix class object
#' @param method a function for a linear dimension reduction method
#' @param targetDim a function or numeric that supplies the target dimension
#' @param ... options past to linear dimension reduction and target dimension methods
#' @param group variable in the data the specifies the groupings of the data
#'
#' @export
reduce <- function(x, method, targetDim, ...){
  UseMethod("reduce")
}


#' @rdname reduce
#' @export
reduce.list <- function(x, method = SYS, targetDim, ...){
  ls <- list(...)
  
  
  
  class(object) <- "reduced"
  object
}

#' @rdname reduce
#' @export
reduce.default <- function(x, method = SYS, targetDim, ..., group){
  
  class(object) <- "reduced"
  object
}