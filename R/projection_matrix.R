#' Projection Matrix
#'
#' Given a linear dimension reduction method projection_matrix parses the data 
#' and uses the specific linear dimension reduction to return the projection 
#' matrix. The projection_matrix function should be used if the user wishes just
#' to produce the projection matrix as opposed to the data reduction matrix.
#'
#' @param x data or reduced class object
#' @param method linear dimension reduction methods
#' @param group the grouping variable in the data
#' @param ... options past to linear dimension reduction methods
#'
#'
#' @export
projection_matrix <- function(x, ...){
  UseMethod("projection_matrix")
}

#' @rdname projection_matrix
#' @export
projection_matrix.reduced <- function(x, ...){
  obj <- x
  
  object <- obj[-which(names(obj) == "projectedData")]
  
  class(object) <- "projection_matrix"
  object
}

#' @rdname projection_matrix
#' @export
projection_matrix.list <- function(x, method = SYS, ...){
  ls <- list(...)
  ls$x <- x
  obj <- do.call(method, ls)
  
  object <- obj[-which(names(obj) == "projectedData")]
  
  class(object) <- "projection_matrix"
  object
}

#' @rdname projection_matrix
#' @export
projection_matrix.default <- function(x, method = SYS, ..., group){
  ls <- list(...)
  dat <- dataToListMatrix(x = dat, group = group)
  ls$x <- dat
  
  obj <- do.call(method, ls)
  
  object <- obj[-which(names(obj) == "projectedData")]
  
  class(object) <- "projection_matrix"
  object
}