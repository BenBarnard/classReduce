#' Classification function
#'
#' Given a dimension reduced data set a classifier is built using a selected 
#' discrimination function.
#'
#' @param x data set
#' @param ... currently for use of different dicriminant function than the 
#'            default for the method
#'            
#' @return discriminant function output plus the projection matrix used,
#'         group variable and the method for dimension reduction
#'
#' @export
#' 
classify <- function(x, ...){
  UseMethod("classify")
}

#' @export
#' @keywords internal
#'
#' @importFrom MASS qda
#'
classify.reduced <- function(x, ...){
  
  discrim <- qda

  group <- x$group
  
  classification <- discrimFunc(x$projectedData, discrim, group)

  object <- c(classification, list(projectionMatrix = x$projectionMatrix, group = group, method = x$method))

  discrimClass <- class(classification)

  class(object) <- c("reduced", discrimClass)

  object
}
