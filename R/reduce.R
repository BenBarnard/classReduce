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
  ls$x <- x
  obj <- do.call(method, ls)
  
  tarDim <- check_targetDimType(targetDim = targetDim, x = x)
  
  obj$projectionMatrix <- obj$projectionMatrix[1:tarDim,]
  obj$projectedData <- lapply(obj$projectedData, FUN = function(x){x[,1:tarDim]})
  
  class(obj) <- "reduced"
  obj
}

#' @rdname reduce
#' @export
reduce.default <- function(x, method = SYS, targetDim, ..., group){
  ls <- list(...)
  
  gr <- substitute(group)
  if(!(is.character(gr))){
    gr <- deparse(gr)
  }else{
    gr
  }
  
  dat <- dataToListMatrix(x = x, group = gr)
  
  tarDim <- check_targetDimType(targetDim = targetDim, x = x)
  
  ls$x <- dat
  
  obj <- do.call(method, ls)
  
  obj$projectionMatrix <- obj$projectionMatrix[1:tarDim,]
  obj$projectedData <- lapply(obj$projectedData, FUN = function(x){x[,1:tarDim]})
  
  class(obj) <- "reduced"
  obj
}