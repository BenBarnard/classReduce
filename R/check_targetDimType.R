#' Check to verify the targetDim Type and Produce a target dimension
#'
#' @param targetDim function or numeric for producing the target dimension
#' @param x a list of matrices
#' @param ... other options passsed to targetDim functions
#'
#' @keywords internal
check_targetDimType <- function(targetDim, x, ...){
  if(is.function(targetDim)){
    tarDim <- do.call(targetDim, list(x = x))
  }else{
    if(is.numeric(targetDim)){
      tarDim <- targetDim
    }else{
      warning("targetDim is neither a function for how to calculate the target 
              dimension or a numeric for the target dimension")
    }
  }
  tarDim
}