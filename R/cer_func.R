#' Conditional Error Rate Calculator
#'
#' @param ind logical on whether the new data ahd a name column
#' @param newdatagroup new data group
#' @param predictObject prediction object
#' @keywords internal
cer_func <- function(ind, predictObject, newdatagroup){
  if(ind){
    check <- all.equal(as.factor(newdatagroup), predictObject$class)
    cer <- if(is.logical(check)){
      as.numeric(check)
    }else{
      as.numeric(gsub("[A-z]+", "", check)) / length(newdatagroup)
    }
    pred <- c(predictObject, cer = cer)
  }else{
    pred <- predictObject
  }
  pred
}