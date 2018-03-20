#' @export
#' @keywords internal
#' @importFrom stats predict
#'
#'
predict.reduced <- function(object, newdata, ..., reduced = TRUE){
  newdata <- as.data.frame(newdata)

  class(object) <- class(object)[-1]
  
  ind <- any(names(newdata) %in% object$group)
  
  if(ind){
    newdatagroup <- newdata[which(names(newdata) == object$group)]
    
    newdata <- newdata[-which(names(newdata) == object$group)]
  }else{
    newdatagroup <- NULL
  }
  
  reducednewdata <- projection_func(newdata, object$projectionMatrix)
  
  pred <- cer_func(ind, predict(object, reducednewdata), newdatagroup[[1]])
  
  pred
}
