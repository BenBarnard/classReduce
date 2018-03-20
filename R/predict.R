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
  }
  
  reducednewdata <- projection_func(newdata, object$projectionMatrix)
  
  pred <- predict(object, reducednewdata)
  
  if(ind){
    check <- all.equal(as.factor(newdatagroup[[1]]), pred$class)
    cer <- if(is.logical(check)){
      as.numeric(check)
    }else{
      as.numeric(gsub("[A-z]+", "", check)) / length(newdatagroup[[1]])
    }
    pred <- c(pred, cer = cer)
  }
  
  pred
}
