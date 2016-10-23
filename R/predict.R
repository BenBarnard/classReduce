#' @export
#' @keywords internal
#'
predict.reduced <- function(object, newdata){
  class(object) <- class(object)[-1]
  ind <- any(names(newdata) %in% paste(object$group))
  if(ind){
    newdatagroup <- select(newdata, eval(object$group))
    newdata <- select(newdata, -eval(object$group))
  }
  reducednewdata <- t(object$projection %*% t(newdata))
  pred <- predict(object, reducednewdata)
  if(ind){
    check <- all.equal(newdatagroup[[1]], pred$class)
    cer <- if(is.logical(check)){
      as.numeric(check)
    }else{
      as.numeric(str_extract(check, "[0-9]+")) / length(newdatagroup[[1]])
    }
    pred <- c(pred,cer = cer)
  }
  pred
}
