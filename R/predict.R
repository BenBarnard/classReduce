#' @export
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#'
#'
predict.reduced <- function(object, newdata, ..., reduced = TRUE){

  class(object) <- class(object)[-1]
  ind <- any(names(newdata) %in% paste(object$group))
  if(ind){
    newdatagroup <- select(newdata, eval(object$group))
    newdata <- select(newdata, -eval(object$group))
  }
  reducednewdata <- if(reduced == TRUE){
    t(object$projection %*% t(newdata))
  }else{
      newdata
  }
  pred <- predict(object, reducednewdata)
  if(ind){
    check <- all.equal(as.factor(newdatagroup[[1]]), pred$class)
    cer <- if(is.logical(check)){
      as.numeric(check)
    }else{
      as.numeric(str_extract(check, "[0-9]+")) / length(newdatagroup[[1]])
    }
    pred <- c(pred, cer = cer)
  }
  pred
}
