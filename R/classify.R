#' Classification function
#'
#' Given a dimension reduced data set a classifier is built using selected discrimination function.
#'
#' @param x data set
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
#' @importFrom dplyr select
#'
classify.reduced <- function(x, ...){

  discrimFunc <- x$discrimFunc

  if(any(class(x) %in% paste(discrimFunc))){stop(cat("Object already Classified"))}

  classification <- do.call(paste(discrimFunc), list(select(x$reducedData, -eval(x$group)),
                                                     select(x$reducedData, eval(x$group))[[1]]))

  object <- c(classification, list(projectionMatrix = x$projectionMatrix, group = x$group))

  discrimClass <- class(classification)

  class(object) <- c("reduced", discrimClass)

  object
}
