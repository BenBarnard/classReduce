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
#' @importFrom dplyr select_
#'
classify.reduced <- function(x, ...){

  discrimFunc <- x$discrimFunc

  if(any(class(x) %in% paste(discrimFunc))){stop(cat("Object already Classified"))}

  group <- x$group

  classification <- do.call(paste(discrimFunc), list(select(ungroup(x$reducedData), -eval(group)),
                                                     select_(x$reducedData, group)[[1]]))

  object <- c(classification, list(projectionMatrix = x$projectionMatrix, group = group, test = x$test))

  discrimClass <- class(classification)

  class(object) <- c("reduced", discrimClass)

  object
}
