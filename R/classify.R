#' CLassification function
#'
#' @param x reduced class object
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
classify.reduced <- function(x, discrimFunc = qda){
  discrimFunc <- expr_find(discrimFunc)
  if(any(class(x) %in% paste(discrimFunc))){stop(cat("Object already Classified"))}
  classification <- do.call(paste(discrimFunc), list(select(x$reducedData, -eval(x$group)),
                                                     select(x$reducedData, eval(x$group))[[1]]))
  object <- c(classification, list(projectionMatrix = x$projectionMatrix, group = x$group))
  discrimClass <- class(classification)
  class(object) <- c("reduced", discrimClass)
  object
}
