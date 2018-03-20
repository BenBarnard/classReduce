#' Discrimination Function Classifier
#'
#' @param dat data
#' @param group grouping variable
#'
#' @keywords internal
discrimFunc <- function(dat, discrim, ...){
  UseMethod("discrimFunc")
}

#' @keywords internal
discrimFunc.data.frame <- function(dat, discrim, group, ...){
  do.call(discrim, list(x = dat[-which(names(dat) == group)],
                        grouping = dat[which(names(dat) == group)][[1]]))
}

#' @keywords internal
discrimFunc.grouped_df <- function(dat, discrim, ...){
  group <- attributes(dat)$vars
  dataUn <- ungroup(dat)
  
  do.call(discrim, list(x = dataUn[-which(names(dataUn) == group)],
                        grouping = dataUn[which(names(dataUn) == group)][[1]]))
}

#' @keywords internal
discrimFunc.list <- function(dat, discrim, group, ...){
  dataUn <- listMatToDataframe(dat, group)
  
  do.call(discrim, list(x = dataUn[-which(names(dataUn) == group)],
                        grouping = dataUn[which(names(dataUn) == group)][[1]]))
}
