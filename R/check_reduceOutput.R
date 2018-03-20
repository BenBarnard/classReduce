#' Check The Reduce Output and Output a matrix, data.frame, or grouped_df
#'
#' @param dat data
#' @param output output type either matrix, data.frame, or grouped_df
#' @param group grouping variable name
#'
#' @keywords internal
check_reduceOutput <- function(dat, output, group){
  if(output == "matrix"){
    obj <- dat
  }else{
    if(output == "data.frame"){
      obj <- listMatToDataframe(dat, group = group)
    }else{
      if(output == "grouped_df"){
        obj <- listMatToGroupedDataframe(dat, group = group)
      }else{
        stop("output type reqested is not matrix, data.frame, or grouped_df")
      }
    }
  }
}