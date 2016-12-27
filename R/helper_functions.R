#' Turn no a Tidy data frame into data matrix (helper function)
#'
#' @param data not a tidy dataframe
#'
#' @importFrom plyr dlply
#'
#' @keywords internal
#'
#' @export
#'
dataDftoMatrixDim <- function(data, group, targetDim, method, .dots){
  do.call(what = paste(method),
          args = c(x = dlply(.data = data,
                       .variables = group,
                       .fun = Tidy_,
                       group = group),
                   group = group,
                   targetDim = targetDim,
                   lazy_eval(.dots)
          )
  )
}

#' not tidy helper
#'
#' @param data data
#' @param group grouping variable
#'
#' @importFrom dplyr select
#'
#' @keywords internal
#'
#' @export
#'
Tidy_ <- function(data, group){
  as.matrix(select(data, -eval(group)))
}

