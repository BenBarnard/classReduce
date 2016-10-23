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
dataDftoMatrixDim <- function(data, group, test, targetDim, svdMethod){
  do.call(what = paste(test),
          args = c(x = dlply(.data = data,
                       .variables = group,
                       .fun = Tidy_,
                       group = group),
                   targetDim = targetDim,
                   group = group,
                   svdMethod = svdMethod
          )
  )
}

#' not tidy helper
#'
#' @param data
#' @param group
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

