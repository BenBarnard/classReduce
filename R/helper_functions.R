#' Turn Tidy data frame into data matrix (helper function)
#'
#' @param data tidy dataframe
#'
#' @importFrom plyr dlply
#'
#' @keywords internal
#'
#' @export
#'
tidyDataDftoMatrixDim <- function(data, group, variables, samples, value, test, targetDim, svdMethod){
  do.call(what = paste(test),
          args = c(x = dlply(.data = data,
                       .variables = group,
                       .fun = tidy_,
                       group = group,
                       samples = samples,
                       variables = variables,
                       value = value),
                       targetDim = targetDim,
                       svdMethod = svdMethod

          )
  )
}

#' Tidy helper
#'
#' @param data
#' @param group
#' @param variables
#' @param samples
#' @param value
#'
#' @importFrom reshape2 acast
#' @importFrom dplyr select
#'
#' @keywords internal
#'
#' @export
#'
tidy_ <- function(data, group, variables, samples, value){
  as.matrix(acast(select(data,
               -eval(group)),
        eval(samples)~eval(variables),
        value.var = paste(eval(value))))
}

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
dataDftoMatrixDim <- function(data, group, test){
  do.call(what = paste(test),
          args = c(x = dlply(.data = data,
                       .variables = group,
                       .fun = notTidy_,
                       group = group),
                   targetDim = targetDim,
                   svdMethod = svdMehtod
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
notTidy_ <- function(data, group){
  as.matrix(select(data, -eval(group)))
}

