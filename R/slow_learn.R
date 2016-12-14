#' Slow Learning Algorithm
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples slow_learn(iris, group = Species, loss = conditional_loss, lossValue = .1, method = SYS)
slow_learn <- function(x, ...){
  UseMethod("slow_learn")
}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr select
#' @importFrom dplyr group_by_
#'
slow_learn.data.frame <- function(x, group, loss, lossValue, method, ...){
  x <- group_by_(x, expr_find(group))
  ls <- lazy_dots(...)
  do.call(slow_learn.grouped_df, c(list(x = x, loss = loss,
                                        lossValue = lossValue,
                                        method = method),
                                   lazy_eval(ls)))

}

#' @keywords internal
#' @export
#'
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom dplyr ungroup
#'
slow_learn.grouped_df <- function(x, loss, lossValue, method, ...){
  ls <- lazy_dots(...)
Browser()
  M <- do.call(method, c(list(x = x,
                         targetDim = ncol(select(ungroup(x), -eval(attributes(x)$vars[[1]]))) - 1),
                    lazy_eval(ls)))$M


  while(energyTotal > 1 - lossValue){
    browser()


    reduced <- do.call(method, c(list(x = x,
                           targetDim = ncol(select(ungroup(x), -eval(attributes(x)$vars[[1]]))) - 1),
                      lazy_eval(ls)))




  }
}
