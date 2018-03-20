#' List of Matrices to Data Frame
#'
#' @keywords internal
listMatToDataframe <- function(x, group){
  Reduce(rbind, mapply(function(dat, nam, gr){
    frame <- as.data.frame(dat)
    frNames <- names(frame)
    frNames <- c(frNames, gr)
    grFrame <- cbind(frame, data.frame(gr = nam, stringsAsFactors = FALSE))
    names(grFrame) <- frNames
    grFrame
  }, dat = x, nam = names(x), MoreArgs = list(gr = group), SIMPLIFY = FALSE))
}

#' List of Matrices to Grouped Data Frame
#'
#' @keywords internal
listMatToGroupedDataframe <- function(x, group){
  grouped_df(Reduce(rbind, mapply(function(dat, nam, gr){
    frame <- as.data.frame(dat)
    frNames <- names(frame)
    frNames <- c(frNames, gr)
    grFrame <- cbind(frame, data.frame(gr = nam, stringsAsFactors = FALSE))
    names(grFrame) <- frNames
    grFrame
  }, dat = x, nam = names(x), MoreArgs = list(gr = group), SIMPLIFY = FALSE)), 
  group)
}

