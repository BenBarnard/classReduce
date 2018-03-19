#' Data Conversion to A List of Matrices
#'
#' @param x data not in a list of matrices form
#' @param group The grouping variable in a data.frame in quotations
#' @param ... other options that are not used at the moment
#'
dataToListMatrix <- function(x, ...){
  UseMethod("dataToListMatrix")
}


#' @rdname dataToListMatrix
dataToListMatrix.data.frame <- function(x, group, ...){
  groups <- as.character(unique(x[[group]]))
  mats <- lapply(groups, function(gr){
    as.matrix(x[x[which(names(x) == group)] == gr, -which(names(x) == group)], 
              rownames.force = FALSE)
  })
  names(mats) <- groups
  mats
}

#' @rdname dataToListMatrix
dataToListMatrix.grouped_df <- function(x, ...){
  group <- attributes(x)$vars
  groups <- as.character(unique(x[[group]]))
  mats <- lapply(groups, function(gr){
    as.matrix(x[x[which(names(x) == group)] == gr, -which(names(x) == group)], 
              rownames.force = FALSE)
  })
  names(mats) <- groups
  mats
}