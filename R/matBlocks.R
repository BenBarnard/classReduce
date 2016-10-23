#' Title
#'
#' @param x
#' @param rows
#' @param columns
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
matBlocks <- function(x, rows, columns){
  rg <- (row(x) - 1) %/% rows + 1
  cg <- (col(x) - 1) %/% columns + 1
  rci <- (rg - 1) * max(cg) + cg
  tri <- rci[upper.tri(rci, diag = TRUE)]
  N <- prod(dim(x)) / rows / columns
  cv <- lapply(unique(tri), function(y){
    mat <- x[rci == y]
    dim(mat) <- c(rows, columns)
    mat
    })
  cv
}
