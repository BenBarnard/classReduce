#' Within group covarinace
#'
#' @param prior prior based on group samples
#' @param matrix_ls group sample covarinace matrices
#' 
#' @importFrom stats cov
#'
#' @keywords internal
#' @export
#'
S_W <- function(prior, matrix_ls){
  Reduce(`+`,
         mapply(function(x, y){
           x * cov(y)
         }, prior, matrix_ls, SIMPLIFY = FALSE)
  )
}
