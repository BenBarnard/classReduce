#' Within group covarinace
#'
#' @param prior
#' @param matrix_ls
#' 
#' @importFrom stats cov
#'
#' @export
#'
S_W <- function(prior, matrix_ls){
  Reduce(`+`,
         mapply(function(x, y){
           x * cov(y)
         }, prior, matrix_ls, SIMPLIFY = FALSE)
  )
}
