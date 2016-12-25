#' Scatter Matrix Between
#'
#' @param prior prior based on sample sizes of groups
#' @param xbar sample means
#' @param mu overall mean 
#'
#' @export
#' @keywords internal
#'
S_B <- function(prior, xbar){
  xbarbar <- Reduce(`+`,
                    mapply(function(x, y){x * y},
                           prior, xbar, SIMPLIFY = FALSE)
                    )
  Reduce(`+`,
         mapply(function(x, y, z){
           (y - z) %*% t(y - z)
         }, prior, xbar, list(xbarbar = xbarbar), SIMPLIFY = FALSE)
  )
}


