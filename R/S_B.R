#' Title
#'
#' @param prior
#' @param xbar
#' @param mu
#'
#' @export
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


