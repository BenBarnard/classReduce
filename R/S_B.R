# Then calculate the Between Class Scatter Matrix
S_B <- function(...){

}

S_B <- Reduce(`+`,
              lapply(1:length(xbar_ls), function(population){
                prior[[population]] * (xbar_ls[[population]] - mu) %*%
                  t(xbar_ls[[population]] - mu)
              })
)
