# And finally calculate the Within Class Scatter Matrix

S_w <- function(...){
  browser()
  Reduce(`+`, lapply(1:length(N), function(population){
    prior[[population]] * S_ls[[population]]
  })
  )
}
