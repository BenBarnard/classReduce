# The cosine angle metric for two vectors
cosineMetric <- function(x,y){
  abs(1 - sum(x * t(y)) / (vecNorm(x) * vecNorm(y)))
}
