# VOM: measures the differences between to covariance matrices based on
# hypervolume and eigenvector orientation
vom <- function(A,B){
  svdA <- svd(A); svdB <- svd(B)
  lambdaA <- svdA$d; lambdaB <- svdB$d
  UA <- split(svdA$u, col(svdA$u)); UB <- split(svdB$u, col(svdB$u))
  metric_vec <- mapply(cosineMetric, x = UA, y = UB, USE.NAMES = FALSE)
  eigwtA <- lambdaA / sum(lambdaA); eigwtB <- lambdaB / sum(lambdaB)

  cosineMetric <- (eigwtA + eigwtB)%*%metric_vec
  sizeMetric <- (2 / sqrt(sum(diag(A + B)))) * vecNorm(lambdaA - lambdaB)

  # This gives equal weight to the orientation metric and the size metric. We
  # care more about the orientation metric. Dr. Young reccomends a 2/3 to 1/3
  # weight.
  (cosineMetric + sizeMetric) / (length(lambdaA))
}
