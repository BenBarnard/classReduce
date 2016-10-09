# The vector norm (square root of dot product of vector with itself)
# Depricated should use norm(x, type = "F")
vecNorm <- function(x) sqrt(sum(x * t(x)))
