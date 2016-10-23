###### Invert and Transpose Function ##########################################
# Each of the above methods return a pxr matrix. We need to take the MP Inverse
# of these matrices, and then transpose them. This will yield the rxp projection
# matrices necessary to reduce the dimension of the original data.
projectionMat <- function(F_mat) {
  F_mat %>% MASS::ginv() %>% t
}
