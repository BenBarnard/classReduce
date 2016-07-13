mld_diffmean2 <- llply(combn, function(x, xbar_ls){
  (xbar_ls[[x[1]]] - xbar_ls[[x[2]]]) %*% t(xbar_ls[[x[1]]] - xbar_ls[[x[2]]])
}, xbar_ls = xbar_ls)
