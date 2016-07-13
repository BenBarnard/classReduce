mld_pie <- llply(combn, function(x, prior){
  c(prior[[x[1]]] / (prior[[x[1]]] + prior[[x[2]]]), prior[[x[2]]] / (prior[[x[1]]] + prior[[x[2]]]))
}, prior = prior)
