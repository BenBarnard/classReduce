classifiedName <- function(named_vec) {
  index <- which(named_vec == min(named_vec))
  vecnames <- (named_vec %>% dimnames)[[2]]
  vecnames[index]
}
