invCovs <- function(listOfCovs) {
  listOfCovs %>% plyr::llply(solve)
}
