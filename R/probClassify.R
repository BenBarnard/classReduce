# Now we want a data frame of 1 row and 5 columns. Each column will be the
# method, and the row will contain the probability of incorrect classification
# for each method.

probClassify <- function(classifiedData_ls){
  ldply(classifiedData_ls, .fun = function(method){
    1 - (method$Correct %>% sum) / length(method$Correct)
  }) %>% tidyr::spread(.id, V1, fill = FALSE) %>% colSums
}
