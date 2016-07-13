pctCorrect <- function(testData, posOfNameCol, meansList, invCovsList) {
  truePops <- testData[posOfNameCol] %>% sapply(as.character)
  df <- testData[-posOfNameCol]
  predPops <- classify(df, meansList, invCovsList)
  mean(truePops == predPops)
}
