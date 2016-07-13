classify <- function(testData, meansList, invCovsList) {
  n <- ncol(testData)
  df <- testData %>% adply(.margins = 1, .fun = function(x) {
    Mahalanobis(x, meansList = meansList, invCovsList = invCovsList)
  })
  Guess_df <- df[,(n+1):ncol(df)] %>% adply(.margins = 1, .fun = classifiedName)
  Guess <- Guess_df[ncol(Guess_df)]
  names(Guess) <- "Class Guess"
  Guess
}
