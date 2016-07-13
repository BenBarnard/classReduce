# This is my old version. This works, but doesn't guarantee at least one
# observation from each population. However, until we can figure out why
# Whitney's testAndTrain is giving my paramOptim function fits, I'm going back
# to using this one.
testAndTrain <- function(screen_df, popColName, proportion = 0.2) {
  screen_df["Observation"] <- 1:nrow(screen_df)
  popList <- dlply(screen_df, .variables = popColName, .fun = function(x) x)
  n <- popList %>% names %>% length()
  test_ls <- list()
  for(i in 1:n){
    test_ls[[i]] <- popList[[i]] %>% sample_frac(0.2)
  }
  test_df <- test_ls %>% ldply(.fun = data.frame)
  # dplyr::setdiff doesn't work. I'm pissed about that.
  train_df <- test_df %>% base::setdiff(screen_df, .)
  test_df["Observation"] <- NULL
  train_df["Observation"] <- NULL
  list(Test = test_df, Train = train_df)
}
