# Reduce based on variance differences
bfDimReduce <- function(df, label, colToKeep = 25) {
  print(Sys.time())
  options("pbapply.pb" = "txt")

  df2 <- df
  names(df2)[names(df2) == label] <- "label"
  columns <- df2 %>%
    dplyr::select(-label) %>%
    names %>%
    as.list()
  df2 <- tbl_df(df2) %>%
    group_by(label)

  bfstat <- pbsapply(columns, function(col) {
    form <- as.formula(paste(col, " ~ label"))
    bfTest <- hov(form, data = df2)$statistic
    names(bfTest) <- paste0(col)
    bfTest
  })
  bfstatKeep <- sort(bfstat, decreasing = TRUE)[1:colToKeep]
  selection <- bfstatKeep %>%
    names
  newDF <- df[c(paste0(label), selection)] %>%
    tbl_df()

  print(Sys.time())
  newDF
}
