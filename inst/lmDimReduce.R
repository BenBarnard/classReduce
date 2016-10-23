# Reduce based on mean differences
lmDimReduce <- function(df, label, colToKeep = 100){

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

  fstat <- pbsapply(columns, function(col) {
    form <- paste(col, " ~ label")
    lm <- lm(form, data = df2, model = FALSE)
    f <- summary(lm)$fstatistic["value"]
    names(f) <- paste0(col)
    f
  })

  fstatKeep <- sort(fstat, decreasing = TRUE)[1:colToKeep]
  selection <- fstatKeep %>%
    names
  newDF <- df[c(paste0(label), selection)] %>%
    tbl_df()

  print(Sys.time())
  newDF
}
