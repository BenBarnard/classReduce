context("check_reduceOutput")

test_that("check_reduceOutput produces list of matrices", {
  expect_equal(is.matrix(check_reduceOutput(dat = dataMat, 
                                            output = "matrix", 
                                            group = "Group")[[1]]), TRUE)
})

test_that("check_reduceOutput produces a data.frame", {
  expect_equal(is.data.frame(check_reduceOutput(dat = dataMat, 
                                            output = "data.frame", 
                                            group = "Group")), TRUE)
})

test_that("check_reduceOutput produces a grouped_df", {
  expect_equal(is.grouped_df(check_reduceOutput(dat = dataMat, 
                                            output = "grouped_df", 
                                            group = "Group")), TRUE)
})

test_that("check_reduceOutput produces a warning", {
  expect_error(check_reduceOutput(dat = dataMat, 
                                  output = "grouped", 
                                  group = "Group"))
})