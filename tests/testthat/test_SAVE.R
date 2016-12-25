context("SAVE")

test_that("SAVE converts data.frame", {
  expect_error(SAVE(iris))
})