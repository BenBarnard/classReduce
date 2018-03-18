context("dataToListMatrix")

test_that("dataToListMatrix.data.frame converts data.frame to a list", {
  expect_equal(is.list(dataToListMatrix(iris, group = "Species")), TRUE)
})

test_that("dataToListMatrix.data.frame converts data.frame to a list with first 
          list item a matrix", {
  expect_equal(is.matrix(dataToListMatrix(iris, group = "Species")[[1]]), TRUE)
})
