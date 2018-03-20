context("classify")

test_that("classify supplies a projection matrix", {
  expect_equal(is.matrix(classify(reduce(iris, 
                                         targetDim = 2, 
                                         group = Species))$projectionMatrix), 
               TRUE)
})

test_that("classify supplies a projection matrix", {
  expect_equal(is.matrix(classify(reduce(iris, 
                                         targetDim = 2, 
                                         group = Species,
                                         output = "grouped_df"))$projectionMatrix), 
               TRUE)
})

test_that("classify supplies a projection matrix", {
  expect_equal(is.matrix(classify(reduce(iris, 
                                         targetDim = 2, 
                                         group = Species,
                                         output = "matrix"))$projectionMatrix), 
               TRUE)
})