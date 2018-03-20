context("predict")

test_that("predict supplies a cer", {
  expect_equal(predict(classify(reduce(iris, 
                          targetDim = 2, 
                          group = Species,
                          output = "matrix")), iris)$cer, 0.02)
})

test_that("predict supplies a projection matrix", {
  expect_equal(is.matrix(predict(classify(reduce(iris, 
                                       targetDim = 2, 
                                       group = Species,
                                       output = "matrix")), iris[-5])$posterior), TRUE)
})