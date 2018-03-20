context("predict")

test_that("classify supplies a projection matrix", {
  expect_equal(predict(classify(reduce(iris, 
                          targetDim = 2, 
                          group = Species,
                          output = "matrix")), iris)$cer, 0.02)
})