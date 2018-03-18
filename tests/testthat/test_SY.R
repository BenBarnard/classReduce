context("SY")

dataMat <- list(a = matrix(rnorm(10), nrow = 5), 
                b = matrix(rnorm(10), nrow = 5), 
                c = matrix(rnorm(10), nrow = 5))

test_that("SY supplies a list of projected data", {
  expect_equal(is.matrix(SY(dataMat)$projectedData[[1]]), TRUE)
})

test_that("SY supplies a projection matrix", {
  expect_equal(is.matrix(SY(dataMat)$projectionMatrix), TRUE)
})

test_that("SY supplies a M matrix", {
  expect_equal(is.matrix(SY(dataMat)$M), TRUE)
})

test_that("SY supplies the method SY", {
  expect_equal(SY(dataMat)$method, SY)
})
