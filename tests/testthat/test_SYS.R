context("SYS")

dataMat <- list(a = matrix(rnorm(10), nrow = 5), 
                b = matrix(rnorm(10), nrow = 5), 
                c = matrix(rnorm(10), nrow = 5))

test_that("SYS supplies a list of projected data", {
  expect_equal(is.matrix(SYS(dataMat)$projectedData[[1]]), TRUE)
})

test_that("SYS supplies a projection matrix", {
  expect_equal(is.matrix(SYS(dataMat)$projectionMatrix), TRUE)
})

test_that("SYS supplies a M matrix", {
  expect_equal(is.matrix(SYS(dataMat)$M), TRUE)
})

test_that("SYS supplies the method SYS", {
  expect_equal(SYS(dataMat)$method, SYS)
})

test_that("SYS supplies covariance estimation function", {
  expect_equal(SYS(dataMat)$covEst, Haff_shrinkage)
})