context("SIR")

test_that("SIR supplies a list of projected data", {
  expect_equal(is.matrix(SIR(dataMat)$projectedData[[1]]), TRUE)
})

test_that("SIR supplies a projection matrix", {
  expect_equal(is.matrix(SIR(dataMat)$projectionMatrix), TRUE)
})

test_that("SIR supplies a M matrix", {
  expect_equal(is.matrix(SIR(dataMat)$M), TRUE)
})

test_that("SIR supplies the method SIR", {
  expect_equal(SIR(dataMat)$method, SIR)
})
