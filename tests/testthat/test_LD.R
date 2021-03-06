context("LD")

test_that("LD supplies a list of projected data", {
  expect_equal(is.matrix(LD(dataMat)$projectedData[[1]]), TRUE)
})

test_that("LD supplies a projection matrix", {
  expect_equal(is.matrix(LD(dataMat)$projectionMatrix), TRUE)
})

test_that("LD supplies a M matrix", {
  expect_equal(is.matrix(LD(dataMat)$M), TRUE)
})

test_that("LD supplies the method LD", {
  expect_equal(LD(dataMat)$method, LD)
})