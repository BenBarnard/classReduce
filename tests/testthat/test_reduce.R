context("reduce")

test_that("reduce supplies a projection matrix", {
  expect_equal(is.matrix(reduce(dataMat, targetDim = 2)$projectionMatrix), TRUE)
})

test_that("reduce supplies a M matrix", {
  expect_equal(is.matrix(reduce(dataMat, targetDim = 2)$M), TRUE)
})

test_that("reduce supplies the method SYS", {
  expect_equal(reduce(dataMat, targetDim = 2)$method, SYS)
})

test_that("reduce using group = Species still supplies SYS", {
  expect_equal(reduce(iris, group = Species, targetDim = 3)$method, SYS)
})

test_that("reduce using group = 'Species' still supplies SYS", {
  expect_equal(reduce(iris, group = "Species", targetDim = 3)$method, SYS)
})