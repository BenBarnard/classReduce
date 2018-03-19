context("reduce")

dataMat <- list(a = matrix(rnorm(20), nrow = 5), 
                b = matrix(rnorm(20), nrow = 5), 
                c = matrix(rnorm(20), nrow = 5))

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
  expect_equal(projection_matrix(iris, group = Species)$method, SYS)
})

test_that("reduce using group = 'Species' still supplies SYS", {
  expect_equal(projection_matrix(iris, group = "Species")$method, SYS)
})