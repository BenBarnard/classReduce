context("projection_matrix")

dataMat <- list(a = matrix(rnorm(10), nrow = 5), 
                b = matrix(rnorm(10), nrow = 5), 
                c = matrix(rnorm(10), nrow = 5))

test_that("projection_matrix supplies a projection matrix", {
  expect_equal(is.matrix(projection_matrix(dataMat)$projectionMatrix), TRUE)
})

test_that("projection_matrix supplies a M matrix", {
  expect_equal(is.matrix(projection_matrix(dataMat)$M), TRUE)
})

test_that("projection_matrix supplies the method SYS", {
  expect_equal(projection_matrix(dataMat)$method, SYS)
})

test_that("projection_matrix using group = Species still supplies SYS", {
  expect_equal(projection_matrix(iris, group = Species)$method, SYS)
})

test_that("projection_matrix using group = 'Species' still supplies SYS", {
  expect_equal(projection_matrix(iris, group = "Species")$method, SYS)
})
