context("projection_func")

test_that("projection_func does required matrix multiplication", {
  expect_equal(projection_func(matrix(c(1, 2, 3), nrow = 1),
                                 matrix(c(1, 2, 3), nrow = 1)), 
               matrix(14, nrow = 1))
})