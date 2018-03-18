context("check_targetDimType")

test_that("check_targetDimType throws warning for non-numerics", {
  expect_error(check_targetDimType("This is not a numeric"))
})

test_that("check_targetDimType produces a target dimension for a function", {
  expect_equal(check_targetDimType(targetDim = function(x){x + 1}, x = 3), 4)
})

test_that("check_targetDimType produces a target dimension for a function", {
  expect_equal(check_targetDimType(targetDim = 4, x = 3), 4)
})