context("trace")

test_that("Trace equals sum of diagonals of square matrix", {
  expect_equal(tr(diag(rep(1, 5))), 5)

  expect_error(tr(diag(rep(1, 5))[1:4,]))
})
