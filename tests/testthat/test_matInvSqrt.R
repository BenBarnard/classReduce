context("matInvSqrt")

test_that("matInvSqrt produces a the matrix inverse squareroot", {
  expect_equal(matInvSqrt(diag(rep(1, 4))), diag(rep(1, 4)))
})