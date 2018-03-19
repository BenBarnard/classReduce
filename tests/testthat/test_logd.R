context("logd")

test_that("logd produces a the matrix log", {
  expect_equal(logd(diag(rep(1, 4))), diag(rep(0, 4)))
})