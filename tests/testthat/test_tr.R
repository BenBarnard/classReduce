context("tr")

test_that("tr produces a the trace of a matrix", {
  expect_equal(tr(diag(rep(1, 4))), 4)
})

test_that("tr produces a error for none square matrices", {
  expect_error(tr(diag(rep(1, 4))[1:3,]))
  })