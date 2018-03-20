context("SAVE")

test_that("SAVE supplies a list of projected data", {
  expect_equal(is.matrix(SAVE(dataMat)$projectedData[[1]]), TRUE)
})

test_that("SAVE supplies a projection matrix", {
  expect_equal(is.matrix(SAVE(dataMat)$projectionMatrix), TRUE)
})

test_that("SAVE supplies a M matrix", {
  expect_equal(is.matrix(SAVE(dataMat)$M), TRUE)
})

test_that("SAVE supplies the method SAVE", {
  expect_equal(SAVE(dataMat)$method, SAVE)
})
