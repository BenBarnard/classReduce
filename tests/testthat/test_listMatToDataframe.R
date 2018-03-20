context("listMatToDataframe")

test_that("listMatToDataframe produces data frame", {
  expect_equal(
    is.data.frame(listMatToDataframe(dataMat, group = "Species")), 
    TRUE)
})