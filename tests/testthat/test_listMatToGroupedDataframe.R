context("listMatToGroupedDataframe")

test_that("listMatToGroupedDataframe produces data frame", {
  expect_equal(is.data.frame(listMatToGroupedDataframe(dataMat, 
                                                       group = "Species")), 
               TRUE)
})