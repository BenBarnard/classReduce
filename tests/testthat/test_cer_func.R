context("cer_func")

newdataGr <- iris[5]

object <- predict(classify(reduce(iris, 
                        targetDim = 2, 
                        group = Species,
                        output = "matrix")), 
        iris[-5])

object2 <- predict(classify(reduce(dataMat, 
                                  targetDim = 2, 
                                  group = "Group",
                                  output = "matrix")), 
                  listMatToDataframe(dataMat, group = "Group")[-5])


test_that("cer_func produces predict object when ind = FALSE", {
  expect_equal(cer_func(ind = FALSE, object, newdataGr), object)
})

test_that("cer_func produces predict object when ind = FALSE", {
  expect_equal(cer_func(ind = FALSE, object2, listMatToDataframe(dataMat, group = "Group")[-5]), object2)
})