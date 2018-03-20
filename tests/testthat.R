library(testthat)
library(hldr)
library(dplyr)

dataMat <- list(a = matrix(rnorm(10), nrow = 5), 
                b = matrix(rnorm(10), nrow = 5), 
                c = matrix(rnorm(10), nrow = 5))

test_check("hldr")
