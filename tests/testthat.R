library(testthat)
library(hldr)
library(dplyr)

dataMat <- list(a = matrix(rnorm(100, -100, 1), nrow = 25), 
                b = matrix(rnorm(100, 0, 1), nrow = 25), 
                c = matrix(rnorm(100, 100, 1), nrow = 25))

test_check("hldr")
