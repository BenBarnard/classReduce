# Data Simulation File This file contains the function to generate 1 sample of
# multivariate normal data. In: a list of means and a list of covariances, both
# from the Parameter_Configurations file. Out: a list of two data frames - the
# testing and training data frames. Each data frame will have p columns plus an
# additional column containing the true population classification for training
# and comparison.

mvNormData <- function(params, N = 5000, testPCT = 0.995){
  # We expect m populations, so the length of means_ls and covs_ls should be m
  # means_ls is the list of p-dimensional mean vectors
  # covs_ls is the list of p x p covariance matrices
  # N = is the simulation sample size ~ from each population ~ . This should be
  # very large to overcome simulation standard error.
  # testPCT is the percentage of N to be held aside for testing purposes.

  library(MASS)
  library(plyr)
  library(dplyr)

  means_ls <- params$Mu
  covs_ls <- params$Sigma

  # Create a wrapper for mvrnorm
  mvrnorm_wrap <- function(mean, cov){
    data <- MASS::mvrnorm(n = N, mu = mean, Sigma = cov)
    as.data.frame(data)
  }

  # This is a list of m data frames, each of N observations over p columns. We
  # then combine each data frame into one data frame with a label column. Thus,
  # we have a data frame of p + 1 columns and m * N rows
  data_df <- Map(mvrnorm_wrap, means_ls, covs_ls) %>% ldply
  names(data_df)[1] <- "category"

  # Now we split the data frame into testing and training data.
  test_df <- data_df  %>%
                dplyr::group_by(category) %>%
                dplyr::sample_frac(testPCT)  %>%
                as.data.frame
  # This line requires the latest version of dplyr. It errors otherwise.
  train_df <- dplyr::setdiff(data_df, test_df)

  # Return statement
  list(Test = test_df, Train = train_df)
}

##### Testing the Function ####################################################

# Test this function (using "params" object defined at the end of file 1)
# data_ls <- mvNormData(means_ls = params$Mu, covs_ls = params$Sigma)
# test_df <- data_ls$Test
# train_df <- data_ls$Train
# # Remove the original list to save space
# rm(data_ls)
