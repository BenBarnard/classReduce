##### Master Pooling Function #################################################
#This combines all the previous pooling functions into one monster. All you have
#to enter is the screened data frame and the name of the population column. This
#returns a list of two lists: the Pooled class covariance matrices and the
#Unpooled class covariance matrices. I am very proud of this function.
# See Cosine_Metric_Tests.R for the commented version

masterPoolFun <- function(screened_df, popColName) {
  # 1. Block Data and Covariances
  DataandCovs_ls <- covsList(screened_df, popColName)
  covs_ls <- DataandCovs_ls$PopCovs

  # 2. Calculate the n vector.
  dim_mat <- sapply(1:length(DataandCovs_ls$PopData), function(i) {
    dim(DataandCovs_ls$PopData[[i]])
  })
  n <- dim_mat[1,]

  # 3. Make a data frame of all the combinations of names
  library(gtools)
  population_combos <- data.frame(combinations(n = length(names(covs_ls)),
                                               r = 2, v = names(covs_ls),
                                               repeats.allowed = FALSE))
  names(population_combos) <- c("Pop1", "Pop2")

  # 4. Add the Delta column of VOM distances
  population_combos$Delta <- sapply(1:nrow(population_combos), function(i){
    vom(covs_ls[[as.character(population_combos[i,1])]],
        covs_ls[[as.character(population_combos[i,2])]])
  })

  # 5. Add the inverse distance (i.e. weight) column
  # We originally had the exponential base at 1.15
  population_combos <- mutate(.data = population_combos,
                              Inv_Dist = (1 + 0.50) ^ -Delta)

  # 6. Create the Weight Matrix The Young Weight Matrix will not take sample
  # size into account, as this information will overpower our VOM semi-metric.
  weight <- symmetricMat(population_combos,
                         "Pop1", "Pop2", "Inv_Dist", names(covs_ls))
  friedmanWeight <- (n - 1) * matrix(1,
                                     nrow = length(names(covs_ls)),
                                     ncol = length(names(covs_ls)))

  # 7. Calculate the pooled covariance matrices
  poolCovs_ls <- allPoolCov(weight, covs_ls)
  friedmanPoolCovs_ls <- allPoolCov(friedmanWeight, covs_ls)

  # 8. Return the pooled and unpooled covariances
  list(Data = DataandCovs_ls$PopData,
       Weight = weight,
       FriedWeight = friedmanWeight,
       YoungPooled = poolCovs_ls,
       FriedmanPooled = friedmanPoolCovs_ls,
       Unpooled = covs_ls)
}
