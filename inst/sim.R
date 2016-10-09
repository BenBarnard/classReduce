sim <- function(situation = 1,
                targetDim,
                methods_ls = 'ALL',
                N = 5000,
                testPCT = 0.997, # gives us 15 observations to train with
                data = NULL,
                dataOut = FALSE){
  # This function takes in the parameter configuration via the situation
  # argument. These situations are defined in 1Parameter_Configurations.R. The
  # targetDim argument allows the user to specify to which dimension the data
  # should be reduced to. For our simulation study, we apply this function
  # across all dimensions <= the original data dimension (10). The methods_ls
  # argument allows the user to choose from the following dimension reduction
  # methods: Loog and Duin, Sliced Inverse Regression (MSIR), Sliced Average
  # Variance Regression (MSAVE), and the three methods put forth by Drs. Young.
  # As a default, all methods are used. N is the number of multivariate
  # observations to generate PER POPULATION. The number of populations are
  # specified by the number of covariances matrices and mean vectors passed into
  # the function through the situation configuration. (QUESTION: CAN WE HAVE
  # MORE THAN 3 POPULATIONS?). The testPCT argument sets which proportion of the
  # N data points will be used to test the classifier. Because we are interested
  # in testing unstable covariance inverses (from poorly posed scenarios where n
  # is only slightly larger than p), this testing percentage defaults to a very
  # large percentage. Thus the classifier will be built with (1 - testPCT)% of
  # the data, ensuring small training data sets. Finally, this function can take
  # in user specified data to classify, and will return the best
  # (cross-validated) classifier. Of note, when the user provides data, the
  # testPCT argument should be dropped to 15%-20%.
  if(is.null(data)){
    # If the user did not provide data, this will generate testing and training
    # data frames, and store them as a list
  data_ls <- situation %>% mvParamConfig %>% mvNormData(N, testPCT)
  }else{
    # This code handles user-supplied data
    if(is.list(data) & length(data) == 2){
      data_ls <- data
      }else{
    test_df <- data  %>%
      dplyr::group_by(category) %>%
      dplyr::sample_frac(testPCT)  %>%
      as.data.frame
    # This line requires the latest version of dplyr. It errors otherwise.
    train_df <- dplyr::setdiff(data, test_df)

    # Return statement
    data_ls <- list(Test = test_df, Train = train_df)
    }
  }

  # This specfies the set of all methods, allowing R to use the user's choice of
  # methods, if supplied.
  methods_ls_All <- list(SY = quote(SY),
                         SYS   = quote(SYS),
                      #  NewSY = quote(newSY),
                         MLD   = quote(MLD),
                         MSIR  = quote(MSIR),
                         MSAVE = quote(MSAVE))

  if(any(methods_ls == "ALL")){
    methods_ls <- methods_ls_All
  }else{
    methods_ls <- methods_ls_All[methods_ls_All %in% methods_ls]
  }

  # This is the actual simulation bit. It takes 7 seconds to run. Thus,
  # repeating it 100 times will take about 11 minutes and 40 seconds. The entire
  # simulation (across each of the 9 target dimensions) will take approximately
  # two hours
  popColName <- "category"

  sample_summary_ls <- mvSummary(data_ls$Train, popColName, methods_ls)
  if(dataOut == FALSE){
    out <- data_ls %>%
    projMatrices(methods_ls,
                 popColName,
                 targetDim,
                 sample_summary_ls) %>%
    classifyCompare(methods_ls = methods_ls,
                    targetDim) %>%
     probClassify
  }

  if(dataOut == TRUE){
    out <- data_ls %>%
      projMatrices(methods_ls,
                   popColName,
                   targetDim,
                   sample_summary_ls)
  }
  out
}

###############################################################################
# Simulations for each parameter configuratiom
targetDimensions <- list(n1 = 1,
                         n2 = 2,
                         n3 = 3,
                         n4 = 4,
                         n5 = 5,
                         n6 = 6,
                         n7 = 7,
                         n8 = 8,
                         n9 = 9,
                         n10 = 10)
#

# # This takes roughly 11 minutes per target dimension.
#
# The probabilities for situations 1 - 15
# probsSit1 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(1, x), simplify = FALSE) %>% ldply
# })
#
# probsSit2 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(2, x), simplify = FALSE) %>% ldply
# })
#
# probsSit3 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(3, x), simplify = FALSE) %>% ldply
# })
#
# probsSit4 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(4, x), simplify = FALSE) %>% ldply
# })
#
# probsSit5 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(5, x), simplify = FALSE) %>% ldply
# })
#
# probsSit6 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(6, x), simplify = FALSE) %>% ldply
# })
#
# # probsSit7 <- llply(targetDimensions, function(x){
# #   pbapply::pbreplicate(100, sim(7, x), simplify = FALSE) %>% ldply
# # })
#
# probsSit8 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(8, x), simplify = FALSE) %>% ldply
# })
#
# probsSit9 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(9, x), simplify = FALSE) %>% ldply
# })
#
# probsSit10 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(10, x), simplify = FALSE) %>% ldply
# })
#
# probsSit11 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(11, x), simplify = FALSE) %>% ldply
# })
#
# probsSit12 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(12, x), simplify = FALSE) %>% ldply
# })
#
# probsSit13 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(13, x), simplify = FALSE) %>% ldply
# })
#
# probsSit14 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(14, x), simplify = FALSE) %>% ldply
# })
#
# probsSit15 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(15, x), simplify = FALSE) %>% ldply
# })

# probsSit16 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(16, x), simplify = FALSE) %>% ldply
# })
#
# probsSit17 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(17, x), simplify = FALSE) %>% ldply
# })
#
# probsSit18 <- llply(targetDimensions, function(x){
#   pbapply::pbreplicate(100, sim(18, x), simplify = FALSE) %>% ldply
# })
