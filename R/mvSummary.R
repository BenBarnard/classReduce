# The Sample Estimates Calculator This file takes in a data frame with p
# parameters and a population label column, splits the data frame into m data
# frames by population, calculates the sample mean vector, sample covariance
# matrix, sample inverse covariance matrix (should the design matrix be full
# rank), and the Friedman RDA pooled covariance estimate (minimising
# cross-validated error and defaulting to QDA parameters). Finally, this
# function returns 4 lists: a list of sample mean vectors, a list of sample
# covariances, a list ov inverse covariances, and a list of RDA pooled inverse
# covariances.

mvSummary <- function(train_df, PopColName, methods_ls){
  # Find the covariances
  # masterPoolFun is listed in Functions_only.R
    summary_ls <- masterPoolFun(train_df, PopColName)
    data_ls <- summary_ls$Data

  # Find the Cross-Validated optimised RDA covariances and inverses. Because of
  # my limited coding ability. This portion of the code will run slowly. We are
  # taking 121 covariances and cross validating them to minimise the proabbility
  # of misclassification. This takes time. Moreover, we have to passs the
  # location of the "category" column, instead of the name of the column itself.
    popColLabel <- which(names(train_df) == PopColName)
    if("NewSY" %in% names(methods_ls)){
    friedman <- paramOptim(train_df, popColLabel) # Runs in 8 seconds for N=75
    alphaLambda <- friedman$Parameters
    friedmanPooledCovs_ls <- friedman$PoolCovars
    friedmanInv_ls <- friedman$InvCovariances
    }
  # The Population covariances and their inverses
    S_ls <- summary_ls$Unpooled
    SInv_ls <- lapply(S_ls, solve)

  # Calculate the mean vectors. This function ignores the character column.
  # meanVecs is listed in Functions_only.R

    N <- data_ls %>% lapply(nrow)
    mu <- train_df[,-popColLabel] %>% colMeans
    xbar_ls <- data_ls %>% meanVecs()

  # Calculate the Within and Between Class Scatter Matrices. These are necessary
  # for the Loog and Duin, SIR, and SAVE methods.
  # First, calculate the a priori probabilities:
    prior <- lapply(1:length(N), function(population) {
      N[[population]] / Reduce(`+`,N)
    })
  # Then calculate the Between Class Scatter Matrix
    S_B <- Reduce(`+`,
                  lapply(1:length(xbar_ls), function(population){
                    prior[[population]] * (xbar_ls[[population]] - mu) %*%
          t(xbar_ls[[population]] - mu)
      })
      )

  # And finally calculate the Within Class Scatter Matrix
    S_w <- Reduce(`+`, lapply(1:length(N), function(population){
      prior[[population]] * S_ls[[population]]
    }))


    combn  <- combn(1:length(data_ls), 2, simplify = FALSE)

    mld_diffmean2 <- llply(combn, function(x, xbar_ls){
      (xbar_ls[[x[1]]] - xbar_ls[[x[2]]]) %*% t(xbar_ls[[x[1]]] - xbar_ls[[x[2]]])
    }, xbar_ls = xbar_ls)

    mld_pie <- llply(combn, function(x, prior){
      c(prior[[x[1]]] / (prior[[x[1]]] + prior[[x[2]]]), prior[[x[2]]] / (prior[[x[1]]] + prior[[x[2]]]))
    }, prior = prior)

  # Return the lists
    if("NewSY" %in% names(methods_ls)){
    list(N = N,
         Combns = combn,
         MLD_diff = mld_diffmean2,
         MLD_pie = mld_pie,
         Priors = prior,
         GrandMean = mu,
         xBars = xbar_ls,
         S = S_ls,
         S_B = S_B,
         S_W = S_w,
         SInv = SInv_ls,
         Sigma = friedmanPooledCovs_ls,
         SigmaInv = friedmanInv_ls
    )}else{
      list(N = N,
           Combns = combn,
           MLD_diff = mld_diffmean2,
           MLD_pie = mld_pie,
           Priors = prior,
           GrandMean = mu,
           xBars = xbar_ls,
           S = S_ls,
           S_B = S_B,
           S_W = S_w,
           SInv = SInv_ls
      )
    }

}

###### Testing the Function ###################################################
# NOTE: THIS FUNCTION TAKES ABOUT 12 MINUTES TO RUN.
#sample_summary_ls <- mvSummary(train_df, "category")
