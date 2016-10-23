##### Final Product ###########################################################
# For comments, see GridSearch_over_Parameter_Plane.R
# Because of covsList(), the population label column shouldn't be numeric
paramOptim <- function(trainData, popColLabel,gridpoints = 121,
                       lamMin = 0.00001, lamMax = 1,
                       gamMin = 0.00001, gamMax = 1,
                       Method = 1) {
  print(Sys.time())
  require(dplyr)

  # Add an error if the data frame is empty

  # Create the character vector of the population column name
  popColName <- names(trainData)[popColLabel]

  # The Parameter Vectors
  vecLength <- sqrt(gridpoints) %>% ceiling
  lambdaVec <- seq(lamMin, lamMax, length.out = vecLength)
  gammaVec <- seq(gamMin, gamMax, length.out = vecLength)

  ##### Data Analysis #########################################################
  data_and_covs_ls <- masterPoolFun(trainData, popColName)
  covs_ls <- data_and_covs_ls$Unpooled
  poolCovs_ls <- data_and_covs_ls$YoungPooled
  data_by_pop_ls <- data_and_covs_ls$Data
  dataMeans_ls <- meanVecs(data_by_pop_ls)

  # Because these functions hard-reference covs_ls and poolCovs_ls, we have to
  # build them within this function environment.
  covsRDA <- function(names, lambda, gamma) {
    Ip <- diag(1, nrow = ncol(covs_ls[[names]]))
    Sig_lambda <- (1 - lambda) * covs_ls[[names]] +
      lambda * poolCovs_ls[[names]]
    (1 - gamma) * Sig_lambda +
      gamma * (tr(Sig_lambda) / ncol(Ip)) ^ 2 * Ip
  }

  sigmaHat_ls <- function(lambda, gamma) {
    list <- lapply(names(covs_ls), function(x){
      covsRDA(x, lambda, gamma) %>%
        data.frame %>%
        as.matrix
    })
    names(list) <- names(covs_ls)
    list
  }

  # Calculate the pooled regularized covariances function
  pctCorrectWrap <- function(lambda, gamma) {
    Sigma_ls <- sigmaHat_ls(lambda = lambda, gamma = gamma)
    SigmaInv_ls <- invCovs(Sigma_ls)
    pctCorrect(trainData, popColLabel, dataMeans_ls, SigmaInv_ls)
  }

  # Vectorise this function
  pctCorrectVectWrap <- Vectorize(FUN = pctCorrectWrap,
                                  vectorize.args = c("lambda", "gamma"))

  ##### Parameter Space Grid ##################################################
  #This outer function puts the gamma vector across the top of the matrix, and
  #the lambda vector down the side.
  classification_PCTMat <- outer(X = lambdaVec, Y = gammaVec,
                                 FUN = pctCorrectVectWrap)
  params <- which(max(classification_PCTMat) == classification_PCTMat,
                  arr.ind = TRUE) %>% data.frame()

  # Distance from QDA (0,0)
  params <- params %>%
    mutate(QDADistance = sqrt(row ^ 2 + col ^ 2)) %>%
    arrange(QDADistance)
  QDAparams <- c(lambda = lambdaVec[params[1,1]],
                 gamma = gammaVec[params[1,2]])

  # Distance from LDA (1,0)
  params <- params %>%
    mutate(LDADistance = sqrt((max(row) - row) ^ 2 + col ^ 2)) %>%
    arrange(LDADistance)
  LDAparams <- c(lambda = lambdaVec[params[1,1]],
                 gamma = gammaVec[params[1,2]])

  # Distance from Nearest Means Classifier (1,1)
  params <- params %>%
    mutate(NMCDistance = sqrt((max(row) - row) ^ 2 + (max(col) - col) ^ 2)) %>%
    arrange(NMCDistance)
  NMCparams <- c(lambda = lambdaVec[params[1,1]],
                 gamma = gammaVec[params[1,2]])


  # Diagonalized Quadratic Classifier (0,1)
  params <- params %>%
    mutate(DQCDistance = sqrt(row ^ 2 + (max(col) - col) ^ 2)) %>%
    arrange(DQCDistance)
  DQCparams <- c(lambda = lambdaVec[params[1,1]],
                 gamma = gammaVec[params[1,2]])

  # The parameter data frame
  optParams <- rbind.data.frame(QDAparams, LDAparams, NMCparams, DQCparams)
  colnames(optParams) <- c("Lambda", "Gamma")
  rownames(optParams) <- c("QDA",
                           "LDA",
                           "Nearest_Means_Classifier",
                           "Diagonalized_Quadratic_Classifier")

  ##### The Function Return ###################################################
  #Return the tuning parameter touple, and the list of covariance matrices using
  #this touple.
  paramVec <- optParams[Method,] %>% as.matrix()
  ccpct <- pctCorrectWrap(paramVec[1], paramVec[2])
  InvSigmaHat_ls <- sigmaHat_ls(paramVec[1], paramVec[2]) %>%
    invCovs()

  print(Sys.time())

  list(Parameters = paramVec,
       Prop_classification = ccpct,
       PoolCovars = sigmaHat_ls(paramVec[1], paramVec[2]),
       InvCovariances = InvSigmaHat_ls)
}
