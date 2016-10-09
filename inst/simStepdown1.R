###### Create a simulation wrapper ############################################
simStepdown1 <- function(situation, q = 5, listofMethods, simReps){

  # Cheat the seed:
  x <- rpois(1,5000) + 1
  set.seed(x)


  # Generate the data. This creates a training list of 5 (one for each method -
  # Friedman's method) data frames. The training data frames each have 15 rows,
  # while the testing data frame each have 9970 rows. These data frames contain
  # data that have been reduced to 5 dimensions: one data frame per method. The
  # last line of the assignment reorders the columns so that the label column is
  # the first column of the data frame. This is for the next section.
  simData10D <- sim( situation = situation,
                     methods_ls = listofMethods,
                     targetDim = 10,
                     dataOut = TRUE)
  trainData10D <- simData10D$Train
  testData10D <- simData10D$Test

  # Now we use these reduced data frames as our training and testing data frames.
  # We use the "classifyCompare" function from file 6DataClassification.R, but we
  # need a list of data frames for each method first.

  data10d <- list( MLD = rbind.data.frame(testData10D$MLD,   trainData10D$MLD),
                   MSAVE = rbind.data.frame(testData10D$MSAVE, trainData10D$MSAVE),
                   MSIR = rbind.data.frame(testData10D$MSIR,  trainData10D$MSIR),
                   SY = rbind.data.frame(testData10D$SY,    trainData10D$SY),
                   SYS = rbind.data.frame(testData10D$SYS,   trainData10D$SYS)
  )

  reducedDims_ls <- targetDimensionsRed1
  reducedDims_ls$n10 <- 10

  data10D_ls <- llply(names(listofMethods), function(method){
    ldply(reducedDims_ls, function(x){
      pbapply::pbreplicate(simReps,
                           sim(situation = situation,
                               targetDim = x,
                               methods_ls = method,
                               data = data10d[[method]]),
                           simplify = FALSE) %>%
        ldply
    })
  })

  data10D_df <- data10D_ls[[1]][[1]] %>%
    as.data.frame(stringsAsFactors = FALSE)
  burn10D_df <- sapply(data10D_ls, "[", 2) %>%
    llply(as.data.frame) %>%
    do.call(cbind, .)
  data10D_df <- cbind.data.frame(data10D_df,burn10D_df)
  names(data10D_df) <- c("TargetDim",listofMethods)

  data10D_df <- data10D_df %>% melt %>% rename_(PMC_Old = "value")

  PMCSummary10D <- data10D_df %>%
    filter(TargetDim == "n10") %>%
    dplyr::select(PMC_Old) %>%
    as.matrix %>%
    fiveNum()

  data10D_df$Median <- PMCSummary10D["Med"]
  data10D_df <- data10D_df %>% filter(TargetDim %in% c("n1", "n2", "n3", "n4", "n5"))

  # Reset the seed so that we generate the same data twice. This is horribly ugly
  set.seed(x)

  simDataQD <- sim( situation = situation,
                    methods_ls = listofMethods,
                    targetDim = q,
                    dataOut = TRUE)
  trainDataQD <- simDataQD$Train
  testDataQD <- simDataQD$Test

  dataQd <- list(  MLD = rbind.data.frame(testDataQD$MLD,   trainDataQD$MLD),
                   MSAVE = rbind.data.frame(testDataQD$MSAVE, trainDataQD$MSAVE),
                   MSIR = rbind.data.frame(testDataQD$MSIR,  trainDataQD$MSIR),
                   SY = rbind.data.frame(testDataQD$SY,    trainDataQD$SY),
                   SYS = rbind.data.frame(testDataQD$SYS,   trainDataQD$SYS)
  )

  # This returns a list of 5 500 x 2 data frames with columns for dimension
  # (".id") and method, where each data frame is one method. We need to tidy
  # these data frames to combine them.
  data_ls <- llply(names(listofMethods), function(method){
    ldply(targetDimensionsRed1, function(x){
      pbapply::pbreplicate(simReps,
                           sim(situation = situation,
                               targetDim = x,
                               methods_ls = method,
                               data = dataQd[[method]]),
                           simplify = FALSE) %>%
        ldply
    })
  })

  # I'd love a simpler version of this, but I'm out of ideas. I need to rebuild
  # the combineMeltClean (file 8Graphics.R) function to handle this data.

  data_df <- data_ls[[1]][[1]] %>%
    as.data.frame(stringsAsFactors = FALSE)
  burn_df <- sapply(data_ls, "[", 2) %>%
    llply(as.data.frame) %>%
    do.call(cbind, .)
  data_df <- cbind.data.frame(data_df,burn_df)
  names(data_df) <- c("TargetDim",listofMethods)

  data_df <- data_df %>% melt %>% rename_(PMC_New = "value")

  data_df$PMC_Old <- data10D_df$PMC_Old
  data_df$Median <- data10D_df$Median
  data_df %>% mutate(PMC_Diff = PMC_New - PMC_Old)
}
