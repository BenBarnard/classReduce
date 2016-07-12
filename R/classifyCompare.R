# Apply the QDA classifier to the training data frame for each dimension
# reduction method. This function takes in each dimension reduction method and
# the projected testing and training data frames, then builds a classifier for
# each method. Next, this function classifies each observation from the testing
# data. Finally, this function returns a data frame of the true and predicted
# classification values for each method, as a list.
classifyCompare <- function(projData_ls, methods_ls, targetDim){
  require(MASS)
  require(dplyr)
  # projData_ls is the list of training and testing data frames projected into
  # some smaller dimension.
  # methods_ls is the classification method
  # targetDim should have been named just dimension. It marks how many columns
  # of the testing a training data frames are values. The first column is
  # assumed to be the classifier column.
  methods_ls <- setNames(as.list(names(projData_ls[[1]])), names(projData_ls[[1]]))
  llply(methods_ls, function(x, targetDim, projData_ls){
    train <- projData_ls$Train[[x]]
    test <- projData_ls$Test[[x]]
    classifier <- MASS::qda(as.matrix(train[2:(targetDim + 1)]),
                            as.factor(as.matrix(train[1])))
    Predict <- predict(classifier, as.matrix(test[2:(targetDim + 1)]))$class
    data.frame(True = test[[1]],
               Predict = Predict) %>%
      mutate(Correct = (True == Predict))
  }, targetDim = targetDim, projData_ls = projData_ls)
}
