# Project the test and train data down to the target dimension.

projMatrices <- function(splitData_ls,
                         methods_ls,
                         popColName,
                         targetDim,
                         sample_summary_ls){llply(splitData_ls,
                                                  reduceData,
                                                  methods_ls = methods_ls,
                                                  popColName = popColName,
                                                  targetDim = targetDim,
                                                  summary = sample_summary_ls)
}
