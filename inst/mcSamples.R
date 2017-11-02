#' Simulate Monte Carlo Samples from a Multivariate Normal Distribution
#'
#' @param meanVec Numeric vector of means for the variables.
#' @param covMat Positive-definite symmetric matrix of the variables.
#' @param samples number of samples
#' @param pops number of populations
#' @param ... Other variables used in mvnorm function in the mass package.
#' @param matrix whether data should be in data frame or matrix
#'
#' @return Matrix of maxN multivariate normal samples.
#'
#' @importFrom plyr ldply
#' @importFrom plyr llply
#' @importFrom MASS mvrnorm
#' @importFrom reshape2 melt
#'
#' @keywords internal
#' @export
#'
mcSamples <- function(meanVec, covMat, samples, pops, ..., matrix = FALSE){
  pop_list <- pop_lists(meanVec, covMat, samples, pops)
  if(matrix == TRUE){
    llply(pop_list, function(list){
      mvrnorm(n = list$samples, mu = list$meanVec, Sigma = list$covMat)
    })
  }else{
      ldply(pop_list, function(list){
        mvrnorm(n = list$samples, mu = list$meanVec, Sigma = list$covMat)
      }, .id = "population")
  }
}

#' List population parameters
#'
#' @param meanVec vector of means
#' @param covMat covariance matrices
#' @param samples number of samples
#' @param pops number of populations
#'
#' @importFrom plyr llply
#' @importFrom stats setNames
#'
#' @keywords internal
#'
#' @export
#'
pop_lists <- function(meanVec, covMat, samples, pops){
  params <- llply(seq(pops), function(pop, meanVec, covMat, samples){
    list(meanVec = if(is.list(meanVec)){meanVec[[pop]]}else{meanVec},
         covMat = if(is.list(covMat)){covMat[[pop]]}else{covMat},
         samples = if(is.list(samples)){samples[[pop]]}else{samples})
  }, meanVec = meanVec, covMat = covMat, samples = samples)
  setNames(params, 1:length(params))
}

