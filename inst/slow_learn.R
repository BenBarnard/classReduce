#' Slow Learning Algorithm
#'
#' @inheritParams SYS
#' @param loss loss function
#' @param totallossValue total loss attained with overall reduction
#' @param conditionallossValue loss attained for each dimension reduction
#' @param method method used in slow learn algorithm
#'
#' @return list of reduced data, projection matrix, 
#'          group variable, discrimination function, 
#'          m matrix and dimension reduction method used.
#' @export
#'
#' @examples slow_learn(iris, group = Species, loss = conditional_loss, 
#'           totallossValue = .1, conditionallossValue = .01, method = SYS)
slow_learn <- function(x, ...){
  UseMethod("slow_learn")
}

#' @rdname slow_learn
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr select
#' @importFrom dplyr group_by_
slow_learn.data.frame <- function(x, group, loss = conditional_loss, totallossValue, 
                                  conditionallossValue, method, ...){
  x <- group_by_(x, expr_find(group))
  ls <- lazy_dots(...)
  do.call(slow_learn.grouped_df, c(list(x = x, loss = loss,
                                        totallossValue = totallossValue,
                                        conditionallossValue = conditionallossValue,
                                        method = method),
                                   lazy_eval(ls)))

}

#' @rdname slow_learn
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom dplyr ungroup
slow_learn.grouped_df <- function(x, loss = conditional_loss, totallossValue,
                                  conditionallossValue,
                                  method, ...){
  ls <- lazy_dots(...)

  reduced <- do.call(method,
                     c(list(x = x,
                            targetDim = ncol(x) - 1),
                       lazy_eval(ls)))
  reducedData_ <- x

  values <- svd(reduced$M)$d
  energy <- cumsum(values) / sum(values)
  tarDim <- length(energy) -
    max(length(energy) - min(which(energy >= (1 - conditionallossValue))), 1)

  energyTotal_ <- 0
  projection_ <- diag(1, nrow = ncol(x) - 1)
  conditionalEnergy <- integer(0)
  iter <- 1

  while(energyTotal_ < totallossValue){

    reducedData <- reducedData_
    energyTotal <- energyTotal_
    projection <- projection_

    if(tarDim == 0){break}

    conditionalEnergy[iter] <- 1 - energy[tarDim]

    reducedData_ <- reduced$reducedData[, c(1:tarDim, ncol(reduced$reducedData))]

    projection_ <- projection %*% reduced$projectionMatrix[, 1:tarDim]

    reduced <- do.call(method, c(list(x = reducedData_,
                                      targetDim = ncol(reducedData_) - 1),
                                 lazy_eval(ls)))

    values <- svd(reduced$M)$d
    energy <- cumsum(values) / sum(values)

    tarDim <- length(energy) -
      max(length(energy) - min(which(energy >= (1 - conditionallossValue))), 1)

    energyTotal_ <- do.call(loss, list(conditionalEnergy))
    iter <- iter + 1
  }

  object <- list(reducedData = reducedData,
                 projectionMatrix = t(projection),
                 group = reduced$group,
                 discrimFunc = reduced$discrimFunc,
                 energyTotal = energyTotal)
  class(object) <- "reduced"
  object
}

#' @rdname slow_learn
#' @export
#' @importFrom lazyeval expr_find
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom dplyr ungroup
slow_learn.resample <- function(x, loss = conditional_loss, totallossValue,
                                  conditionallossValue,
                                  method, ...){
  ls <- lazy_dots(...)
  x <- as.data.frame(x)
  reduced <- do.call(method,
                     c(list(x = x,
                            targetDim = ncol(x) - 1),
                       lazy_eval(ls)))
  reducedData_ <- x

  values <- svd(reduced$M)$d
  energy <- cumsum(values) / sum(values)
  tarDim <- length(energy) -
    max(length(energy) - min(which(energy >= (1 - conditionallossValue))), 1)
  
  energyTotal_ <- 0
  projection_ <- diag(1, nrow = ncol(x) - 1)
  conditionalEnergy <- integer(0)
  iter <- 1
  
  while(energyTotal_ < totallossValue){
    
    reducedData <- reducedData_
    energyTotal <- energyTotal_
    projection <- projection_
    
    if(tarDim == 0){break}
    
    conditionalEnergy[iter] <- 1 - energy[tarDim]
    
    reducedData_ <- reduced$reducedData[, c(1:tarDim, ncol(reduced$reducedData))]
    
    projection_ <- reduced$projectionMatrix[1:tarDim,] %*% projection
    
    reduced <- do.call(method, c(list(x = reducedData_,
                                      targetDim = ncol(reducedData_) - 1),
                                 lazy_eval(ls)))
    
    values <- svd(reduced$M)$d
    energy <- cumsum(values) / sum(values)
    
    tarDim <- length(energy) -
      max(length(energy) - min(which(energy >= (1 - conditionallossValue))), 1)
    
    energyTotal_ <- do.call(loss, list(conditionalEnergy))
    iter <- iter + 1
  }
 
  object <- list(reducedData = reducedData,
                 projectionMatrix = projection,
                 group = reduced$group,
                 discrimFunc = reduced$discrimFunc,
                 energyTotal = energyTotal)
  class(object) <- "reduced"
  object
}

