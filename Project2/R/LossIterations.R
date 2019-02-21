#' Linear square loss
#'
#'A package that computes information about linearmodels and regression 
#'This funciton calls the c interface in the package to calculate the square loss for a given set of data
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param max.iterations max number of steps to take on gradient descent
#' @param step.size how big of a step to take for each iteration
#'
#' @return w.mat a weight vector with one weight per iteration
#' @export
#'
#' @examples

LMSquareLossIterations <- function(x.mat, y.vec,max.iterations ,step.size){
    x.mat <- x.mat[,-1] #need to scale x.mat to so standard dev = 1, mean = 0
    num.row <- dim(x.mat)[1]
    num.col <- dim(x.mat)[2]
    
    mean.vec <- colMeans(x.mat)
    
    x.scaled.vec <- sqrt(rowSums((t(x.mat) - mean.vec)^2) / num.row)
    x.scaled.mat <- diag(num.feature) * (1/x.scaled.vec)
    
    x.std.mat <- (t(x.mat) - mean.vec) / x.scaled.vec
    slope.mat <- matrix(c(rep(0, num.col * max.iterations), num.col, max.iterations)) 
    
    # loop to get the slope matrix
    for (index in (1:max.iterations)){
      if (index == 1){
        mean.loss.temp.vec <- (2 * t(x.std.mat) %*% 
                                 (x.std.mat %*% slope.mat[,1])) / num.train
        slope.vec.temp <- slope.mat[,1] - step.size * mean.loss.temp.vec
      }else{
        mean.loss.temp.vec <- (2 * t(x.std.mat) %*% (x.std.mat %*% slope.mat[,index - 1])) / num.train
        slope.vec.temp <- slope.mat[,index - 1] - step.size * mean.loss.temp.vec
      }
      slope.mat[,index] = slope.vec.temp
      
    }
    itercept <- -t(slope.mat) %*% x.std.mat %*% mean.vec #n x 1 vector
    slope.mat <- t(slope.mat) %*% x.std.mat  #n x (p-1) matrix
    w.mat <- rbind(t(itercept),t(slope.mat)) #p x n matrix
    return(w.mat)
  }
  

#' Linear logistic loss
#' This funciton calls the c interface in the package to calculate the logistic loss for a given set of data
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param max.iterations max number of steps to take on gradient descent
#' @param step.size how big of a step to take for each iteration
#'
#' @return w.mat a weight vector with one weight per iteration
#' @export
#'
#' @examples
LMLogisticLossIterations <- function(x.mat, y.vec,max.iterations ,step.size){
  x.mat <- x.mat[,-1] #need to scale x.mat to so standard dev = 1, mean = 0
  num.row <- dim(x.mat)[1]
  num.col <- dim(x.mat)[2]
  
  mean.vec <- colMeans(x.mat)
  
  x.scaled.vec <- sqrt(rowSums((t(x.mat) - mean.vec)^2) / num.row)
  x.scaled.mat <- diag(num.feature) * (1/x.scaled.vec)
  
  x.std.mat <- (t(x.mat) - mean.vec) / x.scaled.vec
  slope.mat <- matrix(c(rep(0, num.col * max.iterations), num.col, max.iterations)) 
  
  # loop to get the slope matrix. may need to change this
  for (index in (1:max.iterations)){
    if (index == 1){
      mean.loss.temp.vec <- (2 * t(x.std.mat) %*% (x.std.mat %*% slope.mat[,1])) / num.train
      slope.vec.temp <- slope.mat[,1] - step.size * mean.loss.temp.vec
    }else{
      mean.loss.temp.vec <- (2 * t(x.std.mat) %*% (x.std.mat %*% slope.mat[,index - 1])) / num.train
      slope.vec.temp <- slope.mat[,index - 1] - step.size * mean.loss.temp.vec
    }
    slope.mat[,index] = slope.vec.temp
    
  }
  itercept <- -t(slope.mat) %*% x.std.mat %*% mean.vec #n x 1 vector
  slope.mat <- t(slope.mat) %*% x.std.mat  #n x (p-1) matrix
  w.mat <- rbind(t(itercept),t(slope.mat)) #p x n matrix
  return(w.mat)
}

#TODO: set up more functions like above by friday