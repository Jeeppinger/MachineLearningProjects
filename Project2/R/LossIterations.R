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
    
    x.standard.vec <- sqrt(rowSums((t(x.mat) - mean.vec)^2) / num.row)
    x.standars.mat <- diag(num.row) * (1/x.standard.vec)
    
    x.scaled.mat <- (t(x.standard.mat) - mean.vec) / x.standard.vec
    slope.mat <- matrix(c(rep(0, num.col * max.iterations), num.col, max.iterations)) 
    
    # loop to get the slope matrix
    for (index in (1:max.iterations)){
      if (index == 1){
        temp.mean.loss.vec <- (2 * t(x.scaled.mat) %*% 
                                 (x.scaled.mat %*% slope.mat[,1])) / num.row
        temp.slope.vec <- slope.mat[,1] - step.size * temp.mean.loss.vec
      }else{
        temp.mean.loss.vec <- (2 * t(x.scaled.mat) %*% (x.scaled.mat %*% slope.mat[,index - 1])) / num.row
        temp.slope.vec <- slope.mat[,index - 1] - step.size * temp.mean.loss.vec
      }
      slope.mat[,index] = temp.slope.vec
      
    }
    itercept.vec <- -t(slope.mat) %*% x.scaled.mat %*% mean.vec #n x 1 vector
    slope.mat <- t(slope.mat) %*% x.scaled.mat  #n x (p-1) matrix
    w.mat <- rbind(t(itercept.vec),t(slope.mat)) #(p-1 x n) + (1xn) = p x n matrix
    return(w.mat)
    #X.mat %*% W.mat should get a prediction matrix
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
LMLogisticLossIterations <- function(x.mat, y.vec,max.iterations, step.size){
  x.mat <- x.mat[,-1] #need to scale x.mat to so standard dev = 1, mean = 0
  num.row <- dim(x.mat)[1]
  num.col <- dim(x.mat)[2]
  
  mean.vec <- colMeans(x.mat)
  
  x.standard.vec <- sqrt(rowSums((t(x.mat) - mean.vec)^2) / num.row)
  x.standars.mat <- diag(num.row) * (1/x.standard.vec)
  
  x.scaled.mat <- (t(x.standard.mat) - mean.vec) / x.standard.vec
  slope.mat <- matrix(c(rep(0, num.col * max.iterations), num.col, max.iterations)) 
  
  # loop to get the slope matrix
  for (index in (1:max.iterations)){
    if (index == 1){
      temp.mean.loss.vec <- (2 * t(x.scaled.mat) %*% 
                               (x.scaled.mat %*% slope.mat[,1])) / num.row
      temp.slope.vec <- slope.mat[,1] - step.size * temp.mean.loss.vec
    }else{
      temp.mean.loss.vec <- (2 * t(x.scaled.mat) %*% (x.scaled.mat %*% slope.mat[,index - 1])) / num.row
      temp.slope.vec <- slope.mat[,index - 1] - step.size * temp.mean.loss.vec
    }
    slope.mat[,index] = temp.slope.vec
    
  }
  itercept.vec <- -t(slope.mat) %*% x.scaled.mat %*% mean.vec #n x 1 vector
  slope.mat <- t(slope.mat) %*% x.scaled.mat  #n x (p-1) matrix
  w.mat <- rbind(t(itercept.vec),t(slope.mat)) #(p-1 x n) + (1xn) = p x n matrix
  return(w.mat)
  #X.mat %*% W.mat should get a prediction matrix
}

#TODO: set up more functions like above by friday