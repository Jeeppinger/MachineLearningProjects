#' Linear square loss
#'
#'A package that computes information about linearmodels and regression 
#'This funciton uses R functions to calculate the square loss for a given set of data
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
    x.standard.mat <- diag(num.row) * (1/x.standard.vec)
    
    x.scaled.mat <- scale(x.mat)
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
#' This funciton uses R to calculate the logistic loss for a given set of data
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
#' 

LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {
    # TODO: add error checking
    
    x.scaled.mat <- scale(x.mat)
    
    # Initialize weight matrix (w) and beta (b)
    w.mat <- matrix(0, nrow = ncol(x.mat), ncol = max.iterations)
    w.vec <- w.mat[,0]
    b.vec <- rep(0,l = max.iterations)
    b.temp <- 0 
    # loop through all iterations
    for (num.interations in (1:max.iterations)) {
      #these come from the notes in class
      w.grad.vec <- -t(x.scaled.mat) %*% y.vec / (1 + exp(y.vec * (x.scaled.mat %*% w.vec + b.temp)))
      
      b.grad <- -sum(y.vec) / (1 + exp(y.vec * (x.scaled.mat %*% w.vec + b.temp)))
      
      # Take one step along gradient
      w.mat[,num.iterations] <-wvec - step.size * w.grad.vec
      b.vec[num.iterations] <- b.temp - step.size * b.grad
      
      w.vec <- w.mat[,num.interations]
      b.grad <- b.vec[num.iterations]
    }
    
    # need to convert back to scaled data
    x.mean.vec <- colMeans(x.mat)
    #TODO: look into a function to make standard dev easier
    x.stddev.vec <-sqrt(rowSums((t(x.mat) - x.mean.vec)^2) / nrows(x.mat))
    
    temp.vec <- -t(x.mean.vec) %*% x.stddev.mat %*% w.mat
    w.mat <- rbind(temp.vec, x.stddev.mat %*% w.mat)
    
    return(w.mat)
    
    
  }