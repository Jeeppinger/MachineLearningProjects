#' Linear square loss
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
#' data <- data(prostate, package="ElemStatLearn")
#' data.set<-prostate[,-dim(prostate)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iterations=10
#' LMSquareLossIterations(x.mat,y.vec,max.iterations)
LMSquareLossIterations <-function(x.mat, y.vec, max.iterations, step.size = 0.5) {
    if(class(x.mat)!="matrix" || class(y.vec)!="matrix"){
      return("bad input")
    }
    num.train <- nrow(x.mat)
    num.feature <- ncol(x.mat)
    
    x.scaled.mat <- scale(x.mat)
    slope.mat <-matrix(c(rep(0, num.feature * max.iterations)), num.feature, max.iterations) #p x max iteration
    
    # loop to populate slope.mat 
    for (index in (1:max.iterations)) {
      temp.mean.loss.vec <- (2 * t(x.scaled.mat) %*%(x.scaled.mat %*% slope.mat[, index] - as.matrix(y.vec))) / num.train
      temp.slope.vec <-slope.mat[,index] - step.size * temp.mean.loss.vec
      slope.mat[, index] = temp.slope.vec
      
    }
    
    mean.vec <- colMeans(x.mat) # 1 x p
    x.stddev.vec <-sqrt(rowSums((t(x.mat) - mean.vec) ^ 2) / num.train) 
    x.stddev.mat <- diag(num.feature) * (1 / x.stddev.vec)

    intercept <- -t(slope.mat) %*% x.stddev.mat %*% mean.vec #max iteration x 1
    slope <- t(slope.mat) %*% x.stddev.mat  #max iteration x p
    w.mat <- rbind(t(intercept), t(slope)) #p + 1 x max iteration
    
    return(w.mat)
    #x.mat %*% w.mat should get a prediction matrix
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
#' data <- data(spam, package="ElemStatLearn")
#' data.set<-spam[,-dim(spam)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iterations=10

# LMLogisticLossIterations(x.mat,y.vec,max.iterations)
# put ' when this works

LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {
    # TODO: add error checking
    
    x.scaled.mat <- scale(x.mat)
    # Initialize weight matrix (w) and beta (b)
    w.mat <- matrix(0, nrow = ncol(x.mat), ncol = max.iterations)
    w.vec <- w.mat[,1]
    b.vec <- rep(0,l = max.iterations)
    b.temp <- 0 
    # loop through all iterations
    for (num.interations in (1:max.iterations)) {
      #these come from the notes in class
      w.grad.vec <- -t(x.scaled.mat) %*% as.matrix(y.vec) / (1 + exp(y.vec * (x.scaled.mat %*% w.vec + b.temp)))
      
      b.grad <- - sum(y.vec) / (1 + exp(y.vec * (x.scaled.mat %*% w.vec + b.temp)))
      
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