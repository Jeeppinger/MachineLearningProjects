
#' @param x.scaled.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param penalty non negative numeric scalar
#' @param opt.thresh positive numeric scalar
#' @param initial.weight.vec vector of initial weights
#' @param step.size integer > 1
#' 
#'@return optimal.weight.vector
#' @export
#'
#' @examples
LinearModelL1 <- function(x.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size=.5) {
  #1/n ∑i=1^n L[w^T x_i + b, y_i] + penalty * ||w||_1
  #trying to use the cost function written above
      # Check type and dimension
    if (!all(is.numeric(x.scaled.mat), is.matrix(x.scaled.mat))) {
      stop("x.scaled.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),is.vector(y.vec),length(y.vec) == nrow(x.scaled.mat))) {
      stop("y.vec must be a numeric vector")
    }
    
    if (!all(is.numeric(penalty), length(penalty) == 1, penalty >= 0)) {
      stop("penalty must be a numeric scalar > 0")
    }
    
    if (!all(is.numeric(opt.thresh),length(opt.thresh) == 1, opt.thresh > 0)) {
      stop("opt.thresh must be a numeric scalar > 0")
    }
    
    if (!all(is.numeric(initial.weight.vec), is.vector(initial.weight.vec),length(initial.weight.vec) == ncol(x.scaled.mat) + 1)) {
      stop("initial.weight.vec must be a numeric vector")
    }
    
    # Initializing functions
    sigmoid<- function(z){
      1/(1+exp(-z)) 
    }
    grad.loss<-function(w.vec){
      #binary classification so logistic loss
      pred.vec <- x.sc %*% w
      prob.vec <- sigmoid(-pred.vec * y.tilde)
      grad.vec <- - t(x.sc) %*% (y.tilde  * prob.vec)
    }
    positive.part<-function(x){
      ifelse(x > 0, x, 0)
    }
    soft<- function (w, lambda){
      sign(w)*positive.part(abs(w) - lambda)
    }
    l1opt<-function(w.vec, d){
      ifelse(w.vec==0, 
             positive.part(abs(d) - lambda), 
             abs(d-sign(w.vec) * lambda))
    }
    
    
    
    is.binary <- ifelse(y.vec %in% c(0, 1), TRUE, FALSE)
    max.iteration <- 10000L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.cols <- ncol(x.scaled.mat)   # p 
    n.rows <- nrow(x.scaled.mat)  # n 
    
    x.train <- cbind(1,x.scaled.mat) # n x (p+1)    
    w.vec <- rnorm(n.cols) # p x 1
    intercept <- rnorm(1)
    
    while (1) {
      if (is.binary) {
        # do logistic
        w.gradient.vec <-t(x.train) %*% (y.vec / (1 + exp(y.vec * (x.train %*% w.vec + rep(1,n.rows) * intercept))))
        
        intercept.gradient <- t(rep(1,n.rows)) %*% (y.vec / (1 + exp(y.vec * (x.train %*% w.vec + rep(1,n.rows) * intercept))))
        
        u.vec <- w.vec + step.size * w.gradient.vec / n.rows
        intercept <- intercept + step.size * intercept.gradient / n.rows
        w.vec <- soft(u.vec, step.size * penalty)
      } else{
        # do linear square loss
        w.gradient.vec <- -t(x.train) %*% (x.train %*% w.vec + rep(1,n.rows) * intercept - y.vec)
        
        intercept.gradient <- -t(rep(1,n.rows)) %*% (x.train %*% w.vec + rep(1,n.rows) * intercept - y.vec)
        
        intercept <- intercept + step.size * intercept.gradient / n.rows
        u.vec <- w.vec + step.size * w.gradient.vec / n.rows
        w.vec <- soft(u.vec, step.size * penalty)
      }
      
      temp.w.vec <- c(intercept, w.vec) 
      if (all(positive.part(w.gradient.vec[w.vec==0] - penalty) < opt.thresh, 
              positive.part(w.gradient.vec[w.vec!=0] - sign(w.vec[w.vec!=0]) * penalty) < opt.thresh,
              abs(intercept.gradient) < opt.thresh))
        break;
    }
    
    w.vec <- c(intercept, w.vec)
    return(w.vec)
}  

#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param fold.vec
#' @param n.folds
#' @param penalty.vec vector of decreasing penalty values
#' @param step.size
#'
#' @return mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty, weight, predict function

LinearModelL1CV <- function(x.mat, y.vec, fold.vec = sample(rep(1:5, l=nrow(x.mat))), n.folds = 5, penalty.vec, step.size) {
  #set up our functions that we need
  sigmoid<- function(z){
    1/(1+exp(-z)) 
  }
  grad.loss<-function(w.vec){
    #binary classification so logistic loss
    pred.vec <- x.sc %*% w
    prob.vec <- sigmoid(-pred.vec * y.tilde)
    .grad.vec <- - t(x.sc) %*% (y.tilde  * prob.vec)
  }
  soft<- function (w, lambda){
    sign(w)*positive.part(abs(w) - lambda)
  }
  positive.part<-function(x){
    ifelse(x > 0, x, 0)
  }
  
  #first we scale x
  x.scaled.mat<- scale(x.mat)
  
  for (fold.idx in seq(n.folds)) {
    #split out the training data
    is.train <- fold.vec != fold.idx
    x.train<- x.scaled.mat[is.train,]
    y.train <- y.vec[is.train]
    #create y tilde
    y.tilde = ifelse(y.train ==1, 1, -1)
    #this is where we call our other function, LossPenalties
    #Compute validation loss for each one
    #determine the best penalty here
    #append all penalties together
    
  }
  #lastly use the optimal penalty to get the result list we need.
  #return our list of things here.
  
  
  
  
}

