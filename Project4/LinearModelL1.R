#'Linear Model algorithm
#'
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
  
  #first we are going to declare all of our functions that we need 
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

  
  #wrap this whole piece in a while loop until we have hit the optimal thresh
  w <- rep(0, l=ncol(x.scaled.mat)) 
  b<- 0
  d.vec <- -grad.loss(w)
  u.vec<- w+step.size*d.vec
  
  lambda <- 5
  w <- soft(u.vec, step.size*lambda)
  
  
  
}

#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param penalty.vec vector of decreasing penalty values
#'
#' @return optimal.weight.vector

LinearModelL1penalties <- function(x.mat, y.vec, penalty.vec) {
  #first scale x mat
  #then loop over pentalty values
  #finally unscale the optimal weight vector
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

