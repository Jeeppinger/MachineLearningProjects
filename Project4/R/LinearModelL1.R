#' Linear Model with L1 regularization
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
     # stop("initial.weight.vec must be a numeric vector")
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
    
    
    
    # Setting up case for binary
    is.binary <- all(ifelse(y.vec %in% c(0, 1), TRUE, FALSE))
    step.factor <- 2L
    
    if (is.binary) {
      y.vec <- ifelse(y.vec == 0, -1, 1)
    }
    
    n.cols <- ncol(x.scaled.mat)   # number of cols p 
    n.rows <- nrow(x.scaled.mat)  # number of rows n 
    max.iter <- 50L
    x.train <- cbind(1,x.scaled.mat) # n x (p+1)    
    
    loss <- function(lst){
      if (is.binary)
        mean(log(1+exp(-y.vec * (x.train %*% lst$w.vec))))
      else
        mean((x.train %*% lst$w.vec - y.vec)^2)
    }
    
    iter.learn <- function(initial.weight.vec, step.size){
      if (is.binary){ 
        # do logistic loss
        w.gradient.vec <-t(x.train) %*% (y.vec / (1 + exp(-y.vec * (x.train %*% initial.weight.vec))))
      }else{
        # do square loss
        w.gradient.vec <- -t(x.train) %*% (x.train %*% initial.weight.vec - y.vec)
      }
      
      u.vec <- initial.weight.vec + step.size * w.gradient.vec / n.rows
      initial.weight.vec.new <- c(u.vec[1], soft(u.vec[-1], step.size * penalty))
      
      ret.lst <- list( w.vec = initial.weight.vec.new,gradient.vec = w.gradient.vec)
      return(ret.lst)
    }
    
    norm.gradient <- function(gradient, w){
      ifelse(w==0, positive.part(abs(gradient) - penalty),
             abs(gradient - sign(w) * penalty))
    }
    
    iter <-  0
    while (1) {
      
      lst.n <- iter.learn(initial.weight.vec, step.size)
      return.weight.vec <- lst.n$w.vec
      loss(lst.n)
      can.break <- c(lst.n$gradient.vec[1], norm.gradient(lst.n$gradient.vec[-1], return.weight.vec[-1]))
      iter <- iter + 1
      if ((norm(as.matrix(abs(can.break)),'2') < opt.thresh) || (iter >= max.iter))
        break;
    }
    return(return.weight.vec)
}