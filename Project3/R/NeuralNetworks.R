#Neural Network algorithm
#'
#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param max.iterations integer > 1
#' @param step.size integer > 1
#' @param n.hidden.units nunmber of hidden units >= 1
#' @param is.train booleans in vector of size n
#' 
#' @return results.list a list with pred.mat, v.mat, w.vec, and predict functionthat takes testx.mat
#'
#' @export
#'
#' @examples
NNetIterations <- function(x.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train) {
#see the demoProject3.R file for a starting point on the NNet algorithm
  is.binary <- TRUE
  #first we should split the data into training and validation data
  #split the data on the is.train vector of booleans  
  for (index in (1:length(is.train))) {
    if(is.train[index]){
      x.train <- cbind(x.train, x.mat[index,])
      y.train <-cbind(y.train, y.vec[index])
    }else{
      x.valid <- cbind(x.valid, x.mat[index,])
      y.valid <-cbind(y.valid, y.vec[index])
    }
    if (y.vec[index] != 1 && y.vec[index]!=0){
     is.binary <- FALSE 
    }
  }
    
  #then scale the train data here
  x.scaled.mat <- scale(x.train)
  V<- matrix(rnorm(ncol(x.scaled.mat)*n.hidden.units), ncol(x.scaled.mat), n.hidden.units)
  A <- x.scaled.mat %*% V
  sigmoid <- function(a){
    1/(1+exp(-a))
  }
  Z <- sigmoid(A)
  w <- rnorm(n.hidden.units)
  b <- as.numeric(Z %*% w)
  
  #decide which delta.w based on if we are binary
  if(is.binary){
    delta.w <- -y.train %*% sigmoid(-y.train%*%b)
  }else{
    delta.w <- b - y.train
  }
  
  sigmoid.prime <- Z * (1-Z) 
  delta.v <- diag(delta.w) %*% sigmoid.prime %*% diag(w)
  grad.w <- t(Z) %*% delta.w / nrow(x.scaled.mat)
  grad.v<- t(x.scaled.mat) %*% delta.v / nrow(x.scaled.mat) 
  
  #need to loop through this max iteraitons number of times 
  for (index in (1:max.iterations)) {
  #now take a step
  w<- w - step.size * grad.w
  V<- V - step.size * grad.v
  cost<- sum(abs(c(grad.w, as.numeric(grad.v)))) #find the minimum cost for L1
  #optimize square loss
  #append w and V to w.vec and V.mat
  }
  #need to return our list of things
  #pred.mat should be n x max.iteration
  #v.mat n+1 x hidden units
  #w.vec hidden units +1
  
}


#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param fold.vec vector of size n containing folds
#' @param max.iterations integer > 1
#' @param step.size integer > 1
#' @param n.hidden.units nunmber of hidden units >= 1
#' 
#' @return results.list a list with pred.mat, v.mat, w.vec, predict function that takes (testx.mat), mean.validaiton.loss.vec, selected.steps
#'
#' @export
#'
#' @examples

NNetEarlyStoppingCV <- function(x.mat, y.vec, fold.vec=sample(rep(1:n.folds),length(y.vec)), 
                                          max.iterations, step.size, n.hidden.units, n.folds = 4) {
  
}