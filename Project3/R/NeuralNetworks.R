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
  
  is.binary <- all(y.vec %in% c(-1,0,1))
  
  #first we should split the data into training and validation data
  #split the data on the is.train vector of booleans  
  x.train<- x.mat[is.train,]
  (x.train)
  (x.mat)
  
  x.valid<- x.mat[!is.train,]
  y.train<- y.vec[is.train]
  y.valid <- y.vec[!is.train]
  pred.mat <- matrix(0, nrow(x.train), max.iterations)
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
 
  sigmoid.prime <- Z * (1-Z) 
  
  intercept <- rep(0, nrow(x.train))
  #descale v.mat and w.vec here
  #need to loop through this max iteraitons number of times 
  for (index in (1:max.iterations)) {
    #fill out prediction matrix
    if(is.binary){
      delta.w <- -y.train %*% sigmoid(-y.train%*%b)
      pred.mat[,index] <-  ifelse(sigmoid(b)>0.5, 1, 0)
    }else{
      delta.w <- b - y.train
      pred.mat[,index] <- b
    }
    
     #(re)compute our gradient/deltas
    delta.v <- diag(as.vector(delta.w)) %*% sigmoid.prime %*% diag(as.vector(w))
    grad.w <- t(Z) %*% delta.w / nrow(x.scaled.mat)
    grad.v<- t(x.scaled.mat) %*% delta.v / nrow(x.scaled.mat) 
   
    #cost<- sum(abs(c(grad.w, as.numeric(grad.v)))) #find the minimum cost for L1
    
    #calculate intercept
    intercept <- intercept - step.size * delta.w
    b <- Z %*% w + intercept # n x 1
    
    
  
    #now take a step
    w<- w - step.size * grad.w
    V<- V - step.size * grad.v
    }
  
  V.unscaled<-V/attr(x.scaled.mat, "scaled:scale")
  b.unscaled<- -t(V/attr(x.scaled.mat, "scaled:scale")) %*% attr(x.scaled.mat, "scaled:center")
  w.vec <- c(intercept, w)
  #need to return our list of things
  #pred.mat should be n x max.iteration
  #v.mat n+1 x hidden units
  #w.vec hidden units +1
  result.list <- list(
    pred.mat = pred.mat,
    V.mat = V.unscaled,
    w.vec = w,
    predict = function(testX.mat) {
      prediction.vec <- sigmoid(cbind(1, x.train) %*% V) %*% w
      return(prediction.vec)
    }
  )
  return(result.list)
  
}


