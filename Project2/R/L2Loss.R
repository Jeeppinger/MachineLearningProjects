#' Linear model L2 with square loss
#'
#' @param x.scaled.mat a matrix of size [n x p] with mean = 0, stddev = 1
#' @param y.vec a vector of size n
#' @param penalty a scalar > 0
#' @param optimal.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size p 
#'
#' @return current.weight.vec the optimal weight vector of size p
#' @export
#'
#' @examples
#' data <- data(prostate, package="ElemStatLearn")
#' data.set<-prostate[,-dim(prostate)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' penalty<-5
#' max.iterations=10
#' w.vec <- LMSquareLossIterations(x.mat,y.vec,max.iterations)[,9]

# LMSquareLossL2(x.mat,y.vec,penalty,initial.weight.vec=w.mat[,9])
LMSquareLossL2 <-function(x.scaled.mat, y.vec, penalty, optimal.thresh = 0.5, initial.weight.vec) {
  #1/2 is a good default optimal thresh  
  current.weight.vec <- initial.weight.vec
    # loop until we are in the optimal threshold
    while (TRUE) {
      grad.cost <- 2* t(x.scaled.mat) %*%(x.scaled.mat %*% current.weight.vec - as.matrix(y.vec)) + 2 * penalty * current.weight.vec
      #the above formula was derived in class notes
      if (sum(abs(grad.cost)) <= optimal.thresh) {
        break#stop because we are in the threshold
      } else{
        current.weight.vec <- current.weight.vec - penalty * (grad.cost)#reset current weight and try again
      }
      
    }
    return(current.weight.vec)
    
  }


#' Linear model L2 regularization with logistic loss
#'
#' @param X.scaled.mat a matrix of size [n x p] with mean = 0, stddev = 1
#' @param y.vec a vector of size n
#' @param penalty a scalar > 0
#' @param optimal.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size p 
#'
#' @return optimal.weight the optimal weight vector of length ncol(X.scaled)     
#' @export
#'
#' @examples
#' data <- data(spam, package="ElemStatLearn")
#' data.set<-spam[,-dim(spam)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' penalty<-5
#' max.iterations=10
#'w.vec <- LMLogisticLossIterations(x.mat,y.vec,max.iterations)[,9]
 
#LMLogisticLossL2(x.mat,y.vec,penalty,initial.weight.vec=w.mat[,9])
LMLogisticLossL2 <- function(x.scaled.mat, y.vec, penalty, optimal.thresh, initial.weight.vec){
    
    step.size <- 0.5
    max.iterations <- 100 
    
    optimal.weight.vec = initial.weight.vec
    
    loss.vec<- -t(x.scaled.mat) %*% as.matrix(y.vec) / as.double((as.matrix(1) + exp(as.vector(y.vec) %*% (x.scaled.mat %*% optimal.weight.vec))))
    
    cost.vec <- loss.vec + penalty * optimal.weight.vec #L1 norm
    i<-1
    while (i <10){ #while the cost is out of the threshold 
      #-t(x.scaled.mat) %*% as.matrix(y.vec) / as.double((as.matrix(1) + exp(as.vector(y.vec) %*% (x.scaled.mat %*% w.vec))))#used to add b.temp
      
      loss.vec<- -t(x.scaled.mat) %*% as.matrix(y.vec) / as.double((as.matrix(1) + exp(as.vector(y.vec) %*% (x.scaled.mat %*% optimal.weight.vec))))
      cost.vec <- loss.vec + penalty * optimal.weight.vec #L1 norm
      optimal.weight.vec <- optimal.weight.vec - step.size * cost.vec #L1 norm
      i<- i+1
    }
    
    
    return(optimal.weight.vec)
}
