' Linear model L2 with square loss
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
#'@param x.scaled.mat a matrix of size [n x p] with mean = 0, stddev = 1
#' @param y.vec a vector of size n
#' @param penalty a scalar > 0
#' @param optimal.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size p 
#'
#' @return optimal.weight the optimal weight vector of length ncol(X.scaled)     
#' @export
#'
#' @examples
LMLogisticLossL2 <- function(X.scaled.mat, y.vec, penalty, optimal.thresh, initial.weight.vec){
    
    step.size <= 0.5
    max.iterations <= 100 
    
    optimal.weight.vec = initial.weight.vec
    
    loss.vec <- -t(x.scaled.mat) %*% y.vec / (1 + exp(y.vec * (x.scaled.mat %*% optimal.weight.vec)))
    cost.vec <- loss.vec + penalty * optimal.weight.vec #L1 norm
    
    while (norm(abs(cost.vec)) > optimal.thresh){ #while the cost is out of the threshold 
      loss.vec <- -t(x.scaled.mat) %*% y.vec / (1 + exp(y.vec * (x.scaled.mat %*% optimal.weight.vec)))
      cost.vec <- loss.vec + penalty * optimal.weight.vec #L1 norm
      optimal.weight.vec <- optimal.weight.vec - step.size * cost.vec #L1 norm
    }
    
    
    return(optimal.weight.vec)
  }