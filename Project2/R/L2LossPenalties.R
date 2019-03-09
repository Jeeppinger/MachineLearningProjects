#' Linear model with L2 penalties and square loss
#'
#' @param x.mat a matrix of size [n x p]
#' @param y.vec a vector of length n
#' @param penalty.vec a vector > 0
#'
#' @return w.mat a weight matrix of size [p x length(penalty.vec)]
#' @export
#'
#' @examples
#' data <- data(prostate, package="ElemStatLearn")
#' data.set<-prostate[,-dim(prostate)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])

# penalty.vec <-
# LMSquareLossL2penalties(x.mat,y.vec,penalty.vec)
LMSquareLossL2penalties <- function(x.mat, y.vec, penalty.vec) {
  
  #first scale x
  x.scaled.mat <- scale(x.mat)
  total.mat <- matrix(c(rep(0, ncol(x.mat) * length(penalty.vec)),ncol(x.mat),length(penalty.vec)))
  slope.mat <- matrix(c(rep(0, ncol(x.mat) * length(penalty.vec)),ncol(x.mat),length(penalty.vec)))
  
  for (current.index in seq(length(penalty.vec))) {#seq is a sequence that counts up to length of penalty vector
    #need initial weight vector
    optimal.weight.vec <- LMSquareLossL2(x.scaled.mat,y.vec = y.vec, penalty.vec[current.index], 1, slope.mat)
    total.mat[, current.index] <- optimal.weight.vec
  }
  
  mean.vec <- colMeans(x.mat)
  x.stddev.vec <- sqrt(rowSums((t(x.mat) - mean.vec) ^ 2) / nrows(x.mat))
  x.stddev.mat <- diag(nrows(x.mat)) * (1 / x.stddev.vec)
  
  intercept <- -t(total.mat) %*% x.stddev.mat %*% mean.vec #m x 1
  slope <- t(total.mat) %*% x.stddev.mat #m x f-1
  w.mat <- rbind(t(intercept), t(slope))
  return(w.mat)
}



#' Linear model with L2 penalties and logistic loss
#'
#' @param x.mat a matrix of size [n x p]
#' @param y.vec a vector of length n
#' @param penalty.vec a vector > 0
#'
#' @return w.mat a weight matrix of size [p x length(penalty.vec)]
#' @export
#'
#' @examples
#' data <- data(spam, package="ElemStatLearn")
#' data.set<-spam[,-dim(spam)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])

# penalty.vec <-
# LMLogisticLossL2penalties(x.mat,y.vec,penalty.vec)
LMLogisticLossL2penalties <- function(x.mat, y.vec, penalty.vec) {

  #first scale x
  x.scaled.mat <- scale(x.mat)
  
  w.mat <- matrix(0, nrow = nrow(x.mat), ncol = length(penalty.vec))
  
  for(current.index in (1:length(penalty.vec))){
    w.mat[,current.index] <- LMLogisticLossL2(x.scaled.mat,y.vec,1, penalty.vec[current.index]) 
    # 1 is the optimal.thresh
  }
  mean.vec <- colMeans(x.mat)
  x.stddev.vec <- sqrt(rowSums((t(x.mat) - mean.vec)^2)/nrow(x.mat))
  x.stddev.mat <- 1 / diag(x.stddev.vec)
  
  intercept.vec <- -t(mean.vec) %*% x.stddev.mat %*% w.mat
  w.mat <- rbind(intercept.vec,x.stddev.mat %*% w.mat)
  
  return(w.mat)
  
}