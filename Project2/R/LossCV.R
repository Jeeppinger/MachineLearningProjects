#' Cross validation for linear model using L2 regularization and square loss
#'
#' @param x.mat a matrix of size [n x p]
#' @param y.vec a vector of length n
#' @param fold.vec a vector of length n
#' @param penalty.vec a vector of penalties
#'
#' @return results.list a list with mean.validation.loss.vec, mean.train.loss.vec, penalty.vec, selected.penalty, weight.vec, and predict function
#' @export
#'
#' @examples
#' data <- data(prostate, package="ElemStatLearn")
#' data.set<-prostate[,-dim(prostate)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iterations=10
#' fold.vec<-sample(rep(1:5,l=nrow(x.mat)))

# penalty.vec <-
# LMSquareLossL2CV(x.mat,y.vec,fold.vec,penalty.vec)
LMSquareLossL2CV <- function(x.mat, y.vec, fold.vec, penalty.vec) {
  num.folds <- length(unique(fold.vec))
  
  validation.loss.mat <- matrix(rep(0, num.folds * length(penalty.vec)), num.folds, length(penalty.vec))
  train.loss.mat <- matrix(rep(0, num.folds * length(penalty.vec)), n.folds, length(penalty.vec))
  
  #split train and validation
  for (fold.index in seq_len(num.folds)) {#a sequence counts from 1 to num.folds
    train.index <- which(fold.vec != fold.index)
    validation.index <- which(fold.vec == fold.index)
    
    #determine loss
    w.mat <-LMSquareLossL2penalties(x.mat[train.index, ], y.vec[train.index, ], penalty.vec)
    
    train.prediction <- x.mat[train.index, ] %*% w.mat
    train.loss <- (train.prediction - y.vec[train.index, ]) ^ 2
    
    #determine validation loss
    validation.prediction <- x.mat[validation.index, ] %*% w.mat
    validation.loss <-(validation.predict - y.vec[validation.index, ]) ^ 2
    
    
    mean.train.loss <- colMeans(train.loss)
    mean.validation.loss <- colMeans(validation.loss)
    
    train.loss.mat[fold.index, ] = mean.train.loss
    validation.loss.mat[fold.index, ] = mean.validation.loss
  }
  
  train.loss.vec <- colMeans(train.loss.mat)
  validation.loss.vec <- colMeans(validation.loss.mat)
  
  current.penalty <- penalty.vec[which.min(validation.loss.vec)]
  w.mat <- LMSquareLossL2penalties(x.mat[train.index, ], y.vec[train.index, ], penalty.vec)
  w.vec <- w.mat[, current.penalty]
  
  prediction <- function(test.mat) {
      prediction.vec <- test.mat %*% w.vec
  }
  
  results.list <- list(
    validation.loss.vec = mean.validation.loss.vec,
    train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    current.penalty = selected.penalty,
    w.vec = weight.vec,
    prediction
  )
  
  return(results.list)
}


#' Cross validation for linear model using L2 regularization / logistic loss
#'
#' @param X.mat a  matrix of size [n x p]
#' @param y.vec a of length n
#' @param fold.vec a vector of length n
#' @param penalty.vec a vector > 0
#'
#' @return results.list a list of mean.validation.loss.vec, mean.train.loss.vec, penalty.vec, selected.penalty, weight.vec, and predict function
#' 
#' @export
#'
#' @examples
#' data <- data(spam, package="ElemStatLearn")
#' data.set<-spam[,-dim(spam)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iterations=10
#' fold.vec<-sample(rep(1:5,l=nrow(x.mat)))

# penalty.vec <-
# LMLogisticLossL2CV(x.mat,y.vec,fold.vec,penalty.vec)
LMLogisticLossL2CV <- function(x.mat, y.vec, fold.vec, penalty.vec) {
  train.loss.mat <-matrix(0, nrow(x.mat), ncol(x.mat))
  validation.loss.mat <-matrix(0, nrow(x.mat), ncol(x.mat))
  
  # split data
  for (fold.index in seq_len(num.folds)) {
    train.index <- which(fold.vec != fold.index)
    
    # go through all train validation splits
    for (trainvalid.split in c("train", "validation")) {
      if (trainvalid.split == "train") {
        validation.index <- which(fold.vec != fold.index)
      } else{#validation
        validation.index <- which(fold.vec == fold.index)
      }
      
      w.mat <- LMLogisticLossL2penalties(x.mat[train.index,], y.vec[train.index], penalty.vec) 
      if (trainvalid.split == "train") {
        train.loss.mat[fold.index, ] <-colMeans(x.mat[validation.index,] %*% w.mat - y.vec[validation.index])
      } else{#validation
        validation.loss.mat[fold.index, ] <-colMeans(X.mat[validation.index,] %*% w.mat - y.vec[validation.index])
      }
    }
  }
  mean.train.loss <- colMeans(train.loss.mat)
  mean.validation.loss <- colMeans(validation.loss.mat)
  current.penalty <- which.min(mean.validation.loss)
  
  w.vec <-LMLogisticLossL2penalties(x.mat, y.vec, penalty.vec)[, current.penalty]
  
  prediction <- function(test.mat) {
    prediction.vec <- test %*% t(w.vec)
  }
  
  result.list <- list(
    mean.validation.loss = mean.validation.loss.vec,
    mean.train.loss = mean.train.loss.vec,
    current.penalty = penalty.vec,
    selected.penalty = penalty.vec[current.penalty],
    w.vec = weight.vec,
    prediction = predict
  )
  
  return(result.list)
}