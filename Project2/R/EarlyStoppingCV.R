#' Cross validation algorithm using linear model with square loss
#'
#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param fold.vec folds for CV split
#' @param max.iteration integer > 1
#'
#' @return results.list a list with mean.validation.loss.vec, mean.train.loss.vec, selected.steps, weight.vec, and predict function
#'
#' @export
#'
#' @examples
#' data <- data(prostate, package="ElemStatLearn")
#' data.set<-prostate[,-dim(prostate)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iteration=10
#' fold.vec<-sample(rep(1:5,l=nrow(x.mat)))
#' LMSquareLossEarlyStoppingCV(x.mat,y.vec,fold.vec,max.iteration)

LMSquareLossEarlyStoppingCV <-function(x.mat, y.vec, fold.vec, max.iteration) {
    # Find the num of K-fold
    num.folds <- length(unique(fold.vec))
    
    validation.loss.mat <-matrix(rep(0, num.folds * max.iteration), num.folds, max.iteration)
    train.loss.mat <- matrix(rep(0, num.folds * max.iteration), num.folds, max.iteration)
    
    
    #set up train and validation split
    for (fold.index in seq_len(num.folds)) {
      train.index <- which(fold.vec != fold.index)
      validation.index <- which(fold.vec == fold.index)
      
      #determine loss
      w.mat <- LMSquareLossIterations(x.mat[train.index, ], as.matrix(y.vec[train.index, ]), max.iteration)
      
      train.prediction <- as.matrix(x.mat[train.index, ]) %*% as.matrix(w.mat[-1,])
      train.loss <- (train.prediction - matrix( rep( y.vec[train.index,] , 10 ) , ncol = 10, byrow = TRUE )) ^ 2
      
      #validation loss
      validation.prediction <- x.mat[validation.index, ] %*% w.mat[-1,]
      validation.loss <-(validation.prediction - y.vec[validation.index, ]) ^ 2
      
      mean.train.loss <- colMeans(train.loss)
      mean.validation.loss <- colMeans(validation.loss)
      
      train.loss.mat[fold.index, ] = mean.train.loss
      validation.loss.mat[fold.index, ] = mean.validation.loss
    }
    
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    
    selected.steps <- which.min(mean.validation.loss.vec)
    w.mat <- LMSquareLossIterations(x.mat, y.vec, max.iteration = selected.steps)
    w.vec <- x.mat[, selected.steps]
    
    prediction <- function(test.mat) {
      prediction.vec <- testX.mat %*% weight.vec
    }
    
    results.list <-list(
        mean.validation.loss.vec = mean.validation.loss.vec,
        mean.train.loss.vec = mean.train.loss.vec,
        selected.steps = selected.steps,
        w.vec = w.vec,
        prediction = predict
      )
    return(results.list)
  }



#' Cross validation for linear model with logistic loss
#'
#' @param x.mat matrix of size [n x p]
#' @param y.vec of size n
#' @param fold.vec fold vector of size n
#' @param max.iteration integer > 1
#' @param step.size a number > 0, usually around .5

#'
#' @return result.list a list with mean.validation.loss.vec, mean.train.loss.vec, selected.steps, weight.vec, and predict function
#'
#' @export
#'
#' @examples
#' data <- data(spam, package="ElemStatLearn")
#' data.set<-spam[,-dim(spam)[2]]
#' x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
#' y.vec<-as.matrix(data.set[,dim(data.set)[2]])
#' max.iteration=10
#' fold.vec<-sample(rep(1:5,l=nrow(x.mat)))


# LMLogisticLossIterations(x.mat,y.vec,fold.vec,max.iteration)
LMLogisticLossEarlyStoppingCV <-function(x.mat, y.vec, fold.vec = NULL, max.iteration, step.size = 0.5) {
    
    num.folds <- length(unique(fold.vec))
    
    train.loss.mat <-matrix(0, nrow = num.folds, ncol = max.iteration)
    validation.loss.mat <-matrix(0, nrow = num.folds, ncol = max.iteration)
    
    for (fold.index in seq_len(num.folds)) {
      train.index <- which(fold.vec != fold.index)
      
      # go through all train validation splits
      for (trainvalid.split in c("train", "validation")) {
        if (trainvalid.split == "train") {
          validation.index <- which(fold.vec != fold.index)
        } else{
          validation.index <- which(fold.vec == fold.index)
        }
        
        w.mat <-LMLogisticLossIterations(x.mat[train.index,], as.matrix(y.vec[train.index]), max.iteration, step.size)
        
        if (trainvalid.split == "train") {
          #train.loss.mat[fold.index, ] <-colMeans(x.mat[validation.index,] %*% w.mat - as.matrix(y.vec[validation.index]))
          #cbind to add a column
          } else{
          #validation.loss.mat[fold.index, ] <-colMeans(cbind(x.mat)[validation.index,] %*% w.mat - y.vec[validation.index])
          
        }
      }
    }
    
    mean.train.loss <- colMeans(train.loss.mat)
    mean.validation.loss <- colMeans(validation.loss.mat)
    selected.steps <- which.min(mean.validation.loss)
    
    w.mat <-LMLogisticLossIterations(x.mat, y.vec, selected.steps, step.size)
    w.vec <- rbind(1, w.mat)[, selected.steps]
    
    
    prediction <- function(test.mat) {
      prediction.vec <-cbind(1, test.mat) %*% t(weight.vec)
      }
    
    
    results.list <-list(
        mean.validation.loss = mean.validation.loss.vec,
        mean.train.loss = mean.train.loss.vec,
        selected.steps = selected.steps,
        w.vec = w.vec,
        prediction = predict
      )
    
    return(results.list)
}
