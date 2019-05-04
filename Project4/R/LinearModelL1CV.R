#' Linear Model with L1 regularization using cross validation
#'
#' This algorithm splits the data into several folds and apply LinealModelL1penalites to each fold
#'
#' @param x.mat a numeric feature matrix of size n x p
#' @param y.vec a numeric labe vector of length nrow(x.mat)
#' @param fold.vec a numeric vector of lenght nrow(x.mat)
#' @param n.folds a positive integer indicate number of folds, default is 5
#' @param penalty.vec a non-negative numeric decreasing penalty vector, default is 1 to 0.1 with 0.1 decreament
#' @param step.size a positive numeric value, default is 0.1
#'
#' @return
#' @export
#'
#' @examples
LinearModelL1CV <-function(x.mat,y.vec,fold.vec = sample(rep(1:n.folds, l = length(y.vec))),n.folds = 5L,penalty.vec = seq(1, 0.1, -0.1),step.size = 0.1) {
    # Check types
  
    if (!all(is.numeric(x.mat), is.matrix(x.mat))) {
      stop("x.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),is.vector(y.vec), length(y.vec) == nrow(x.mat))) {
      stop("y.vec must be a numeric vector")
    }
    
    
    if (!all(is.numeric(fold.vec),is.vector(fold.vec),length(fold.vec) == nrow(x.mat))) {
      stop("fold.vec must be a numeric vector")
    }
    
    if (!all(is.vector(penalty.vec),is.numeric(penalty.vec),penalty.vec >= 0,diff(penalty.vec) < 0)) {
      stop("penalty.vec must be a decreasing numeric vector")
    }
    
    # Set up 
    is.binary <- ifelse((all(y.vec %in% c(0, 1))), TRUE, FALSE)
    
    n.cols <- ncol(x.mat)
    train.loss.mat <- matrix(0, nrow = n.folds, ncol = length(penalty.vec))
    validation.loss.mat <- matrix(0, nrow = n.folds, ncol = length(penalty.vec))
    
    # Iterating over each fold
    for (fold.idx in seq(n.folds)) {
      train.vec <- (fold.vec != fold.idx)
      
      set.list <- list(train = train.vec, validation = (!train.vec))
      for (set.name in names(set.list)) {
        index <- get(set.name, set.list)
        
        W.mat <-
          LinearModelL1penalties(x.mat, y.vec, penalty.vec, step.size)
        predict <- cbind(1, x.mat[index,]) %*% W.mat
        
        if (is.binary) {
          # Do 0-1 loss
          predict <- ifelse(predict > 0.5, 1, 0)
          loss.vec <-colMeans((ifelse(predict == y.vec[get(set.name, set.list)], 0, 1)))
        } else{
          # Do square loss
          loss.vec <-colMeans((predict - y.vec[get(set.name, set.list)]) ^ 2)
        }
        
        if (set.name == "train") {
          train.loss.mat[fold.idx, ] <- loss.vec
        } else{
          validation.loss.mat[fold.idx, ] <- loss.vec
        }
      }
    }
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.penalty.index <- which.min(mean.validation.loss.vec)
    
    weight.vec <- LinearModelL1penalties(x.mat, y.vec, penalty.vec,step.size)[, selected.penalty.index]
    
    predict <- function(testx.mat) {
      # Check inputs here
      if (!all(is.numeric(testx.mat),is.matrix(testx.mat), ncol(testx.mat) == n.cols)) {
        stop("testx.mat must be a numeric matrix ")
      }
      
      prediction.vec <- cbind(1, testx.mat) %*% weight.vec
      
      return(prediction.vec)
    }
    
    result.list <- list(
      mean.validation.loss.vec = mean.validation.loss.vec,
      mean.train.loss.vec = mean.train.loss.vec,
      penalty.vec = penalty.vec,
      selected.penalty = penalty.vec[selected.penalty.index],
      weight.vec = weight.vec,
      predict = predict
    )
    
    return(result.list)
}
