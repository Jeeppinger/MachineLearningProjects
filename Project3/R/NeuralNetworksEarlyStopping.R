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

NNetEarlyStoppingCV <-
  function(x.mat, y.vec, fold.vec = sample(rep(1:n.folds), length(y.vec)), max.iterations, step.size, n.hidden.units, n.folds = 4) {
    
    is.binary <- all(y.vec %in% c(-1,0,1))
    #set n folds correctly
    n.folds <- length(unique(fold.vec))
    
    #n.feature <- ncol(x.mat)
    #n.observation <- nrow(x.mat)
    
    #delare zero matrices
    validation.loss.mat <-matrix(0, nrow = n.folds, ncol = max.iterations)
    loss.mat <-matrix(0, nrow = n.folds, ncol = max.iterations)
    train.loss.mat <-matrix(0, nrow = n.folds, ncol = max.iterations)
    
    sigmoid <- function(x){
      1/(1 + exp(-x))
    }
    
    # iterate through each of the folds
    for (fold.idx in seq(n.folds)) {
      train.index <- which(fold.vec != fold.idx)
      validation.index <- which(fold.vec == fold.idx)
      train.vec <- (fold.vec != fold.idx)
      
      temp.list <-NNetIterations(x.mat, y.vec, max.iterations, step.size, n.hidden.units, train.vec)
      
      V.mat <- temp.list$V.mat
      w.vec <- temp.list$w.vec
      
      #list of what is the training data and what is the validation data
      set.list <- list(train = fold.vec != fold.idx, validation = fold.vec == fold.idx)
      for(set.name in names(set.list)){
        #get our prediction for that data
        prediction <- temp.list$pred.mat[set.list$set.name,]
        
        #determine the loss
        if(is.binary){
          # Do 0-1 loss
          prediction <- ifelse(prediction > 0.5, 1, 0)
          loss.mat[fold.idx,] <- colMeans((ifelse(prediction == y.vec[set.list$set.name], 0, 1)))
        }else{
          # Do square loss
          loss.mat[fold.idx,] <- colMeans((prediction - y.vec[set.list$set.name])^2)
        }
        #add the loss to our matrix
        if(set.name == "train"){
          train.loss.mat <- loss.mat
        }else{
          validation.loss.mat <- loss.mat
        }
      }
      
    }
    #determine the mean loss
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    mean.train.loss.vec <- colMeans(train.loss.mat)
    #pick the step size that had minimum loss
    selected.steps <- which.min(mean.train.loss.vec)
    
    #get the best step size results
    result.list <- NNetIterations(x.mat, y.vec, selected.steps, step.size, n.hidden.units, as.vector(rep(1,nrow(x.mat))))
    
    
    #add to the list
    result.list$mean.validation.loss.vec = mean.validation.loss.vec
    result.list$mean.train.loss.vec = mean.train.loss.vec
    result.list$selected.steps = selected.steps
    
    return(result.list)
    
  }