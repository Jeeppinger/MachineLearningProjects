#' @title neural network cross validation
#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param fold.vec vector of size n containing folds
#' @param max.iterations integer > 1
#' @param step.size integer > 1
#' @param n.hidden.units nunmber of hidden units >= 1
#' @param n.folds number of folds >=1
#' 
#' @return results.list a list with pred.mat, v.mat, w.vec, predict function that takes (testx.mat), mean.validaiton.loss.vec, selected.steps
#'
#' @export
#'
#' @examples
#' library(neuralnetwork)
#' data(ozone, package="ElemStatLearn")
#' head(ozone)
#' x.mat <- as.matrix(ozone[,-1])
#' y.vec <- ozone[,1]
#' n.hidden.units <- 4
#' n.folds <- 4
#' fold.vec <- sample(rep(1:n.folds), length(y.vec),TRUE)
#' step.size <- .5
#' max.iterations <-200
#' neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)

NNetEarlyStoppingCV <-
  function(x.mat, y.vec, fold.vec = sample(rep(1:n.folds), length(y.vec),TRUE), max.iterations, step.size, n.hidden.units, n.folds = 4) {
    # error checking
    if(class(x.mat)!="matrix"){
      return("ERROR: x.mat is not a matrix")
    }
    if(nrow(x.mat)!=length(y.vec)) {
      return("ERROR: Mismatching x.mat and y.vec dimensions")
    }
    if(class(fold.vec)!="integer"){
      return("ERROR: fold.vec is not a vector")
    }
    if(class(max.iterations)!="numeric"){
      return("ERROR: max.iterations is not an numeric")
    }
    if(class(step.size)!="numeric"){
      return("ERROR: step.size is not a numeric")
    }
    if(class(n.hidden.units)!="numeric"){
      return("ERROR: n.hidden.units is not an numeric")
    }
    if(class(n.folds)!="numeric"){
      return("ERROR: n.folds is not an numeric")
    }
    is.binary <- all(y.vec %in% c(-1,0,1))
    #set n folds correctly
    n.folds <- length(unique(fold.vec))
    
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
      head(x.mat)
      temp.list <-NNetIterations(as.matrix(x.mat), as.numeric(y.vec), as.numeric(max.iterations), as.numeric(step.size), as.numeric(n.hidden.units), as.logical(train.vec))
      
      V.mat <- temp.list$V.mat
      w.vec <- temp.list$w.vec
      
      #list of what is the training data and what is the validation data
      set.list <- list(train = fold.vec != fold.idx, validation = fold.vec == fold.idx)
     
       for(set.name in names(set.list)){
        #get our prediction for that data
        prediction <- temp.list$pred.mat
        #determine the loss
        if(is.binary){
          # Do 0-1 loss
          prediction <- ifelse(prediction > 0.5, 1, 0)
          loss.mat[fold.idx,] <- colMeans((ifelse(prediction == y.vec[train.vec], 0, 1)))
        }else{
          # Do square loss
          loss.mat[fold.idx,] <- colMeans((prediction - y.vec[train.vec])^2)
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
    is.train <- !logical(nrow(x.mat))
    result.list <- NNetIterations(as.matrix(x.mat), as.numeric(y.vec), as.numeric(selected.steps), as.numeric(step.size), as.numeric(n.hidden.units), as.logical(is.train))
    
    
    #add to the list
    result.list$mean.validation.loss.vec = mean.validation.loss.vec
    result.list$mean.train.loss.vec = mean.train.loss.vec
    result.list$selected.steps = selected.steps
    
    return(result.list)
    
  }
