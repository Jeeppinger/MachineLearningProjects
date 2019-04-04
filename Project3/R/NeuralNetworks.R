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