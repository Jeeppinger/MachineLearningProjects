#'Linear Model algorithm
#'
#' @param x.scaled.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param penalty non negative numeric scalar
#' @param opt.thresh positive numeric scalar
#' @param initial.weight.vec vector of initial weights
#' @param step.size integer > 1
#' 
#'@return optimal.weight.vector
#' @export
#'
#' @examples
LinearModelL1 <- function(x.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size) {
  #1/n ∑i=1^n L[w^T x_i + b, y_i] + penalty * ||w||_1
  #trying to use the cost function written above
}

#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param penalty.vec vector of decreasing penalty values
#'
#' @return optimal.weight.vector

LinearModelL1penalties <- function(x.mat, y.vec, penalty.vec) {
  #first scale x mat
  #then loop over pentalty values
  #finally unscale the optimal weight vector
}

#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param fold.vec
#' @param n.folds
#' @param penalty.vec vector of decreasing penalty values
#' @param step.size
#'
#' @return optimal.weight.vector

LinearModelL1CV <- function(x.mat, y.vec, fold.vec, n.folds, penalty.vec, step.size) {
  
}

