#' Linear square loss
#'
#'A package that computes the k nearest neighbors 
#'This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param max.iterations max number of steps to take on gradient descent
#' @param step.size how big of a step to take for each iteration
#'
#' @return w.mat a weight vector with one weight per iteration
#' @export
#'
#' @examples

LMSquareLossIterations <- function(x.mat, y.vec,max.iterations ,step.size){
  w.mat <- .C("loss_interface", as.double(x.mat), as.double(y.vec), as.integer(max.iterations),
                    as.double(step.size), PACKAGE="linearmodels")
  
}