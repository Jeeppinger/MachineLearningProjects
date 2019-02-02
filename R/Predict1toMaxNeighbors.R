#' K nearest neighbors alogrithm
#'
#'A package that computes the k nearest neighbors 
#'This funcitons
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param testX.vec numeric feature vector for test [p]
#' @param max.neighbors max number of neighbors
#'
#' @return numeric vector of size max.neighbors with predicitons from one to max.neighbors
#' @export
#'
#' @examples
knn <- function(x.mat, y.vec, testX.vec, max.neighbors){
  result.list() <- .C("knn_interface", as.double(x.mat), as.double(y.vec), as.double(testX.vec),
                      as.integer(nrow(x.mat)), as.integer(ncol(x.mat)), as.integer(max.neighbors), 
                      predicitons=double(max.neighbors), PACKAGE="MachineLearningProjects") 
  result.list$predicitons
}
