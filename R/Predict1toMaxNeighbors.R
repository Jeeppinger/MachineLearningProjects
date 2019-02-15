#' K nearest neighbors alogrithm
#'
#'A package that computes the k nearest neighbors 
#'This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
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
#' data(zip.train, package="ElemStatLearn")
#' i01 <- which(zip.train[,i] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' knn(x, y, testx, 3)
#' zip.train[test.i, 1]
knn <- function(x.mat, y.vec, testx.vec, max.neighbors){
  result.list() <- .C("knn_interface", as.double(x.mat), as.double(y.vec), as.double(testx.vec),
                      as.integer(nrow(x.mat)), as.integer(ncol(x.mat)), as.integer(max.neighbors), 
                      predicitons=double(max.neighbors), PACKAGE="nearestneighbors") 
  result.list$predicitons
}


