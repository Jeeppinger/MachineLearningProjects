#' K nearest neighbors alogrithm
#'
#'A package that computes the k nearest neighbors 
#'This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param testx.vec numeric feature vector for test [p]
#' @param max.neighbors max number of neighbors
#'
#' @return numeric vector of size max.neighbors with predicitons from one to max.neighbors
#' @export
#'
#' @examples
#' data(zip.train, package="ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' knn(x, y, testx, 3)
#' zip.train[test.i, 1]

knn <- function(x.mat, y.vec, testx.vec, max.neighbors){
  result.list <- .C("knn_interface", as.double(x.mat), as.double(y.vec), as.double(testx.vec),
                      as.integer(nrow(x.mat)), as.integer(ncol(x.mat)), as.integer(max.neighbors), 
                      predicitons=double(max.neighbors), PACKAGE="nearestneighbors")
  result.list$predicitons
}
0

#' Cross Validation Function
#'
#'A package that computes the k nearest neighbors 
#'This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
#'
#' @param x.mat  [n x p] matrix of features
#' @param y.vec label column vector [n]
#' @param max.neighbors max number of neighbors
#' @param fold.vec empty vector
#' @param n.folds number folds
#' 
#'
#' @return numeric vector of size max.neighbors with predicitons from one to max.neighbors
#' @export
#'
#' @examples
#' library(nearestneighbors)
#' data(ozone, package="ElemStatLearn")
#' head(ozone)
#' x.mat <- as.matrix(ozone[,-1])
#' y.vec <- ozone[,1]
#' max.neighbors <- 4
#' n.folds <- 20
#' fold.vec <- sample(rep(1:n.folds), length(y.vec),TRUE)
#' nearestneighbors::NNLearnCV(x.mat=x.mat, y.vec=y.vec, max.neighbors=max.neighbors, fold.vec=fold.vec, n.folds=n.folds)
 NNLearnCV <- function(x.mat, y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5){
   fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
   
   for(fold.i in seq_along(unique.folds)){
     for(prediction.set.name in c("train", "validation")){
      pred.mat <- NN1toKmaxPredict(
         train.features, train.labels,
         prediction.set.features, max.neighbors)
       loss.mat <- if(labels.all.01){
         ifelse(pred.mat>0.5, 1, 0) != y.vec #zero-one loss for binary classification.
       }else{
         (pred.mat-y.vec)^2 #square loss for regression.
       }
       train.or.validation.loss.mat[, fold.i] <- colMeans(loss.mat)
     }
   }
   
 }


