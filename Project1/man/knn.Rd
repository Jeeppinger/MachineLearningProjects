% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Predict1toMaxNeighbors.R
\name{knn}
\alias{knn}
\title{K nearest neighbors alogrithm}
\usage{
knn(x.mat, y.vec, testx.vec, max.neighbors)
}
\arguments{
\item{x.mat}{[n x p] matrix of features}

\item{y.vec}{label column vector [n]}

\item{testx.vec}{numeric feature vector for test [p]}

\item{max.neighbors}{max number of neighbors}
}
\value{
numeric vector of size max.neighbors with predicitons from one to max.neighbors
}
\description{
A package that computes the k nearest neighbors 
This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
}
\examples{
data(zip.train, package="ElemStatLearn")
i01 <- which(zip.train[,1] \%in\% c(0,1))
train.i <- i01[1:5]
test.i <- i01[6]
x <- zip.train[train.i, -1]
y <- zip.train[train.i, 1]
testx <- zip.train[test.i, -1]
knn(x, y, testx, 3)
zip.train[test.i, 1]
}
