% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Predict1toMaxNeighbors.R
\name{NNLearnCV}
\alias{NNLearnCV}
\title{Cross Validation Function}
\usage{
NNLearnCV(x.mat, y.vec, max.neighbors = 30, fold.vec = NULL,
  n.folds = 5)
}
\arguments{
\item{x.mat}{[n x p] matrix of features}

\item{y.vec}{label column vector [n]}

\item{max.neighbors}{max number of neighbors}

\item{fold.vec}{empty vector}

\item{n.folds}{number folds}
}
\value{
numeric vector of size max.neighbors with predicitons from one to max.neighbors
}
\description{
A package that computes the k nearest neighbors 
This funciton calls the c interface in the package to calculate the k nearest neighbors for a given set of data
}
