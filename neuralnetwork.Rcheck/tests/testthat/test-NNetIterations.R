library(testthat)
library(neuralnetwork)
library(ElemStatLearn)
context("NNetIterations")

test_that("NNetIterations gives well formatted output", {
  data(ozone, package="ElemStatLearn")
  head(ozone)
  x.mat <- as.matrix(ozone[,-1])
  y.vec <- ozone[,1]
  n.hidden.units <- 4
  step.size <- .5
  max.iterations <- 200
  is.train <- !logical(nrow(x.mat))
  result.list<-neuralnetwork::NNetIterations(x.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
  expect_equal(class(result.list$pred.mat),"matrix")
  expect_equal(class(result.list$V.mat),"matrix")
  expect_equal(class(result.list$w.vec),"matrix")
  expect_equal(class(result.list$predict),"function")
})

test_that("NNetIterations errors on bad input", {
  data(ozone, package="ElemStatLearn")
  head(ozone)
  x.mat <- 0
  y.vec <- 0
  n.hidden.units <- 0
  step.size <- .0
  max.iterations <- 0
  is.train <- 0
  result.list<-neuralnetwork::NNetIterations(x.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
  expect_equal(class(result.list),"character")
})