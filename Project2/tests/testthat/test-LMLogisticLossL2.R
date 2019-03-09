library(testthat)
library(linearmodels)
context("LMLogisticLossL2")

test_that("LMLogisticLossL2 gives well formatted output", {
  data <- data(spam, package="ElemStatLearn")
  data.set<-spam[,-dim(spam)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  penalty<-5
  max.iterations=10
  w.vec <- LMLogisticLossIterations(x.mat,y.vec,max.iterations)[,9]
  LMLogisticLossL2(x.mat,y.vec,penalty,initial.weight.vec=w.vec)
  expect_equal(class(w.vec),"numeric")
})

test_that("LMLogisticLossL2 fails gracefully on bad input", {
  x.mat <- 0
  y.vec <- 0
  max.iterations <- 0
  w.vec <- 0
  penalty<-0
  w.mat<-LMLogisticLossL2(x.mat,y.vec,penalty,initial.weight.vec=w.vec)
  expect_equal(w.mat,"bad input")
})