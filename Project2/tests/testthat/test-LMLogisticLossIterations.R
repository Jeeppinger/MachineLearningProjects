library(testthat)
library(linearmodels)
context("LMLogisticLossIterations")

test_that("LMLogisticLossIterations gives well formatted output", {
  data <- data(spam, package="ElemStatLearn")
  data.set<-spam[,-dim(spam)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  max.iterations=10
  w.mat=LMLogisticLossIterations(x.mat,y.vec,max.iterations)
  expect_equal(class(w.mat),"matrix")
})

test_that("LMLogisticLossIterations fails gracefully on bad input", {
  x.mat <- 0
  y.vec <- 0
  max.iterations <- 0
  w.mat = LMLogisticLossIterations(x.mat,y.vec,max.iterations)
  expect_equal(w.mat,"bad input")
})
