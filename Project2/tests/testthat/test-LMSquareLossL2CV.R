library(testthat)
library(linearmodels)
context("LMSquareLossL2CV")

test_that("LMSquareLossL2CV gives well formatted output", {
  data <- data(prostate, package="ElemStatLearn")
  data.set<-prostate[,-dim(prostate)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  fold.vec<-sample(rep(1:5,l=nrow(x.mat)))
  penalty.vec <- c(5,4,3,2,1)
  out<-LMSquareLossL2CV(x.mat,y.vec,fold.vec,penalty.vec)
  expect_equal(class(out),"list")
})

test_that("LMSquareLossL2CV fails gracefully on bad input", {
  x.mat<-0
  y.vec<-0
  max.iterations=10
  fold.vec<-0
  penalty.vec <- 0
  out<-LMLogisticLossL2CV(x.mat,y.vec,fold.vec,penalty.vec)
  expect_equal(out,"bad input")
})