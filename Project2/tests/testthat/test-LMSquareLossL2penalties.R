library(testthat)
library(linearmodels)
context("LMSquareLossL2penalties")

test_that("LMSquareLossL2penalties gives well formatted output", {
  data <- data(prostate, package="ElemStatLearn")
  data.set<-prostate[,-dim(prostate)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  penalty.vec <- c(5,4,3,2,1)
  out<-LMSquareLossL2penalties(x.mat,y.vec,penalty.vec)
  expect_equal(class(out),"matrix")
})

test_that("LMSquareLossL2penalties fails gracefully on bad input", {
  x.mat<-0
  y.vec<-0
  max.iterations=10
  penalty.vec <- 0
  out<-LMSquareLossL2penalties(x.mat,y.vec,penalty.vec)
  expect_equal(out,"bad input")
})