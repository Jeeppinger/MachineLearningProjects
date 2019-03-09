library(testthat)
library(linearmodels)
context("LMSquareLossEarlyStoppingCV")

test_that("LMSquareLossEarlyStoppingCV gives well formatted output", {
  data <- data(prostate, package="ElemStatLearn")
  data.set<-prostate[,-dim(prostate)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  max.iterations=10
  fold.vec<-sample(rep(1:5,l=nrow(x.mat)))
  
  result.list=LMSquareLossEarlyStoppingCV(x.mat,y.vec,fold.vec,max.iterations)
  expect_equal(class(result.list),"list")
})

test_that("LMSquareLossIterations fails gracefully on bad input", {
  x.mat <- 0
  y.vec <- 0
  max.iterations <- 0
  fold.vec <- 0
  result.list = LMSquareLossEarlyStoppingCV(x.mat,y.vec,fold.vec,max.iterations)
  expect_equal(result.list,"bad input")
})
