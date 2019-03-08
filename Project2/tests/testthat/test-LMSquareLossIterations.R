library(testthat)
library(linearmodels)
context("LMSquareLossIterations")

test_that("LMSquareLossIterations gives well formatted output", {
  data <- data(prostate, package="ElemStatLearn")
  data.set<-prostate[,-dim(prostate)[2]]
  x.mat<-as.matrix(data.set[,-dim(data.set)[2]])
  y.vec<-as.matrix(data.set[,dim(data.set)[2]])
  max.iterations=10
  w.mat=LMSquareLossIterations(x.mat,y.vec,max.iterations)
  expect_equal(class(w.mat),"matrix")
})

test_that("LMSquareLossIterations fails gracefully on bad input", {
  x.mat <- 0
  y.vec <- 0
  max.iterations <- 0
  w.mat = LMSquareLossIterations(x.mat,y.vec,max.iterations)
  expect_equal(w.mat,"bad input")
})