library(testthat)
library(linearmodels)
library(ElemStatLearn)
context("LinearModelL1")

test_that("LinearModelL1 gives well formatted output", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- all.y.vec[is.01]
  x.mat <- zip.train[is.01,-1]
  n.cols <- ncol(x.mat)
  initial.weight.vec <- rep(0, n.cols + 1)
  penalty <- 1
  opt.thresh <- 1
  weight.vec <- LinearModelL1(x.mat,y.vec,penalty,opt.thresh,initial.weight.vec)
  expect_equal(class(weight.vec),"numeric")
})

test_that("LinearModelL1 error correctly on bad data", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- as.matrix(all.y.vec[is.01])
  x.mat <- zip.train[is.01,-1]
  n.cols <- ncol(x.mat)
  initial.weight.vec <- rep(0, n.cols + 1)
  penalty <- 1
  opt.thresh <- 1
  weight.vec <- try(LinearModelL1(x.mat,y.vec,penalty,opt.thresh,initial.weight.vec))
  expect_equal(is(weight.vec,"try-error"),TRUE)
})
