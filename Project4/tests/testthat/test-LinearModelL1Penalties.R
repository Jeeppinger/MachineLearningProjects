library(testthat)
library(linearmodels)
library(ElemStatLearn)
context("LinearModelL1Penalties")

test_that("LinearModelL1Penalties gives well formatted output", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- all.y.vec[is.01]
  x.mat <- zip.train[is.01,-1]
  penalty.vec <- c(5,4,3,2,1)
  step.size <- .5
  weight.mat <- LinearModelL1penalties(x.mat,y.vec,penalty.vec,step.size)
  expect_equal(class(weight.mat),"matrix")
})

test_that("LinearModelL1Penalties error correctly on bad data", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- "fail"
  x.mat <- zip.train[is.01,-1]
  penalty.vec <- c(5,4,3,2,1)
  step.size <- .5
  weight.mat <- try(LinearModelL1penalties(x.mat,y.vec,penalty.vec,step.size))
  expect_equal(is(weight.mat,"try-error"),TRUE)
})
