library(testthat)
library(linearmodels)
library(ElemStatLearn)
context("LinearModelL1CV.R")

test_that("LinearModelL1CV gives well formatted output", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- all.y.vec[is.01]
  x.mat <- zip.train[is.01,-1]
  result.list <- LinearModelL1CV(x.mat,y.vec)
  expect_equal(class(result.list),"list")
})

test_that("LinearModelL1CV error correctly on bad data", {
  library(linearmodels)
  data(zip.train, package="ElemStatLearn")
  all.y.vec<-zip.train[, 1]
  is.01 <- all.y.vec %in% c(0,1)
  y.vec <- as.matrix(all.y.vec[is.01])
  x.mat <- zip.train[is.01,-1]
  result.list <- try(LinearModelL1CV(x.mat,y.vec))
  expect_equal(is(result.list,"try-error"),TRUE)
})
