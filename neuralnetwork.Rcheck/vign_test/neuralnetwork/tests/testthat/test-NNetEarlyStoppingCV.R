library(testthat)
library(neuralnetwork)
library(ElemStatLearn)
context("NNetEarlyStoppingCV")

test_that("NNetEarlyStoppingCV gives well formatted output", {
  data(ozone, package="ElemStatLearn")
  head(ozone)
  x.mat <- as.matrix(ozone[,-1])
  y.vec <- ozone[,1]
  n.hidden.units <- 4
  n.folds <- 4
  fold.vec <- sample(rep(1:n.folds), length(y.vec),TRUE)
  step.size <- .5
  max.iterations <- 200
  result.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
  expect_equal(class(result.list),"list")
  expect_equal(class(result.list$predict),"function")
  expect_equal(class(result.list$selected.steps),"integer")
  expect_equal(class(result.list$mean.validation.loss.vec),"numeric")
  expect_equal(class(result.list$mean.train.loss.vec),"numeric")
})

test_that("NNetEarlyStoppingCV error correctly on bad data", {
  data(ozone, package="ElemStatLearn")
  head(ozone)
  x.mat <- 0
  y.vec <- 0
  n.hidden.units <- 0
  n.folds <- 0
  fold.vec <- 0
  step.size <- 0
  max.iterations <- 0
  result.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
  expect_equal(class(result.list),"character")
})