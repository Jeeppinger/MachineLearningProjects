library(nearestneighbors)
library(testthat)
context("predict1toMaxNeighbors")

test_that("predict1toMaxNeighbors computes same answer as R", {
  data(zip.train, package="ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i, -1]
  y <- zip.train[train.i, 1]
  testx.vec <- zip.train[test.i, -1]
  max.neighbors <- 4
  pred.vec <- Predict1toMaxNeighbors(x, y, testx.vec, max.neighbors)
  dist.mat <- t(x) - testx.vec
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max.neighbors]
  expected.prediction <- cumsum(y[closest.indices])/(1:max.neighbors)
  expect_equal(pred.vec,expected.prediction)
})
test_that("predict1toMaxNeighbors Fails gracefully when invalid # of neighbors", {
  data(zip.train, package="ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i, -1]
  y <- zip.train[train.i, 1]
  testx.vec <- zip.train[test.i, -1]
  max.neighbors <- -2
  pred.vec <- Predict1toMaxNeighbors(x, y, testx.vec, max.neighbors)
  dist.mat <- t(x) - testx.vec
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max.neighbors]
  expected.prediction <- cumsum(y[closest.indices])/(1:max.neighbors)
  expect_equal(pred.vec,expected.prediction)
})
