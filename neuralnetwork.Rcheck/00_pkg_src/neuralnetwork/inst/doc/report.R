## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width = 6-------------------------------------------------------
data <- data(prostate, package="ElemStatLearn")
data.set <- prostate[, -dim(prostate)[2]]
data.set$labels <- data.set[, dim(data.set)[2]]
data.set$features <- data.set[,-dim(data.set)[2]]
x.mat <- as.matrix(prostate[,-1])
y.vec <- prostate[,1]
max.iterations <- 5
n.folds <- 5
step.size <- .25
n.hidden.units <- 4
fold.vec <- sample(rep(1:n.folds,l=nrow(data.set$features)))
final.mat <- matrix(1:10, nrow = 5, dimnames = list(c("Fold-1","Fold-2","Fold-3","Fold-4","Fold-5"), c("Predictor","Baseline")))
for(test.fold in 1:n.folds){
  is.test <- fold.vec == test.fold
    is.train <- !is.test
    x.test <- x.mat[is.test,]
    y.train <- as.matrix(data.set$labels[is.train])
    baseline <- mean(y.train)
    sub.fold.vec<-sample(rep(1:5,l=nrow(x.mat)))
    early.stop.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=sub.fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
    #predictor <- early.stop.list$predict(x.test)
    valid.loss <- early.stop.list$mean.validation.loss.vec
    train.loss <- early.stop.list$mean.train.loss.vec
    steps <-early.stop.list$selected.steps
    plot(c(valid.loss),type="o", col="blue", pch="o", lty=1)
    points(c(train.loss), col="red", pch="*")
    lines(c(train.loss), col="red",lty=2)
    #final.mat[test.fold,1] = mean(predictor)
    final.mat[test.fold,2] = baseline
}
library(gridExtra)
library(grid)
d <- head(final.mat)
grid.table(d)

## ----fig.width = 6-------------------------------------------------------
data <- data(ozone, package="ElemStatLearn")
data.set <- ozone[, -dim(ozone)[2]]
data.set$labels <- data.set[, dim(data.set)[2]]
data.set$features <- data.set[,-dim(data.set)[2]]
x.mat <- as.matrix(ozone[,-1])
y.vec <- ozone[,1]
max.iterations <- 5
n.folds <- 5
step.size <- .25
n.hidden.units <- 4
fold.vec <- sample(rep(1:n.folds,l=nrow(data.set$features)))
final.mat <- matrix(1:10, nrow = 5, dimnames = list(c("Fold-1","Fold-2","Fold-3","Fold-4","Fold-5"), c("Predictor","Baseline")))
for(test.fold in 1:n.folds){
  is.test <- fold.vec == test.fold
    is.train <- !is.test
    x.test <- as.vector(x.mat[is.test,])
    y.train <- as.matrix(data.set$labels[is.train])
    baseline <- mean(y.train)
    sub.fold.vec<-sample(rep(1:5,l=nrow(x.mat)))
    early.stop.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=sub.fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
    #predictor <- early.stop.list$predict(x.test2)
    valid.loss <- early.stop.list$mean.validation.loss.vec
    train.loss <- early.stop.list$mean.train.loss.vec
    steps <-early.stop.list$selected.steps
    plot(c(valid.loss),type="o", col="blue", pch="o", lty=1)
    points(c(train.loss), col="red", pch="*")
    lines(c(train.loss), col="red",lty=2)
    #final.mat[test.fold,1] = mean(predictor)
    final.mat[test.fold,2] = baseline
}
library(gridExtra)
library(grid)
d <- head(final.mat)
grid.table(d)

## ----fig.width = 6-------------------------------------------------------
# data(spam, package="ElemStatLearn")
# data(zip.train, package = "ElemStatLearn")
# is.01 <- zip.train[,1] %in% c(0,1)
# data.list <- list(
#   spam=list(
#     features=as.matrix(spam[,1:57]),
#     labels=ifelse(spam$spam=="spam",1,0)),
#   zip.train=list(
#     features=zip.train[is.01,-1],
#     labels=as.integer(zip.train[is.01,1])))
# n.folds <- 5
# max.iterations <- 5
# step.size <- .25
# n.hidden.units <- 4
# final.mat <- matrix(1:10, nrow = n.folds, dimnames = list(c("Fold-1","Fold-2","Fold-3","Fold-4","Fold-5"), c("Predictor","Baseline")))
# for(data.name in names(data.list)){
#   data.set <- data.list[[data.name]]
#   stopifnot(all(data.set$labels %in% c(0,1)))
#   stopifnot(length(data.set$labels)==nrow(data.set$features))
#   set.seed(1)
#   fold.vec <- sample(rep(1:n.folds,l=nrow(data.set$features)))
#   result.mat.list <- list()
#   for(test.fold in 1:n.folds){
#     is.test <- fold.vec == test.fold
#     is.train <- !is.test
#     x.mat <- as.matrix(data.set$features[is.train,])
#     y.vec <- as.vector(data.set$labels[is.train])
#     sub.fold.vec <- as.vector(sample(rep(1:n.folds,l=nrow(x.mat))))
#     mode <- unique(y.vec)
#     baseline <- mode[which.max(tabulate(match(y.vec, mode)))]
#     ##plot(fit$mean.loss$mean.loss)
#     early.stop.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=sub.fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
#     x.test <- x.mat[,-1]
#     #predictor <- early.stop.list$predict(x.test)
#     valid.loss <- early.stop.list$mean.validation.loss.vec
#     train.loss <- early.stop.list$mean.train.loss.vec
#     steps <-early.stop.list$selected.steps
#     plot(c(valid.loss),type="o", col="blue", pch="o", lty=1)
#     points(c(train.loss), col="red", pch="*")
#     #final.mat[test.fold,1] = mean(predictor)
#     final.mat[test.fold,2] = baseline
#   }
# }
# library(gridExtra)
# library(grid)
# d <- head(final.mat)
# grid.table(d)

## ----fig.width = 6-------------------------------------------------------
# data(SAheart, package="ElemStatLearn")
# data.set <- SAheart[, -dim(SAheart)[2]]
# data.set$labels <- data.set[, dim(data.set)[2]]
# data.set$features <- data.set[,-dim(data.set)[2]]
# set.seed(1)
# n.folds <- 5
# fold.vec <- sample(rep(1:n.folds,l=nrow(data.set$features)))
# step.size <- .25
# n.hidden.units <- 4
# final.mat <- matrix(1:10, nrow = 5, dimnames = list(c("Fold-1","Fold-2","Fold-3","Fold-4","Fold-5"), c("Predictor","Baseline")))
# for(test.fold in 1:n.folds){
#   is.test <- fold.vec == test.fold
#   is.train <- !is.test
#   x.mat <- as.matrix(data.set$features[is.train,])
#   for(row in 1:nrow(x.mat)){
#     if(x.mat[row,5] == "Absent"){
#       x.mat[row,"famhist"] = as.numeric(0)
#     } else{
#       x.mat[row,"famhist"] = as.numeric(1)
#     }
#   }
#     y.vec <- as.vector(data.set$labels[is.train])
#     sub.fold.vec <- as.vector(sample(rep(1:n.folds,l=nrow(x.mat))))
#     mode <- unique(y.vec)
#     baseline <- mode[which.max(tabulate(match(y.vec, mode)))]
#     ##plot(fit$mean.loss$mean.loss)
#     early.stop.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=sub.fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
#     
#     x.test <- x.mat[,-1]
#     #predictor <- early.stop.list$predict(x.test)
#     valid.loss <- early.stop.list$mean.validation.loss.vec
#     train.loss <- early.stop.list$mean.train.loss.vec
#     steps <-early.stop.list$selected.steps
#     plot(c(valid.loss),type="o", col="blue", pch="o", lty=1)
#     points(c(train.loss), col="red", pch="*")
#     #final.mat[test.fold,1] = mean(predictor)
#     final.mat[test.fold,2] = baseline
#   }
# library(gridExtra)
# library(grid)
# d <- head(final.mat)
# grid.table(d)

## ----fig.width = 6-------------------------------------------------------
# data(spam, package="ElemStatLearn")
# data(zip.train, package = "ElemStatLearn")
# is.01 <- zip.train[,1] %in% c(0,1)
# data.list <- list(
#   spam=list(
#     features=as.matrix(spam[,1:57]),
#     labels=ifelse(spam$spam=="spam",1,0)),
#   zip.train=list(
#     features=zip.train[is.01,-1],
#     labels=as.integer(zip.train[is.01,1])))
# n.folds <- 5
# max.iterations <- 5
# step.size <- .25
# n.hidden.units <- 4
# final.mat <- matrix(1:10, nrow = 5, dimnames = list(c("Fold-1","Fold-2","Fold-3","Fold-4","Fold-5"), c("Predictor","Baseline")))
# for(data.name in names(data.list)){
#   data.set <- data.list[[data.name]]
#   stopifnot(all(data.set$labels %in% c(0,1)))
#   stopifnot(length(data.set$labels)==nrow(data.set$features))
#   set.seed(1)
#   fold.vec <- sample(rep(1:n.folds,l=nrow(data.set$features)))
#   result.mat.list <- list()
#   for(test.fold in 1:n.folds){
#     is.test <- fold.vec == test.fold
#     is.train <- !is.test
#     x.mat <- as.matrix(data.set$features[is.train,])
#     y.vec <- as.vector(data.set$labels[is.train])
#     sub.fold.vec <- as.vector(sample(rep(1:n.folds,l=nrow(x.mat))))
#     mode <- unique(y.vec)
#     baseline <- mode[which.max(tabulate(match(y.vec, mode)))]
#     ##plot(fit$mean.loss$mean.loss)
#     early.stop.list <- neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=sub.fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)
#     x.test <- x.mat[,-1]
#     #predictor <- early.stop.list$predict(x.test)
#     valid.loss <- early.stop.list$mean.validation.loss.vec
#     train.loss <- early.stop.list$mean.train.loss.vec
#     steps <-early.stop.list$selected.steps
#     plot(c(valid.loss),type="o", col="blue", pch="o", lty=1)
#     points(c(train.loss), col="red", pch="*")
#     #final.mat[test.fold,1] = mean(predictor)
#     final.mat[test.fold,2] = baseline
#   }
# }
# library(gridExtra)
# library(grid)
# d <- head(final.mat)
# grid.table(d)

