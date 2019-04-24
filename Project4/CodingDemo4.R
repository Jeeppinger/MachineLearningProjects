data(zip.train, package="ElemStatLearn")
all.y.vec<-zip.train[, 1]
is.01 <- all.y.vec %in% c(0,1)
y.vec <- all.y.vec[is.01]
x.mat<-zip.train[is.01, -1]
str(y.vec)
str(x.mat)

set.seed(1)
n.folds<- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(x.mat)))
validation.fold<- 1
is.train <- fold.vec != validation.fold

x.train<- x.mat[is.train,]
y.train <- y.vec[is.train]
y.tilde = ifelse(y.train ==1, 1, -1)
x.sc <- scale(x.train)
w <- rep(0, l=ncol(x.sc)) 
b<- 0

#the beta term is the first of w
#w <- rep(0, l=ncol(x.sc)+1) 

sigmoid<- function(z){
  1/(1+exp(-z)) 
}

grad.loss<-function(w.vec){
  #binary classification so logistic loss
  pred.vec <- x.sc %*% w
  prob.vec <- sigmoid(-pred.vec * y.tilde)
  .grad.vec <- - t(x.sc) %*% (y.tilde  * prob.vec)
}
d.vec <- -grad.loss(w)
step.size<- .5
u.vec<- w+step.sixe*d.vec


soft<- function (x, lambda){
  sign(x)*(abs(x) - lambda)
}
lambda <- 5
w <- soft(u.vec, step.size*lambda)