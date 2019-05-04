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
  grad.vec <- - t(x.sc) %*% (y.tilde  * prob.vec)
}
positive.part<- function (x){
  ifelse(x<0, 0, x)
}
soft<- function (x, lambda){
  sign(x)*positive.part(abs(x) - lambda)
}
l1opt<-function(w.vec, d){
  ifelse(w.vec==0, 
         positive.part(abs(d) - lambda), 
         abs(d-sign(w.vec) * lambda))
}
cost.step<- function(step){
  new.w<-w.step(step)
  cost.weight(w.step(new.w))
}
cost.weight<- function(w.vec){
  #pred.vec<-x.int%*%w.vec
  #loss.vec<-
}
w.step<- function(step){
  u.vec<- w+step.size*d.vec
  c(u.vec[1], soft(u.vec[-1], step.size*lambda))
}

lambda <- 5

d.vec <- -grad.loss(w)
step.size<- .5
u.vec<- w+step.size*d.vec
w <- soft(u.vec, step.size*lambda)

lambda.max<- max(abs(grad.vec))
