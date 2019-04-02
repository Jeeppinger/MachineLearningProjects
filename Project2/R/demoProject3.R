data(ozone, package="ElemStatLearn")
head(ozone)
x.unscaled.mat <- as.matrix(ozone[,-1])
x.scaled.mat <-scale(x.unscaled.mat)
y.vec <- ozone[,1]
num.hidden.units <- 2
V<- matrix(rnorm(ncol(x.scaled.mat)*num.hidden.units), ncol(x.scaled.mat), num.hidden.units)
A <- x.scaled.mat %*% V
sigmoid <- function(a){
  1/(1+exp(-a))
}
Z <- sigmoid(A)
w <- rnorm(num.hidden.units)
b <- as.numeric(Z %*% w)
delta.w <- b - y.vec
sigmoid.prime <- Z * (1-Z) 
delta.v <- diag(delta.w) %*% sigmoid.prime %*% diag(w)
grad.w <- t(Z) %*% delta.w / nrow(x.scaled.mat)
grad.v<- t(x.scaled.mat) %*% delta.v / nrow(x.scaled.mat) 
#now take a step
step.size <- .5
w<- w - step.size * grad.w
V<- V - step.size * grad.v
cost<- sum(abs(c(grad.w, as.numeric(grad.v)))) #find the minimum cost for L1