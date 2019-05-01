#' Linear Model with L1 regularization
#'
#' This algorithm takes the penalty vector as input and iterate through each value in the vector using LinearModelL1
#'
#' @param x.mat matrix of size [n x p]
#' @param y.vec vector of size n
#' @param penalty.vec vector of decreasing penalty values
#' @param step.size a step size for each step
#'
#' @return optimal.weight.vector

#' @export
#'
#' @examples
LinearModelL1penalties <- function(x.mat, y.vec, penalty.vec, step.size) {
    # Check type and dimension
    if (!all(is.numeric(x.mat), is.matrix(x.mat))) {
      stop("x.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec), is.vector(y.vec), length(y.vec) == nrow(x.mat))) {
      stop("y.vec must be a numeric vector")
    }
    
    is.decreasing <- function(vec) {
      result <- all(diff(vec) < 0)
      return(result)
    }
    
    if (!all( is.vector(penalty.vec), is.numeric(penalty.vec), penalty.vec >= 0,is.decreasing(penalty.vec))) {
      stop("penalty.vec must be a non-negative decreasing numeric vector")
    }
    
    if (!all(is.numeric(opt.thresh),length(opt.thresh) == 1, opt.thresh > 0)) {
      stop("opt.thresh must be a numeric scalar > 0")
    }
    
    # Initializing
    n.rows <- nrow(x.mat)
    n.cols <- ncol(x.mat) # p
    n.penalties <- length(penalty.vec)
    
    # scale x.mat with m = 0, sd = 1
    col.mean.vec <- colMeans(x.mat)
    col.sd.vec <- sqrt(rowSums((t(x.mat) - col.mean.vec) ^ 2) / n.rows)
    
    # columns with zero variance will become zero at the end
    col.sd.vec[col.sd.vec == 0] <- 1
    
    col.sd.mat <- diag(1 / col.sd.vec)
    
    x.scaled.mat <-t((t(x.mat) - col.mean.vec) / col.sd.vec)
    
    initial.weight.vec <- rep(0, n.cols + 1)
    
    w.mat <- matrix(0, nrow = n.cols + 1, ncol = n.penalties)
    # w.temp.mat <- w.mat
    
    for (penalty.idx in c(1:n.penalties)) {
      w.mat[, penalty.idx] <-LinearModelL1(x.scaled,y.vec,penalty.vec[penalty.idx],opt.thresh,initial.weight.vec,step.size)
      
      initial.weight.vec <-w.mat[, penalty.idx] 
    }
    
    intercept.vec <--col.mean.vec %*% col.sd.mat %*% w.mat[-1,] + w.mat[1,] # w.mat is the beta.vec
    w.mat <- rbind(intercept.vec, col.sd.mat %*% w.mat[-1,])
    
    return(w.mat)
  }