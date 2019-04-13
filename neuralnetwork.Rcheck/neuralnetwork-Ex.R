pkgname <- "neuralnetwork"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "neuralnetwork-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('neuralnetwork')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NNetEarlyStoppingCV")
### * NNetEarlyStoppingCV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NNetEarlyStoppingCV
### Title: neural network cross validation
### Aliases: NNetEarlyStoppingCV

### ** Examples

library(neuralnetwork)
data(ozone, package="ElemStatLearn")
head(ozone)
x.mat <- as.matrix(ozone[,-1])
y.vec <- ozone[,1]
n.hidden.units <- 4
n.folds <- 4
fold.vec <- sample(rep(1:n.folds), length(y.vec),TRUE)
step.size <- .5
max.iterations <-200
neuralnetwork::NNetEarlyStoppingCV(x.mat=x.mat,y.vec=y.vec,fold.vec=fold.vec,max.iterations=max.iterations,step.size=step.size,n.hidden.units=n.hidden.units,n.fold=n.folds)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NNetEarlyStoppingCV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NNetIterations")
### * NNetIterations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NNetIterations
### Title: neural network
### Aliases: NNetIterations

### ** Examples

library(neuralnetwork)
data(ozone, package="ElemStatLearn")
head(ozone)
x.mat <- as.matrix(ozone[,-1])
y.vec <- ozone[,1]
n.hidden.units <- 4
step.size <- .5
max.iterations <- 200
is.train <- !logical(nrow(x.mat))
neuralnetwork::NNetIterations(x.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NNetIterations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
