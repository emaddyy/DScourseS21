# ECON 5253
# Ethan Maddy
# 4/1/2021
# PS8

# set up 
rm(list = ls()) 

library(glmnet)
library(magrittr)
library(tidyverse)
library(nloptr)
library(modelsummary)

# Question no. 4
set.seed(100)
X <- matrix(rnorm(100000*10,mean=0,sd=0.5),nrow=100000,ncol=10)
X[,1] <- 1 # first column (X = 1)

# epsilon
eps <- rnorm(n=100000, mean=0, sd=0.5)

# beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Y
Y <- X %*% beta + eps

# Question no. 5

betahatOLS <- as.vector(solve(crossprod(x)) %*% crossprod(x,Y))
betaOLS <- tibble(truth=beta, estimates=betahatOLS)

tibble(betahatOLS)
betaOLS

# Estimates are very close to the true value of beta
# betahatOLS is nearly identical to true beta


# Question no. 6

# step rate & iter
alpha <- 0.0000003
m <- 500000

# define gradient
gradient <- function(beta,Y,x){
  return(as.vector(-2*t(x)%*%(Y-x%*%beta)))}

# vector for all beta and steps
ALLbeta <- matrix("numeric",length(Beta),m)

# find minimum
iter  <- 1
beta0 <- 0*Beta
beta <- runif(length(Beta))
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,x)
  beta.All[,iter] <- Beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)

# estimates 
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))
beta


# Question no. 7 

# objective function 
obj_fun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

# gradient
gradient <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)))
}

# load data
y <- Y
X <- X

# beta0
beta0 <- runif(dim(X)[2])
beta0

# parameters
options <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-6, "maxeval"=1e3)

# results
resultBFGS <- nloptr(x0=beta0, eval_f=obj_fun, eval_grad_f=gradient, opts=options, y=y, X=X)
print(resultBFGS)
# Optimal value of objective function:  24953.283521585 
# Optimal value of controls: 1.500579 -0.9912364 -0.2472997 0.7443806 3.503534 -1.998873 0.5022677 0.9974801 1.25566 1.998769

# looks good 
beta

# Now Nelder-Mead
# Objective function no. 2
obj_fun2 <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

# load data
y <- Y
X <- X

# beta0
beta0 <- runif(dim(X)[2])

# parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1.0e-8, "maxeval"=1e3)

# results
resultNM <- nloptr(x0=beta0, eval_f=obj_fun2, opts=options, y=y, X=X)
print(resultNM)

# Current value of objective function:  25205.9315860045 
# Current value of controls: 1.462162 -0.9774386 -0.2335251 0.7746308 3.506977 -1.970992 
# 0.5374785 0.9711296 1.243423 2.001743

# looks good (but not quite as good)
beta

# NM vs BFGS
resultNM
resultBFGS
# slight difference between the two, BFGS is closer to true beta 


# Question no. 8

# objective function 
obj_fun3  <- function(theta,y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

# gradient
gradient <- function (theta,y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod(y-X%*%beta)/(sig^3)
  return ( grad )
}

# load data
y <- Y
X <- X

# theta0
theta0 <- runif(dim(X)[2]+1)

# parameters
options <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-6, "maxeval"=1e4)

# results
results <- nloptr(x0=theta0, eval_f=obj_fun3, eval_grad_f=gradient, opts=options, y=y, X=X)
print(results)

# Optimal value of objective function:  72485.6149015636 
# Optimal value of controls: 1.500579 -0.9912364 -0.2472996 0.7443806 3.503534 -1.998873 
# 0.5022677 0.9974801 1.25566 1.998769 -0.4995326



# Question no. 9

# beta w/ lm() function
betaOLS <- lm(Y ~ X -1)
summary(betaOLS)

# similar to true beta
beta

# model summary
modelsummary(betaOLS, output = "latex")
modelsummary(betaOLS, output = "markdown")

#  |         |  Model 1   |
#  |:--------|:----------:|
#  |X1       |   1.501    |
#  |         |  (0.002)   |
#  |X2       |   -0.991   |
#  |         |  (0.003)   |
#  |X3       |   -0.247   |
#  |         |  (0.003)   |
#  |X4       |   0.744    |
#  |         |  (0.003)   |
#  |X5       |   3.504    |
#  |         |  (0.003)   |
#  |X6       |   -1.999   |
#  |         |  (0.003)   |
#  |X7       |   0.502    |
#  |         |  (0.003)   |
#  |X8       |   0.997    |
#  |         |  (0.003)   |
#  |X9       |   1.256    |
#  |         |  (0.003)   |
#  |X10      |   1.999    |
#  |         |  (0.003)   |
#  |Num.Obs. |   1e+05    |
#  |R2       |   0.971    |
#  |R2 Adj.  |   0.971    |
#  |AIC      |  144993.2  |
#  |BIC      |  145097.9  |
#  |Log.Lik. | -72485.615 |
#  |F        | 338240.012 |