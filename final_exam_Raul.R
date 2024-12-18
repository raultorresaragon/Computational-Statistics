# Assignment: final exam
# Course: Statistical Computing
# Date: 2023-12-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
set.seed(20231214)


# Problem 3

# Newton-Ralphson
X = c(34,18,20,125)
X = c(5,0,1,14)
PI <- function(theta) {
  as.matrix(c(theta/4, .25*(1-theta), .25*(1-theta), .25*(2+theta)))
}

l <- function(theta, X) {
  sum(X*log(PI(theta)))
}

l_prime <- function(X, theta) {
  X[1]/theta - (X[2]+X[3])/(1-theta) + X[4]/(2+theta)
} 

nr <- function(theta_start, X) {
  theta_old <- theta_start
  for(i in 1:1){
    theta_new <- theta_old - l(theta_old, X)/l_prime(X, theta_old)
  }
  theta_hat <- theta_new
  theta_hat
}
nr(0.17, X)


l <- function(X, PI) {
  X*log(PI)
}

grad_l <- function(X, PI) {
  as.matrix(X/PI)
}

hessian_l <- function(X, PI) {
  diag( as.vector(X/l(X,PI)^2 * grad_l(X, PI)^2 ), nrow = 4, ncol=4)
}

nr <- function(theta_start, niter=10, X) {
  PI_0 <- PI(theta_start)
  for(i in 1:niter) {
    PI_new <- as.matrix(PI_0) - solve(hessian_l(X,PI_0)) %*% grad_l(X, PI_0)
    PI_0 <- abs(PI_new)/sum(abs(PI_new))
  }
  PI_new <- PI_0
}



# EM algorithm
X = c(34,18,20,125)
X = c(5,0,1,14)
theta_old <-0.09
get_theta_new <- function(theta_old, X){
  (theta_old/(2+theta_old)*X[1]+X[4])/(theta_old/(2+theta_old)*X[1] + X[2]+X[3]+X[4])
}
for(i in 1:5) {
  theta_new <- get_theta_new(theta_old, X)
  print(theta_new)
  theta_old <- theta_new
}
theta_hat <- theta_new
theta_hat



# Gibbs sampling algo
set.seed(123)
rm(list=ls())
Nsim=1000
n=15; a=0.99; start <- rnorm(1,0,1)
X1=array(0,dim=c(Nsim,1))  #init arrays
X2=array(0,dim=c(Nsim,1))  #init arrays
X1[1]=rnorm(1,1+a*(start-2),1-a^2)
X2[1]=rnorm(1,2+a*(X1[1]-1),1-a^2)
#initial values
for(i in 2:Nsim){
  X1[i]=rnorm(1,1+a*(X2[i-1]-2), 1-a^2)
  X2[i]=rnorm(1,2+a*(X1[i]-1), 1-a^2)
}
mean(X1); sd(X1)
mean(X2); sd(X2)
par(mfrow=c(1,1))
plot(X1[1:1000], type = "l", main = "traceplot X1")
plot(X2[1:1000], type = "l", main = "traceplot X2")
plot(X1~X2, type="l", main = "line plot of X1 and X2")



save.image("hw/final_exam_en.RData")