# Author: Raul
# Class: Statistical Computing
# Assignment: homework 4
# Date: 20231002

set.seed(20231002)
rm(list = ls())



# Problem 1 
# (b)
f <- function(theta1, theta2) {
  theta1 - theta2 + 2*theta1^2 + 2*theta1*theta2 + theta2^2
}
x <- y <- seq(-5, 5, length = 30)
z <- outer(x, y, f)
persp(x, y, z)

df <- function(theta1, theta2) {
  a1 <-  1 + 4*theta1 +2*theta2
  a2 <- -1 + 2*theta1 +2*theta2
  matrix(c(a1,a2), nrow=2)
}

Hf <- matrix(c(4, 2, 2, 2), nrow=2, ncol=2)

iterate <- function(theta1, theta2, n) {
  x0 <- c(theta1, theta2)
  x_new <- matrix(x0, nrow=2) - solve(Hf) %*% df(x0[1],x0[1])
  for(i in 1:n) {
    x_new <- matrix(x_new, nrow=2) - solve(Hf) %*% df(x_new[1],x_new[1])
  }
  f(x_new[1],x_new[1])
}

for(i in 1:5){
  a <- runif(1, -1, 1)
  b <- runif(1, -1, 1)
  print(iterate(a,b,i))
}




rm(list = ls())
# Problem 2 (Fischer's Scoring algo)
n1 <- 55; n2 <- 157; n3 <- 159; n4 <- 16
Y <- rep(0, n1+n2+n3+n4)
Y[(n1+1):(n1+2)] <- 1
Y[(n1+n2+1):(n1+n2+7)] <- 1
Y[(n1+n2+n3+1):(n1+n2+n3+3)] <- 1
X1 <- rep(1, n1+n2+n3+n4)
X2 <- c(rep(7, n1), rep(14, n2), rep(27, n3), rep(57, n4))
X  <- matrix(c(X1,X2), nrow=n1+n2+n3+n4)

#data.frame(Y=Y, X1=X1, X2=X2)
glm(Y~X[,2], family = "binomial")

theta_0 <- matrix(c(.01,.01), nrow=2)
score_algo <- function(theta, X, Y, n1, n2, n3, n4, its=5) {
  for(i in 1:its) {
    X2 <- X[,2]
    mu <- exp(X%*%theta)/(1+exp(X%*%theta))
    Z <- X%*%theta + (Y-mu)*1/(mu*(1-mu))
    #W <- (c(rep(n1,n1),rep(n2,n2),rep(n3,n3),rep(n4,n4)) * mu*(1-mu))^-1
    W <- (mu*(1-mu)) / c(rep(n1,n1),rep(n2,n2),rep(n3,n3),rep(n4,n4))
    theta_new <- lm(Z~X2, weights = W) |> coefficients() |> as.matrix() |> unname()
    theta <- theta_new
  }
  theta_new
}

theta_hat <- score_algo(theta_0, X, Y, n1, n2, n3, n4, its = 25)


save.image("hw4_en.RData")






### Mu <- function(theta1, theta2, X) {
### theta_vec <- matrix(c(theta1, theta2), nrow=2)
### X%*%theta_vec
### }
### 
### xB <- Mu(1,2,X)
### pi_of_theta <- function(xB){ exp(xB)/(1+exp(xB)) }
### dpi_theta <- function(xB) { exp(xB)/(1+exp(xB))^2 }
### 
### Z <- pi_of_theta(xB) + (Y-xB)*dpi_theta(xB)
### VxB <- c(rep(n1,n1),rep(n2,n2),rep(n3,n3),rep(n4,n4)) * xB*(1-xB)
### W <- (dpi_theta(xB)^2 * VxB)^-1
### 
### lm(Z~X, weights = W) |> coefficients()


#### by hand
### x0 <- c(1,1)
### 
### x1 <- matrix(x0, nrow=2) - solve(Hf) %*% df(x0[1],x0[1])
### f(x1[1],x1[1])
### 
### x2 <- matrix(x1, nrow=2) - solve(Hf) %*% df(x1[1],x1[1])
### f(x2[1],x2[1])
### 
### x3 <- matrix(x2, nrow=2) - solve(Hf) %*% df(x2[1],x2[1])
### f(x3[1],x3[1])
### 
### x4 <- matrix(x3, nrow=2) - solve(Hf) %*% df(x3[1],x3[1])
### f(x4[1],x4[1])

