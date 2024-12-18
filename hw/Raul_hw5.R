# Author: Raul
# Class: Statistical Computing
# Assignment: homework 5
# Date: 20231002

set.seed(20231002)
rm(list = ls())

n <- 10
x <- c(-2.04,-1.95,-1.91,-1.75,-1.66, -1.51, -1.35,-1.15,-0.85,-0.61,-0.38,-0.15,
       0.11, 0.4, 0.77, 1.34, 1.86, 2.34, 2.95, 3.4, 4.38, 4.79, 5.72)

Exact <- function(x,n=10) { 
   #T_n = (V-n)/sqrt(2n) -> V=T_n*(sqrt(2n))+n
   pchisq(x*(sqrt(2*n))+n, n) 
}

H2 <- function(x) { x^2-1 }
H3 <- function(x) { x^3-3*x }
H5 <- function(x) { x^5-10*x^3+15*x }

Edgesworth <- function(x,n=10) { 
   H2 <-  x^2-1 
   H3 <-  x^3-3*x 
   H5 <-  x^5-10*x^3+15*x  
   g1 <- sqrt(2)/(3*sqrt(n))
   g2 <- sqrt(1/(2*n))
   g12<- g1^2
   pnorm(x) - dnorm(x) * (g1*H2 + g2*H3 + g12*H5)
}

tab1 <- data.frame(X=x, Exact=Exact(x), EA=Edgesworth(x), "NA"=pnorm(x))

tab1 |> round(4)
save.image("hw5_en.RData")
