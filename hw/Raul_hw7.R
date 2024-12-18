# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Author: Raul 
# Date: 2023-11-03
# Assignment: hw7
# Class: Statistical Computing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(RColorBrewer)
set.seed(20231103)
rm(list = ls())
df <- faithful
display.brewer.pal(n = 8, name = 'Dark2')
colors <- c("#1B9E77","#D95F02","#7570B3","#E7298A")


# ~~~ #
#  Q1 #
# ~~~ #

# a)
plot(density(df$eruptions), lwd = 2,
     main="Desnsity estimation of Faithful eruptions",
     xlab="eruptions")

# b) 
density(df$eruptions, bw = 0.1) |> lines(col = colors[1])
density(df$eruptions, bw = 0.5) |> lines(col = colors[2])
density(df$eruptions, bw = 1.0) |> lines(col = colors[3])
legend("topright", 
       title="Bandwidth",
       legend=c("0.34", "0.1", "0.5", "1.0"),  
       fill = c("black",colors[1:3]),
       cex = 0.6)

# c) 
dens_est <- density(df$eruptions, kernel = "epanechnikov", bw = 0.15)
dest$y[dest$x>3.091 & dest$x<=3.109]
plot(density(df$eruptions, kernel = "epanechnikov", bw = 0.15), lwd = 2,
     main="Desnsity estimation ((Epanechnikov kernel) of Faithful eruptions",
     xlab="eruptions")
abline(v = 3.1)
abline(h = mean(dest$y[dest$x>3.091 & dest$x<=3.109]))

# ~~~~ #
#  Q3  #
# ~~~~ #

X = c(-1, 0, 0.5, 1, 2, 5/2)
Y = c( 2, 1, 0.0, 1, 2, 3.0)

getZ <- function(X,Y, z0 = 0) {
  Z <- vector(mode = "numeric", length = length(Y))
  Z[1] <- z0
  for(i in 1:(length(Y)-1)){
    Z[i+1] = -Z[i] + 2 * (Y[i+1] - Y[i])/(X[i+1] - X[i])
  }
  Z
}

Z <- getZ(X, Y, z0=0)
Z  


for(i in 1:6) {
  print((Z[i+1]-Z[i])/(X[i+1]-X[i]))
}

getQ <- function(X,Y,Z,x) {
  Q <- vector(mode = "numeric", length = length(x))
  j = 0
  for(i in 1:(length(Y)-1)) {
    for(xj in x) {
      j = j + 1
      Q[j] <-  1/2 * (Z[i+1] - Z[i])/(X[i+1] - X[i]) * (xj-X[i])^2 + Z[i]*(xj-X[i]) + Y[i]
    }
  }
  Q
}
x <- seq(min(X), max(X), by = 0.1)
Q <- getQ(X,Y,Z,x)




plot(Q~seq(min(X), max(X), length.out = length(Q)), type = "line")
lines(Y~X)

plot(Y~X, xaxt='n')
axis(1, at=X)
axis(1,at=X,labels=letters[1:length(X)])




