# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Author: Raul 
# Date: 2023-11-03
# Assignment: hw7
# Class: Statistical Computing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(RColorBrewer)
set.seed(20231103)
rm(list = ls())

# ~~ #
# Q1 #
# ~~ #

u <- runif(1e4, min=0, max=1)
mu <- 1
beta <- 1
x_inv <- mu -beta*log((1-u)/u)
x_r <- rlogis(1e4, location=mu, scale=beta)

hist_x_inv <- hist(x_inv, freq = TRUE, main = "")
hist_x_r <- hist(x_r, freq = TRUE, main = "")

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
plot(hist_x_inv, col = c1, xlab = "x", main = "histogram of X\nunder two methods")
plot(hist_x_r, col = c2, add = TRUE, xlab = "X")
legend("topright", legend = c("x inv method","x rlogis"), fill = c(c1,c2))



sigma <- 5
y_inv <- mu + sigma*tan(pi*(u-.5))
y_r <- rcauchy(1e4, location=mu, scale=sigma)

hist_y_inv <- hist(y_inv, freq = TRUE, main = "")
hist_y_r <- hist(y_r, freq = TRUE, main = "")

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
plot(hist_y_inv, col = c1, xlab = "y", main = "histogram of Y\nunder two methods")
plot(hist_y_r, col = c2, add = TRUE, xlab = "Y")
legend("topleft", legend = c("y inv method","y rlogis"), fill = c(c1,c2))


save.image("hw8.RData")
