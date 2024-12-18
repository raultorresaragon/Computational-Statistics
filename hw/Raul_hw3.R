rm(list = ls())

x1=3;  x2=-1;  x3=0;  x4=1
x_0 <- matrix(c(x1,x2,x3,x4), ncol=1)


f_of_x <- function(x1,x2,x3,x4) {
  (x1+10*x2)^2 + 5*(x3-x4)^2+(x2-2*x3)^4+10*(x1-2*x4)^4
}
f_of_x(x1,x2,x3,x4)


gradient = function(x1,x2,x3,x4) {
  g_wrtx1 <- 2*(x1+10*x2) + 40*(x1-2*x4)^3
  g_wrtx2 <- 20*(x1+10*x2) + 4*(x2-2*x3)^3
  g_wrtx3 <- 10*(x3-x4) -8*(x2-2*x3)^3
  g_wrtx4 <- -10*(x2-x4) -80*(x1-2*x4)^3
  return(c(g_wrtx1, g_wrtx2, g_wrtx3, g_wrtx4))
}
gradient(x1,x2,x3,x4)


inverse_H <- function(x1,x2,x3,x4) {
  row1 <- c(2+120*(x1-2*x4)^2 , 20 , 0 , -240*(x1-2*x4)^2) 
  row2 <- c(20 , 12*(x2-2*x3)^2+200 , -24*(x2-2*x3)^2 , 0)
  row3 <- c(0 , -24*(x2-2*x3)^2 , -48*(x2-2*x3)^2+10 , -10)
  row4 <- c(-240*(x1-2*x4)^2 , 0 , -10 , 480*(x1-2*x4)^2+10)
  H <- matrix(c(row1,row2,row3,row4), byrow = TRUE, ncol=4, nrow=4)
  return(list(invH = solve(H), H = H))
}
inverse_H(x1,x2,x3,x4) 

x_new <- matrix(c(x1,x2,x3,x4), ncol=1) - inverse_H(x1,x2,x3,x4)[[1]] %*% gradient(x1,x2,x3,x4)
# ~~~~~~~~~~~~~~~~~~~~


x1 <- x_new[1]; x2 <- x_new[2]; x3 <- x_new[3]; x4 <- x_new[4]; 
f_of_x(x1,x2,x3,x4)
gradient(x1,x2,x3,x4)
inverse_H(x1,x2,x3,x4) 

save.image("hw3_en.RData")