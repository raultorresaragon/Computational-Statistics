#,Author:,Raul
#,Class:,Statistical,Computing
#,Assignment:,homework,5
#,Date:,20231002

set.seed(20231002)
rm(list=ls())


vel<-c(9172 , 9350, 9558, 9775,10406,16084,18419,18552,18927,19052,19330,
       19343,19440,19473,19541,19547,19846,19856,19914,19918,19989,20166,
       20179,20196,20221,20415,20795,20821,20875,20986,21492,21701,21921,
       21960,22209,22242,22314,22374,22746,22747,22914,23206,23263,23484,
       23542,23666,23711,23413,24289,24366,24990,25633,26995,32065,34279,
       9483 ,10227,16170,18600,19070,19349,19529,19663,19863,19973,20175,
       20215,20629,20846,21137,21814,21185,22249,22495,22888,23241,23538,
       23706,24285,24717,26960,32789)

plot(density(vel), main = "Density of galaxy velocities")

x <- vel

sum.finite <- function(x){ sum(x[is.finite(x)]) }

Q <- function(mu1, mu2, mu3, p1, p2, p3, sigma1, sigma2, sigma3){
  
  pj_norm_sum <- (p1 * dnorm(x, mu1, sigma1)) + 
                 (p2 * dnorm(x, mu2, sigma2)) + 
                 (p3 * dnorm(x, mu3, sigma3))
  
  gamma1 <- p1*dnorm(x, mu1, sigma1)/(pj_norm_sum)
  gamma2 <- p2*dnorm(x, mu2, sigma2)/(pj_norm_sum)
  gamma3 <- p3*dnorm(x, mu3, sigma3)/(pj_norm_sum)
  
  sum.finite(gamma1 * (log(p1) -log(sigma1*sqrt(2*pi)) - ((x-mu1)^2/(2*sigma1^2))) + 
             gamma2 * (log(p2) -log(sigma2*sqrt(2*pi)) - ((x-mu2)^2/(2*sigma2^2))) +   
             gamma3 * (log(p3) -log(sigma3*sqrt(2*pi)) - ((x-mu3)^2/(2*sigma3^2)))
  )
  
}

K <- 3
theta_start <- list(m1 = 2502,
                    m2 = 5005,
                    m3 = 3966,
                    sigma1 = 4220,
                    sigma2 = 3691,
                    sigma3 = 5123,
                    p1 = 0.3,
                    p2 = 0.4,
                    p3 = 0.3)



EM_me <- function(I=100, criterion = 0.05, x) {
  
  mu1 = theta_start[[1]]
  mu2 = theta_start[[2]]
  mu3 = theta_start[[3]]
  sigma1 = theta_start[[4]]
  sigma2 = theta_start[[5]]
  sigma3 = theta_start[[6]]
  p1 = theta_start[[7]]
  p2 = theta_start[[8]]
  p3 = theta_start[[9]]
  
  Q_start <- Q(mu1, mu2, mu3, p1, p2, p3, sigma1, sigma2, sigma3)
  print(paste0("Q start = ", Q_start))
  
  Qval = Q_start + 10
  #while(abs(Q_start - Qval)>=criterion) {
  for(i in 1:I){
    #I <- I + 1
    pj_norm_sum <- (p1 * dnorm(x, mu1, sigma1)) + 
                   (p2 * dnorm(x, mu2, sigma2)) + 
                   (p3 * dnorm(x, mu3, sigma3))
  
    gamma1 <- p1*dnorm(x, mu1, sigma1) / pj_norm_sum
    gamma2 <- p2*dnorm(x, mu2, sigma2) / pj_norm_sum
    gamma3 <- p3*dnorm(x, mu3, sigma3) / pj_norm_sum

    mu1 <- sum.finite(gamma1*x) / sum.finite(gamma1)
    mu2 <- sum.finite(gamma2*x) / sum.finite(gamma2)
    mu3 <- sum.finite(gamma3*x) / sum.finite(gamma3)
  
    sigma1 <- sqrt(sum.finite((gamma1*(x-mu1)^2)) / sum.finite(gamma1))
    sigma2 <- sqrt(sum.finite((gamma2*(x-mu1)^2)) / sum.finite(gamma2))
    sigma3 <- sqrt(sum.finite((gamma3*(x-mu1)^2)) / sum.finite(gamma3))
  
    p1     <- sum.finite(gamma1)/length(x)
    p2     <- sum.finite(gamma2)/length(x)
    p3     <- sum.finite(gamma3)/length(x)
    
    Qval <- Q(mu1, mu2, mu3, p1, p2, p3, sigma1, sigma2, sigma3)

  }
  print(paste0("after ", I, " iterations we get Q=", Qval))
  
  res <- list(Qstart = Q_start, 
              Qfinal = Qval, 
              p1=p1, p2=p2, p3=p3, 
              mu1=mu1, mu2=mu2, mu3=mu3, 
              sigma1=sigma1, sigma2=sigma2, sigma3=sigma3)
  return(res)
}

myresults <- EM_me(I=54, x=vel)

gm <- mixtools::normalmixEM(x, k=3, lambda=c(0.3,0.4,0.3), mu=c(2502,5005,3966), sigma=c(4220,3691,5123))

save.image("hw/hw6_en.RData")
