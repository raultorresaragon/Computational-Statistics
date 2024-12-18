## Raul Torres Aragon
## Stat Comp 
## 2023-09-19
## hw2

set.seed(7625)
rm(list = ls())




## Q3

N <- 5e3

get_means <- function(n, N){
  Xbar <- vector(mode = "numeric", length = N)
  for(i in 1:N) {
    Xbar[i] <- mean(rbinom(n, 1, 0.5))
  }
  return(Xbar)
}

run_sim <- function(n) {
  Xbar <- get_means(n, N)
  Odds <- Xbar/(1-Xbar)
  return(Odds)
}

plot_hist <- function(Odds, n) {
  Odds <- Odds[is.finite(Odds)]
  x <- seq(min(Odds), max(Odds), length = n)
  fun <- dnorm(x, mean=0, sd=0.004)
  hist(Odds, main = paste0("n=",n))
  lines(x, fun, lwd = 2)
}


Odds010 <- run_sim(10) -1
empvar_010 <- var(Odds010[!is.infinite(Odds010)])
plot_hist(Odds010, 10)

Odds030 <- run_sim(30) -1
empvar_030 <- var(Odds030)
plot_hist(Odds030, 30)

Odds100 <- run_sim(100) -1
empvar_100 <- var(Odds100)
plot_hist(Odds100, 100)

Odds500 <- run_sim(500) -1
empvar_500 <- var(Odds500)
plot_hist(Odds500, 500)

Odds1e3 <- run_sim(1000) -1
empvar_1e3 <- var(Odds1e3)
plot_hist(Odds1e3, 1000)


jpeg("Q3_hists.jpg", width=700, height=450)
par(mfrow = c(1,5)) #, mar=c(1,1,1,0))
plot_hist(Odds010, 10)
plot_hist(Odds030, 30)
plot_hist(Odds100, 100)
plot_hist(Odds500, 500)
plot_hist(Odds1e3, 1000)
dev.off()


## Q4
get_BS_stats <- function(n, X){
  B <- data.frame(matrix(ncol = 0, nrow=n))
  for(i in 1:100){
    B[[paste0("B", i)]] <- X[sample(1:n, size=n, replace=TRUE)]
  }
  g_bar_mean <- apply(B, 2, mean) |> mean()
  g_bar_var  <- apply(B, 2, var)  |> mean()
  output <- list(g_bar_mean = g_bar_mean, g_bar_var = g_bar_var)
  return (output)
}

Ps <- c(0.001,0.1,0.25,0.5,0.75,0.9,0.99)
Ns <- c(10,30,100,500)

results <- data.frame(matrix(ncol = 7, nrow(4)))
k=0
means <- vector(mode="numeric", length = length(Ps)*length(Ns))
vars  <- vector(mode="numeric", length = length(Ps)*length(Ns))
for(p in Ps) {
  for(n in Ns) {
    k <- k+1
    X <- rbinom(n, 1, p)
    o <- get_BS_stats(n, X)
    means[k] <- o[[1]]
    vars[k] <- o[[2]]
  }
}

true_vars <- Ps*(1-Ps)
results <- data.frame("Ps" = rep(Ps, each = 4), 
                     "VARs" = rep(true_vars, each = 4),
                     "Ns" = c(rep(Ns, 7)),
                     "BSmean" = means,
                     "BSvar" = vars)

save.image(file="hw2_en.Rdata")

