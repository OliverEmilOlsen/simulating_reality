

library(ggplot2) # sweet plots
library(reshape2) # melt function 
require(gridExtra)
require(quantmod)

######## source functions

source("functions/simulate_GBM.R")
source("functions/simulate_GBM2.R")
source("functions/simulate_GBM3.R")
source("functions/simulate_GBM_antithetic.R")
source("functions/simulate_GBM_antithetic_euler.R")



S0 <- 100
K <- 80
r <- 0.06
T <- 5
sigma <- 0.3
n_sim <- 5000



########################### Europæisk call-oprion ###########################

sim_european_call<-function(S0, K, sigma, r, n_sim, T) {
  V <- simulated_paths <- simulated_paths2 <- rep(NA, n_sim)
    for (i in 1:n_sim) {
        z1 <- rnorm(1)
        z2 <- -z1
        simulated_paths[i] <- S0 * exp((r-0.5*sigma^2)*T + sigma * sqrt(T) * z1)
        simulated_paths2[i] <- S0 * exp((r-0.5*sigma^2)*T + sigma * sqrt(T) * z2)
        V[i] <- 1/2*(pmax(0, simulated_paths[i]-K) + pmax(0, simulated_paths2[i]-K))
    }
    calloptprice <- exp(-r*T) * mean(V)
    return(calloptprice)
}

sim_european_call(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
bscall(S0 = S0, K = K, r = r, T = T, sigma = sigma, q=0)


#### Ændringer i T ####
T_changes <- c(1, 2, 4, 7, 10, 13)
price_vec_MC1 <- rep(NA, 6)
price_vec_BS1 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC1[i] <- sim_european_call(S0 = S0, K = K, 
                                       sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
  price_vec_BS1[i] <- bscall(S0 = S0, K = K, r = r, T = T_changes[i], sigma = sigma, q=0)
}

callplot1 <- qplot(price_vec_BS1, price_vec_MC1, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i T") + geom_abline()


#### Ændringer i S(0) ####
S0_changes <- c(50, 80, 90, 100, 150, 200)
price_vec_MC2 <- rep(NA, 6)
price_vec_BS2 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC2[i] <- sim_european_call(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
  price_vec_BS2[i] <- bscall(S0 = S0_changes[i], K = K, r = r, T = T, sigma = sigma, q=0)
}

callplot2 <- qplot(price_vec_BS2, price_vec_MC2, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i S(0)") + geom_abline() 


#### Ændringer i r ####
r_changes <- c(-0.15, 0, 0.01, 0.1, 0.3, 0.7)
price_vec_MC3 <- rep(NA, 6)
price_vec_BS3 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC3[i] <- sim_european_call(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
  price_vec_BS3[i] <- bscall(S0 = S0, K = K, r = r_changes[i], T = T, sigma = sigma, q=0)
}

callplot3 <- qplot(price_vec_BS3, price_vec_MC3, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i r") + geom_abline() 


#### Ændringer i sigma ####
sigma_changes <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
price_vec_MC4 <- rep(NA, 6)
price_vec_BS4 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC4[i] <- sim_european_call(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
  price_vec_BS4[i] <- bscall(S0 = S0, K = K, r = r, T = T, sigma = sigma_changes[i], q=0)
}

callplot4 <- qplot(price_vec_BS4, price_vec_MC4, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i sigma") + geom_abline() 


grid.arrange(callplot1, callplot2, callplot3, callplot4)


########################### Europæisk put-oprion ############################


S0 <- 100
K <- 120
r <- 0.06
T <- 5
sigma <- 0.3
n_sim <- 5000


sim_european_put<-function(S0, K, sigma, r, n_sim, T) {
  V <- simulated_paths <- simulated_paths2 <- rep(NA, n_sim)
  for (i in 1:n_sim) {
    z1 <- rnorm(1)
    z2 <- -z1
    simulated_paths[i] <- S0 * exp((r-0.5*sigma^2)*T + sigma * sqrt(T) * z1)
    simulated_paths2[i] <- S0 * exp((r-0.5*sigma^2)*T + sigma * sqrt(T) * z2)
    V[i] <- 1/2*(pmax(0, K-simulated_paths[i]) + pmax(0, K-simulated_paths2[i]))
  }
  calloptprice <- exp(-r*T) * mean(V)
  return(calloptprice)
}

sim_european_put(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
bsput(S0 = S0, K = K, r = r, T = T, sigma = sigma, q=0)


#### Ændringer i T ####
T_changes <- c(1, 2, 4, 7, 10, 13)
price_vec_MC1 <- rep(NA, 6)
price_vec_BS1 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC1[i] <- sim_european_put(S0 = S0, K = K, 
                                        sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
  price_vec_BS1[i] <- bsput(S0 = S0, K = K, r = r, T = T_changes[i], sigma = sigma, q=0)
}

callplot1 <- qplot(price_vec_BS1, price_vec_MC1, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i T") + geom_abline()


#### Ændringer i S(0) ####
S0_changes <- c(50, 80, 90, 100, 150, 200)
price_vec_MC2 <- rep(NA, 6)
price_vec_BS2 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC2[i] <- sim_european_put(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
  price_vec_BS2[i] <- bsput(S0 = S0_changes[i], K = K, r = r, T = T, sigma = sigma, q=0)
}

callplot2 <- qplot(price_vec_BS2, price_vec_MC2, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i S(0)") + geom_abline() 


#### Ændringer i r ####
r_changes <- c(-0.15, 0, 0.01, 0.1, 0.3, 0.7)
price_vec_MC3 <- rep(NA, 6)
price_vec_BS3 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC3[i] <- sim_european_put(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
  price_vec_BS3[i] <- bsput(S0 = S0, K = K, r = r_changes[i], T = T, sigma = sigma, q=0)
}

callplot3 <- qplot(price_vec_BS3, price_vec_MC3, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i r") + geom_abline() 


#### Ændringer i sigma ####
sigma_changes <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
price_vec_MC4 <- rep(NA, 6)
price_vec_BS4 <- rep(NA, 6)

for (i in 1:6){
  price_vec_MC4[i] <- sim_european_put(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
  price_vec_BS4[i] <- bsput(S0 = S0, K = K, r = r, T = T, sigma = sigma_changes[i], q=0)
}

callplot4 <- qplot(price_vec_BS4, price_vec_MC4, xlab = "Black-scholes-pris", ylab = "Monte Carlo-pris",
                   main = "Ændring i sigma") + geom_abline() 


grid.arrange(callplot1, callplot2, callplot3, callplot4)







