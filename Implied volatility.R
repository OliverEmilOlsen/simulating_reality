
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

#### 
?getOptionChain
getOptionChain('^SPX',src='yahoo', return.class = 'ts')

# Acquire the volatility of S&P500 from 2011-today:
getSymbols('^OEX',src='yahoo', curl.options = matrix(), from = "2011-01-01")

n <- length(OEX[,4])
T_ <- n/252 

u <- rep(NA, length(OEX[,4])-1)
for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
}

sigma_hat_2011 <- sqrt(1/T_ * sum(u^2))
sigma_hat_2011

mu_hat_2011 <- 1/T_ * log(OEX[[n,4]]/OEX[[1,4]]) + sigma_hat_2011^2/2
mu_hat_2011

S0 <- OEX[[n-252/3,4]]
K <- 2805
T <- T_

bs_call(S = S0, K = K, r = mu_hat_2011, T = T_, sigma = sigma_hat_2011)
bs_put(S = S0, K = K, r = mu_hat_2011, T = T_, sigma = sigma_hat_2011, q=0)



