#### Asian options 

library(ggplot2) # sweet plots
library(reshape2) # melt function 
require(gridExtra)

######## source functions

source("functions/simulate_GBM.R")
source("functions/simulate_GBM2.R")
source("functions/simulate_GBM3.R")
source("functions/simulate_GBM_antithetic.R")
source("functions/simulate_GBM_antithetic_euler.R")
source("functions/Asian_call.R")


library(plyr)
delta <- 1/12
T <- 1
S0 <- 100
sigma <- 0.30
K <- 80
r <- 0.06
n <- 10^5
m <- T/delta
S <- numeric(m + 1)
S[1] <- S0
asian_price <- function() {
  for(j in 1:m) {
    W <- rnorm(1)
    S[j + 1] <- S[j] * exp((r - 0.5 * sigma^2) * delta + sigma * sqrt(delta) * W)
  }
  Si.bar <- mean(S[-1])
  exp(-r * T) * max(Si.bar - K, 0)
}
C <- raply(n, asian_price(), .progress = "text")
mean(C)


library(fExoticOptions)
GeometricAverageRateOption(TypeFlag = "c", S = S0, X = K,
                           Time = 1, r = 0.06, b = 0.0, sigma = 0.30)
?GeometricAverageRateOption


library(OptionPricing)
?AsianCall
AsianCall(T=0.25,d=12,K=40,r=0.08,sigma=0.3,S0=40,method=c("best"),
          sampling=c("MC"),metpar=list(maxiter=100,tol=1.e-14,cvmethod="splitting"),
          sampar=list(nout=50,n=2039,a=1487,baker=TRUE,genmethod="pca"))
AsianCall_AppLord(T = 1, d = 12, K = 80, r = 0.06, sigma = 0.3, S0 = 100, all=TRUE)
?AsianCall_AppLord

library(derivmkts)
s=S0; k=K; v=0.30; r=0.3; tt=1; d=0; m=12; numsim=1e05
#?arithasianmc
arithasianmc(s, k, v, r, tt, d, m, numsim, printsds=TRUE)
geomasianmc(s, k, v, r, tt, d, m, numsim, printsds=TRUE)
geomavgpricecall(s, k, v, r, tt, d, m, cont=TRUE)



#### 5.10
end_time <- 1
n_sim <- 10000
dt <- 1/200
time_vector <- seq(0, end_time, by = dt)



simulate_asian_call(S0 = 200, K = 75, r = 0.02, sigma = 0.2, n_sim = n_sim, time_vector = time_vector)

bscall(S0 = 200, K = 75, r = 0.02, T = 1, sigma = 0.2, q = 0)











          