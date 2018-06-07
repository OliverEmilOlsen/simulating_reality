#### Option pricing

source("functions/Black-Scholes_final.R")

library(ggplot2) # sweet plots
library(reshape2) # melt function 
require(gridExtra)
require(quantmod)
require(pracma)



#################

getSymbols('^GSPC', src='yahoo', return.class = 'matrix')

tail(GSPC)

value_close <- GSPC[,4]

length((value_close))

n <- length(GSPC[,4])
T_ = n/251

u <- rep(NA, length(GSPC[,4])-1)

for (i in 2:length(GSPC[,4])) {
  u[i-1] <- log(GSPC[[i,4]]/GSPC[[i-1,4]])
}

sigma_hat <- sqrt(1/T_ * sum(u^2))
mu_hat <- (log(GSPC[[n,4]]/GSPC[[1,4]]) + sigma_hat^2/2)/T_


# GSPC[[n,4]] er lukkeprisen for S&P500 pr 1/6-2018

## Europæiske optioner fra S&P500 med udløb 31. juli 2018 ##

# Calls
getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$calls

strike <- rep(NA,73)
strike <- getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$calls$Strike


BS_prices_eu <- rep(NA,73)
for (i in 1:73) {
  BS_prices_eu[i] <- bscall(S0 = GSPC[[n,4]], K = strike[i], r = mu_hat, T = 2/12, sigma = sigma_hat, q = 0)
}


BS_prices_eu_q <- rep(NA,73)
for (i in 1:73) {
  BS_prices_eu_q[i] <- bscall(S0 = GSPC[[n,4]], K = strike[i], r = mu_hat, T = 2/12, sigma = sigma_hat, q = 0.0185)
}

obs_prices_eu <- getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$calls
obs_prices_eu[,2]

BSvsOBScall_eu <- qplot(BS_prices_eu, obs_prices_eu[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
      main = "EU-Calls u. dividende") + geom_abline()

BSvsOBScall_eu_q <- qplot(BS_prices_eu_q, obs_prices_eu[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                     main = "EU-Calls m. dividende") + geom_abline()


# Puts
dim(getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$puts)

strike2 <- rep(NA,76)
strike2 <- getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$puts$Strike

BS_prices2_eu <- rep(NA,76)

for (i in 1:76) {
  BS_prices2_eu[i] <- bsput(S0 = GSPC[[n,4]], K = strike2[i], r = mu_hat, T = 2/12, sigma = sigma_hat, q = 0)
}

BS_prices2_eu_q <- rep(NA,76)

for (i in 1:76) {
  BS_prices2_eu_q[i] <- bsput(S0 = GSPC[[n,4]], K = strike2[i], r = mu_hat, T = 2/12, sigma = sigma_hat, q = 0.0185)
}

obs_prices2_eu <- getOptionChain('^SPX', src = 'yahoo', NULL, return.class = 'matrix')$Jul.31.2018$puts
obs_prices2_eu[,2]

BSvsOBSput_eu <- qplot(BS_prices2_eu, obs_prices2_eu[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                     main = "EU-Puts u. dividende") + geom_abline()

BSvsOBSput_eu_q <- qplot(BS_prices2_eu_q, obs_prices2_eu[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                    main = "EU-Puts m. dividende") + geom_abline()





## Amerikanske optioner fra SPDR-ETF med udløb 21. juni 2019 ##


getSymbols('SPY', src='yahoo', return.class = 'matrix')

value_close <- SPY[,4]

length((value_close))

n <- length(SPY[,4])
T_ = n/251

u <- rep(NA, length(SPY[,4])-1)

for (i in 2:length(SPY[,4])) {
  u[i-1] <- log(SPY[[i,4]]/SPY[[i-1,4]])
}

sigma_hat <- sqrt(1/T_ * sum(u^2))
mu_hat <- (log(SPY[[n,4]]/SPY[[1,4]]) + sigma_hat^2/2)/T_


# Calls 
dim(getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$calls)

strike <- rep(NA,51)
strike <- getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$calls$Strike

# Perioden fra 1/6-18 til 21/6-19 er ca 12.7 år

BS_prices_us<- rep(NA,51)
for (i in 1:51) {
  BS_prices_us[i] <- bscall(S0 = SPY[[n,4]], K = strike[i], r = mu_hat, T = 12.7/12, sigma = sigma_hat, q = 0)
}

BS_prices_us_q <- rep(NA,51)
for (i in 1:51) {
  BS_prices_us_q[i] <- bscall(S0 = SPY[[n,4]], K = strike[i], r = mu_hat, T = 12.7/12, sigma = sigma_hat, q = 0.016)
}

obs_prices_us <- getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$calls
obs_prices_us[,2]

BSvsOBScall_us <- qplot(BS_prices_us, obs_prices_us[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                     main = "US-Calls u. dividende") + geom_abline()
BSvsOBScall_us_q <- qplot(BS_prices_us_q, obs_prices_us[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                     main = "US-Calls m. dividende") + geom_abline()



# Puts
dim(getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$puts)

strike2 <- rep(NA,59)
strike2 <- getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$puts$Strike

BS_prices_us2 <- rep(NA,59)
for (i in 1:59) {
  BS_prices_us2[i] <- bsput(S0 = SPY[[n,4]], K = strike2[i], r = mu_hat, T = 12.7/12, sigma = sigma_hat, q = 0)
}

BS_prices_us2_q <- rep(NA,59)
for (i in 1:59) {
  BS_prices_us2_q[i] <- bsput(S0 = SPY[[n,4]], K = strike2[i], r = mu_hat, T = 12.7/12, sigma = sigma_hat, q = 0.016)
}

obs_prices_us2 <- getOptionChain('SPY', src = 'yahoo', NULL, return.class = 'matrix')$Jun.21.2019$puts
obs_prices_us2[,2]

BSvsOBSput_us <- qplot(BS_prices_us2, obs_prices_us2[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                    main = "US-Puts u. dividende") + geom_abline()

BSvsOBSput_us_q <- qplot(BS_prices_us2_q, obs_prices_us2[,2], xlab = "Black-Scholes-pris", ylab = "Observeret pris",
                    main = "US-Puts m. dividende") + geom_abline()


grid.arrange(BSvsOBScall_eu, BSvsOBSput_eu, BSvsOBScall_eu_q, BSvsOBSput_eu_q)

grid.arrange(BSvsOBScall_us, BSvsOBSput_us, BSvsOBScall_us_q, BSvsOBSput_us_q)

grid.arrange(BSvsOBScall_eu, BSvsOBScall_eu_q, BSvsOBScall_us, BSvsOBScall_us_q)

grid.arrange(BSvsOBSput_eu, BSvsOBSput_eu_q, BSvsOBSput_us, BSvsOBSput_us_q)





##### Implied volatility #####

implied.vol <- function(S0, K, T, r, market){
    sigma <- sigma_hat
    sig.up <- 1
    sig.down <- 0.001
    count <- 0
    err <- bscall(S0, K, T, r, sigma, q = 0) - market 
    
    while(abs(err) > 0.0001 && count<100000){
      if(err < 0){
        sig.down <- sigma
        sigma <- (sig.up + sigma)/2
      }else{
        sig.up <- sigma
        sigma <- (sig.down + sigma)/2
      }
      err <- bscall(S0, K, T, r, sigma, q = 0) - market
      count <- count + 1
    }
    
    if(count==100000){
      return(NA)
    }
    else{
      return(sigma)
    }
}


S0 <- GSPC[[n,4]]
T = 2/12
r = mu_hat

impvol_eu_call <- rep(NA, 73)


for(i in 1:73){
  impvol_eu_call[i] <- implied.vol(S0 = S0, K = strike[i], T = T, r = r, market = obs_prices_eu[i,2])
}


impvol_plot1 <- qplot(strike, impvol_eu_call, xlab = "Strike", ylab = "Volatilitet", 
                      main = "Implied Volatility EU-call") + xlim(2600, 3300)



implied.vol <- function(S0, K, T, r, market){
  sigma <- sigma_hat
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  err <- bsput(S0, K, T, r, sigma, q = 0) - market 
  
  while(abs(err) > 0.0001 && count<100000){
    if(err < 0){
      sig.down <- sigma
      sigma <- (sig.up + sigma)/2
    }else{
      sig.up <- sigma
      sigma <- (sig.down + sigma)/2
    }
    err <- bsput(S0, K, T, r, sigma, q = 0) - market
    count <- count + 1
  }
  
  if(count==100000){
    return(NA)
  }
  else{
    return(sigma)
  }
}


S0 <- SPY[[n,4]]
T = 12.7/12
r = mu_hat


impvol_us_put <- rep(NA, 59)
for(i in 1:59){
  impvol_us_put[i] <- implied.vol(S0 = S0, K = strike2[i], T = T, r = r, market = obs_prices_us2[i,2])
}

impvol_plot2 <- qplot(strike2, impvol_us_put, xlab = "Strike", ylab = "Volatilitet", main = "Implied Volatility US-put")

grid.arrange(impvol_plot1, impvol_plot2)




#### Asiatiske vs europæiske optioner ####

S0 <- 100
K <- 80
r <- 0.06
sigma <- 0.2
T <- 1
n_sim <- 5000

simulate_asian_call(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
sim_european_call(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)


##### Calls #####

#### Ændringer i T ####
T_changes <- c(0.3, 0.5, 1, 2, 3, 5)
price_vec_EU1 <- rep(NA, 6)
price_vec_AS1 <- rep(NA, 6)

for (i in 1:6){
  price_vec_EU1[i] <- sim_european_call(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
  price_vec_AS1[i] <- simulate_asian_call(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
}

callplot1 <- qplot(price_vec_EU1, price_vec_AS1, xlab = "Pris på europæisk call", ylab = "Pris på asiatisk call",
                   main = "Ændring i T") 


#### Ændringer i S(0) ####
S0_changes <- c(50, 80, 100, 120, 150, 200)
price_vec_EU2 <- rep(NA, 6)
price_vec_AS2 <- rep(NA, 6)

for (i in 1:6){
  price_vec_EU2[i] <- sim_european_call(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
  price_vec_AS2[i] <- simulate_asian_call(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
}

callplot2 <- qplot(price_vec_EU2, price_vec_AS2, xlab = "Pris på europæisk call", ylab = "Pris på asiatisk call",
                   main = "Ændring i S(0)") 


#### Ændringer i r ####
r_changes <- c(-0.15, 0, 0.01, 0.1, 0.3, 0.7)
price_vec_EU3 <- rep(NA, 6)
price_vec_AS3 <- rep(NA, 6)

for (i in 1:6){
  price_vec_EU3[i] <- sim_european_call(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
  price_vec_AS3[i] <- simulate_asian_call(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
}

callplot3 <- qplot(price_vec_EU3, price_vec_AS3, xlab = "Pris på europæisk call", ylab = "Pris på asiatisk call",
                   main = "Ændring i r")


#### Ændringer i sigma ####
sigma_changes <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
price_vec_EU4 <- rep(NA, 6)
price_vec_AS4 <- rep(NA, 6)

for (i in 1:6){
  price_vec_EU4[i] <- sim_european_call(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
  price_vec_AS4[i] <- simulate_asian_call(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
}

callplot4 <- qplot(price_vec_EU4, price_vec_AS4, xlab = "Pris på europæisk call", ylab = "Pris på asiatisk call",
                   main = "Ændring i sigma") 


grid.arrange(callplot1, callplot2, callplot3, callplot4)





##### Puts #####


S0 <- 100
K <- 120
r <- 0.06
sigma <- 0.2
T <- 1
n_sim <- 5000




#### Ændringer i T ####
T_changes <- c(0.3, 0.5, 1, 2, 3, 5)
price_vec2_EU1 <- rep(NA, 6)
price_vec2_AS1 <- rep(NA, 6)

for (i in 1:6){
  price_vec2_EU1[i] <- sim_european_put(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
  price_vec2_AS1[i] <- simulate_asian_put(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T_changes[i])
}

putplot1 <- qplot(price_vec2_EU1, price_vec2_AS1, xlab = "Pris på europæisk put", ylab = "Pris på asiatisk put",
                   main = "Ændring i T") 


#### Ændringer i S(0) ####
S0_changes <- c(50, 80, 90, 100, 150, 200)
price_vec2_EU2 <- rep(NA, 6)
price_vec2_AS2 <- rep(NA, 6)

for (i in 1:6){
  price_vec2_EU2[i] <- sim_european_put(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
  price_vec2_AS2[i] <- simulate_asian_put(S0 = S0_changes[i], K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
}

putplot2 <- qplot(price_vec2_EU2, price_vec2_AS2, xlab = "Pris på europæisk put", ylab = "Pris på asiatisk put",
                   main = "Ændring i S(0)") 


#### Ændringer i r ####
r_changes <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3)
price_vec2_EU3 <- rep(NA, 6)
price_vec2_AS3 <- rep(NA, 6)

for (i in 1:6){
  price_vec2_EU3[i] <- sim_european_put(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
  price_vec2_AS3[i] <- simulate_asian_put(S0 = S0, K = K, sigma = sigma, r = r_changes[i], n_sim = n_sim, T = T)
}

putplot3 <- qplot(price_vec2_EU3, price_vec2_AS3, xlab = "Pris på europæisk put", ylab = "Pris på asiatisk put",
                   main = "Ændring i r")


#### Ændringer i sigma ####
sigma_changes <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
price_vec2_EU4 <- rep(NA, 6)
price_vec2_AS4 <- rep(NA, 6)

for (i in 1:6){
  price_vec2_EU4[i] <- sim_european_put(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
  price_vec2_AS4[i] <- simulate_asian_put(S0 = S0, K = K, sigma = sigma_changes[i], r = r, n_sim = n_sim, T = T)
}

putplot4 <- qplot(price_vec2_EU4, price_vec2_AS4, xlab = "Pris på europæisk put", ylab = "Pris på asiatisk put",
                   main = "Ændring i sigma") 


grid.arrange(putplot1, putplot2, putplot3, putplot4)














S0 <- 100
K <- 150
r <- 0.06
sigma <- 0.2
T <- 1
n_sim <- 5000

simulate_asian_put(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)
sim_european_put(S0 = S0, K = K, sigma = sigma, r = r, n_sim = n_sim, T = T)





