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
    
    ## repeat until error is sufficiently small or counter hits 1000
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
    
    ## return NA if counter hit 1000
    if(count==100000){
      return(NA)
    }
    else{
      return(sigma)
    }
}

implied.vol(S0 = 100, K = 80, T = 5, r = 0.05, market = 25)




S0 <- SPY[[n,4]]
T = 12.7/12
r = mu_hat
impvol_us_call <- rep(NA, 51)
impvol_eu_call <- rep(NA, 51)

for(i in 1:51){
  impvol_us_call[i] <- implied.vol(S0 = S0, K = strike[i], T = T, r = r, market = obs_prices_us[i,2])
}

qplot(strike, impvol_us_call, xlab = "Strike", ylab = "Volatilitet") + xlim(300,400)


S0 <- GSPC[[n,4]]
T = 2/12
r = mu_hat
for(i in 1:73){
  impvol_eu_call[i] <- implied.vol(S0 = S0, K = strike[i], T = T, r = r, market = obs_prices_eu[i,2])
}

qplot(strike, impvol_eu_call, xlab = "Strike", ylab = "Volatilitet") + xlim(2450,2800)


bla <- function(sigma1) {
  for (i in 1:73) {
    bscall(S0 = GSPC[[n,4]], K = strike[i], r = mu_hat, T = 2/12, sigma = sigma1, q = 0) - obs_prices_eu[i,2]
  }
}


bisect(bla, 0, 1)

'Ingen lukket form løsn for asiatisk opt - Monte Carlo er genialt. 
Pris på eur call > asiatisk call 
Pris afh positivt af volatiliteten. Se Black_Scholes. 
Volatilitet af asiatisk er mindre end eur.

Observationer af priser er under P'

