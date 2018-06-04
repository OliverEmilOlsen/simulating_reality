

rm(list = ls()) # clear work space


######## load packages


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

######## initialize

x0 <- 100 
mu <- 0.03
sigma <- 0.1
n_sim <- 10
end_time <- 5
dt <- 1 / 100
time_vector <- seq(0, end_time, by = dt)



######## read stock price data

sp_100_data <- read.table("data/sp_100_jan_feb_2018.csv", 
                          header = TRUE,
                          sep = ";", 
                          dec = ",", 
                          stringsAsFactors = FALSE)




######## plot closing price

plot(1:dim(sp_100_data)[1], sp_100_data$Close, type = 'l')



######## simulate paths from GBM


simulated_paths <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)

######### plot simulated paths


#### R plot

plot(time_vector, simulated_paths[, 1], type = "l")

#### ggplot

data_plot <- data.frame('time' = time_vector, "GBM" = simulated_paths)
data_plot <- melt(data_plot,  id = c('time'))

plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm


#### save ggplot

'ggsave(filename = "plots/gbm_paths.png", 
       plot = plot_gbm)'


#### save simulated paths

write.table(
  simulated_paths,
  "results/simulated_paths.csv",
  sep = ";",
  dec = ",",
  row.names = FALSE
)

#### q 4.9



S0 <- 5
K <- 10
r <- 0.06
T <- 1
sigma <- 0.3
end_time <- 1
n_sim <- 10000
dt <- 1/50
time_vector <- seq(0, end_time, by = dt)




sim_european_put_av<-function(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=10^4){
  z_1 <- rnorm(n)
  z_2 <- -z_1
  #Simulate the payoffs with both processes
  sim_payoff_1 <- S_0*exp((r-0.5*vol^2)*T_years + vol*z_1*sqrt(T))
  sim_payoff_2 <- S_0*exp((r-0.5*vol^2)*T_years + vol*z_2*sqrt(T))
  sim_payoff <- (sim_payoff_1 + sim_payoff_2)/2
  #Calculate results and bounds
  return(sim_payoff)
}
sim_european_put_av(S_0 = 5, K = 10, vol = 0.3, T_years = 1, r = 0.06, n = 10000)

endtimevaluesmatrix <- matrix(NA, n_sim, 10)
putoptpricematrix <- matrix(NA, n_sim, 10)
priceeuput <- matrix(NA, n_sim, 10)

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  putoptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeuput[k,i] <- exp(-r*T)*mean(putoptpricematrix[1:k,i])
  }
}


simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
simulated_paths[51,]


data_plot3_4 <- data.frame('paths' = 1:n_sim, "EUPutOptPriceAV" = priceeuput)
data_plot3_4 <- melt(data_plot3_4,  id = c('paths'))

plot3_4 <- ggplot(data_plot3_4, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.2, 4.7)

plot3_4


sim_european_put_av<-function(S0, K, sigma, mu, n_sim, time_vector){
  simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevalues <- simulated_paths[51,]
  putoptprice <- exp(-r*end_time) * mean(pmax(0, K-endtimevalues))
  return(putoptprice)
}

sim_european_put_av(S0 = 5, K = 10, sigma = 0.3, mu = 0.06, n_sim = n_sim, time_vector = time_vector)

simulated_paths <- simulate_GBM_AV(x0 = S0,
                                   mu = r,
                                   sigma = sigma,
                                   n_sim = n_sim,
                                   time_vector = time_vector)


endtimevalues <- simulated_paths[51,]
putoptprice <- exp(-r*end_time) * mean(pmax(0, K-endtimevalues))

priceeuput <- c(NA, n_sim)

for (k in 1:n_sim) {
  priceeuput[k] <- exp(-r*T)*mean(calloptprice[1:k])
}
mean(priceeuput)
plot(1:n_sim, priceeuput, type = "l")


## 3.3

endtimevaluesmatrix <- matrix(NA, n_sim, 10)
putoptpricematrix <- matrix(NA, n_sim, 10)
priceeuput <- matrix(NA, n_sim, 10)

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  putoptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeuput[k,i] <- exp(-r*T)*mean(putoptpricematrix[1:k,i])
  }
}


data_plot3_3 <- data.frame('paths' = 1:n_sim, "EUPutOptPrice" = priceeuput)
data_plot3_3 <- melt(data_plot3_3,  id = c('paths'))

plot3_3 <- ggplot(data_plot3_3, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1,4.8)

plot3_3


## 3.4

endtimevaluesmatrix <- matrix(NA, n_sim, 10)
putoptpricematrix <- matrix(NA, n_sim, 10)
priceeuput <- matrix(NA, n_sim, 10)

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  putoptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeuput[k,i] <- exp(-r*T)*mean(putoptpricematrix[1:k,i])
  }
}


simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
simulated_paths[51,]


data_plot3_4 <- data.frame('paths' = 1:n_sim, "EUPutOptPriceAV" = priceeuput)
data_plot3_4 <- melt(data_plot3_4,  id = c('paths'))

plot3_4 <- ggplot(data_plot3_4, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_4

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  simulated_pathsAV <- simulate_GBM_AV_euler(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, 
                                             time_vector = time_vector)
  endtimevaluesmatrixAV[,i] <- simulated_pathsAV[51,]
  endtimevaluesmatrixsamlet <- 0.5*(endtimevaluesmatrix + endtimevaluesmatrixAV)
  AVprice[,i] <- pmax(0, K-endtimevaluesmatrixsamlet[,i])
  for (k in 1:n_sim) {
    priceeuputAV[k,i] <- exp(-r*T)*mean(AVprice[1:k,i])
  }
}

data_plot3_4_ <- data.frame('paths' = 1:n_sim, "EUPutOptPriceAV" = priceeuputAV)
data_plot3_4_ <- melt(data_plot3_4_,  id = c('paths'))

plot3_4_ <- ggplot(data_plot3_4_, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_4_


## 3.5

endtimevaluesmatrix2 <- matrix(NA, n_sim, 5)
putoptpricematrix2 <- matrix(NA, n_sim, 5)
priceeuput2 <- matrix(NA, n_sim, 5)

for (i in 1:5) {
  set.seed(i)
  simulated_paths2 <- simulate_GBM2(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix2[,i] <- simulated_paths2[51,]
  putoptpricematrix2[,i] <- pmax(0, K-endtimevaluesmatrix2[,i])
  for (k in 1:n_sim) {
    priceeuput2[k,i] <- exp(-r*T)*mean(putoptpricematrix2[1:k,i])
  }
}


data_plot3_5 <- data.frame('paths' = 1:n_sim, "EUPutOptPrice" = priceeuput2)
data_plot3_5 <- melt(data_plot3_5,  id = c('paths'))

plot3_5 <- ggplot(data_plot3_5, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1,4.8)

plot3_5




## 3.6

endtimevaluesmatrix <- matrix(NA, n_sim, 5)
putoptpricematrix <- matrix(NA, n_sim, 5)
priceeuput <- matrix(NA, n_sim, 5)

for (i in 1:5) {
  set.seed(i)
  simulated_paths <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  putoptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeuput[k,i] <- exp(-r*T)*mean(putoptpricematrix[1:k,i])
  }
}

data_plot3_6 <- data.frame('paths' = 1:n_sim, "EUPutOptPriceAV2" = priceeuput)
data_plot3_6 <- melt(data_plot3_6,  id = c('paths'))

plot3_6 <- ggplot(data_plot3_6, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_6


## 4.7

getSymbols('^OEX',src='yahoo', from = "2007-01-01", to = "2018-01-01")

n <- length(OEX[,4])
T_ <- 11

u <- rep(NA, length(OEX[,4])-1)

for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
}


sigma_hat_2007_2018 <- sqrt(1/T_ * sum(u^2))
mu_hat_2007_2018 <- 1/T_ * log(OEX[[n,4]]/OEX[[1,4]]) + sigma_hat_2007_2018^2/2
sigma_hat_2007_2018
mu_hat_2007_2018


x0 <- OEX[[1,4]] 
mu <- mu_hat_2007_2018
sigma <- sigma_hat_2007_2018
n_sim <- 5000
end_time <- T_
dt <- 1 / (n/T_)
time_vector <- seq(0, end_time, by = dt)

simulated_paths <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)


mean(simulated_paths[n,])

endtimevalues_3_8 <- simulated_paths[n,]

hist(endtimevalues_3_8, breaks = 100)


data_plot <- data.frame('time' = time_vector, "GBM" = simulated_paths)
data_plot <- melt(data_plot,  id = c('time'))

plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm


# 4.4

S0 <- 1
r <- 0.01
T <- 1
sigma <- 0.95
end_time <- 1
n_sim <- 50
dt <- 1/12
time_vector <- seq(0, end_time, by = dt)


simulated_paths <- simulate_GBM(x0 = S0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)


data_plot <- data.frame('time' = time_vector, "GBM" = simulated_paths)
data_plot <- melt(data_plot,  id = c('time'))

plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm



simulated_paths1 <- simulate_GBM2(x0 = S0,
                                  mu = mu,
                                  sigma = sigma,
                                  n_sim = n_sim,
                                  time_vector = time_vector)

data_plot1 <- data.frame('time' = time_vector, "GBM" = simulated_paths1)
data_plot1 <- melt(data_plot1,  id = c('time'))

plot_gbm1 <- ggplot(data_plot1, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm1



















