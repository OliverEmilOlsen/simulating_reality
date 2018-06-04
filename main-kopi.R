

rm(list = ls()) # clear work space


######## load packages


library(ggplot2) # sweet plots
library(reshape2) # melt function 


######## source functions

source("functions/simulate_GBM.R")
source("functions/simulate_GBM2.R")
source("functions/simulate_GBM3.R")
source("functions/simulate_GBM_antithetic.R")
source("functions/simulate_GBM_antithetic_euler.R")

######## initialize

x0 <- 100 
mu <- 0.03
sigma <- 0.3
n_sim <- 10
end_time <- 1
dt <- 1 / 200
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

ggsave(filename = "plots/gbm_paths.png", 
       plot = plot_gbm)


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


## 3.3

simulated_paths <- simulate_GBM(x0 = S0,
                                   mu = r,
                                   sigma = sigma,
                                   n_sim = n_sim,
                                   time_vector = time_vector)


endtimevalues <- simulated_paths[51,]
calloptprice <- pmax(0, K-endtimevalues)

priceeucall <- c(NA, n_sim)

for (k in 1:n_sim) {
  priceeucall[k] <- exp(-r*T)*mean(calloptprice[1:k])
}

plot(1:n_sim, priceeucall, type = "l")


endtimevaluesmatrix <- matrix(NA, n_sim, 10)
calloptpricematrix <- matrix(NA, n_sim, 10)
priceeucall <- matrix(NA, n_sim, 10)

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  calloptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeucall[k,i] <- exp(-r*T)*mean(calloptpricematrix[1:k,i])
  }
}

priceeucall[10000,]

data_plot3_3 <- data.frame('paths' = 1:n_sim, "EUCallOptPrice" = priceeucall)
data_plot3_3 <- melt(data_plot3_3,  id = c('paths'))

plot3_3 <- ggplot(data_plot3_3, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1,4.8)

plot3_3


## 3.4

endtimevaluesmatrixAV <- matrix(NA, n_sim, 10)
calloptpricematrixAV <- matrix(NA, n_sim, 10)
priceeucallAV <- matrix(NA, n_sim, 10)
AVprice <- matrix(NA, n_sim, 10)

for (i in 1:10) {
  set.seed(i)
  simulated_paths <- simulate_GBM(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  calloptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  simulated_pathsAV <- simulate_GBM_AV_euler(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrixAV[,i] <- simulated_pathsAV[51,]
  calloptpricematrixAV[,i] <- pmax(0, K-endtimevaluesmatrixAV[,i])
  AVprice[,i] <- 0.5*(calloptpricematrix[,i] + calloptpricematrixAV[,i])
  for (k in 1:n_sim) {
    priceeucallAV[k,i] <- exp(-r*T)*mean(AVprice[1:k,i])
  }
}


data_plot3_4 <- data.frame('paths' = 1:n_sim, "EUCallOptPriceAV" = priceeucallAV)
data_plot3_4 <- melt(data_plot3_4,  id = c('paths'))

plot3_4 <- ggplot(data_plot3_4, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_4


## 3.5


endtimevaluesmatrix2 <- matrix(NA, n_sim, 5)
calloptpricematrix2 <- matrix(NA, n_sim, 5)
priceeucall2 <- matrix(NA, n_sim, 5)

for (i in 1:5) {
  set.seed(i)
  simulated_paths2 <- simulate_GBM2(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix2[,i] <- simulated_paths2[51,]
  calloptpricematrix2[,i] <- pmax(0, K-endtimevaluesmatrix2[,i])
  for (k in 1:n_sim) {
    priceeucall2[k,i] <- exp(-r*T)*mean(calloptpricematrix2[1:k,i])
  }
}


data_plot3_5 <- data.frame('paths' = 1:n_sim, "EUCallOptPrice" = priceeucall2)
data_plot3_5 <- melt(data_plot3_5,  id = c('paths'))

plot3_5 <- ggplot(data_plot3_5, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1,4.8)

plot3_5



## 3.6


endtimevaluesmatrixAV2 <- matrix(NA, n_sim, 5)
calloptpricematrixAV2 <- matrix(NA, n_sim, 5)
priceeucallAV2 <- matrix(NA, n_sim, 5)
AVprice2 <- matrix(NA, n_sim, 5)

for (i in 1:5) {
  set.seed(i)
  simulated_paths <- simulate_GBM(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  calloptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  simulated_pathsAV2 <- simulate_GBM_AV(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrixAV2[,i] <- simulated_pathsAV2[51,]
  calloptpricematrixAV2[,i] <- pmax(0, K-endtimevaluesmatrixAV2[,i])
  AVprice2[,i] <- 0.5*(calloptpricematrix[,i] + calloptpricematrixAV2[,i])
  for (k in 1:n_sim) {
    priceeucallAV2[k,i] <- exp(-r*T)*mean(AVprice2[1:k,i])
  }
}

data_plot3_6 <- data.frame('paths' = 1:n_sim, "EUCallOptPriceAV2" = priceeucallAV2)
data_plot3_6 <- melt(data_plot3_6,  id = c('paths'))

plot3_6 <- ggplot(data_plot3_6, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_6






