#### Opgave 4.9


library(ggplot2) # sweet plots
library(reshape2) # melt function 
require(gridExtra)

######## source functions

source("functions/simulate_GBM.R")
source("functions/simulate_GBM2.R")
source("functions/simulate_GBM3.R")
source("functions/simulate_GBM_antithetic.R")
source("functions/simulate_GBM_antithetic_euler.R")


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

endtimevaluesmatrix <- matrix(NA, n_sim, 10)
putoptpricematrix <- matrix(NA, n_sim, 10)
priceeuput <- matrix(NA, n_sim, 10)

for (i in 1:10) {
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
  simulated_paths <- simulate_GBM_AV_euler(x0 = S0, mu = r, sigma = sigma, n_sim = n_sim, time_vector = time_vector)
  endtimevaluesmatrix[,i] <- simulated_paths[51,]
  putoptpricematrix[,i] <- pmax(0, K-endtimevaluesmatrix[,i])
  for (k in 1:n_sim) {
    priceeuput[k,i] <- exp(-r*T)*mean(putoptpricematrix[1:k,i])
  }
}



data_plot3_4 <- data.frame('paths' = 1:n_sim, "EUPutOptPriceAV" = priceeuput)
data_plot3_4 <- melt(data_plot3_4,  id = c('paths'))

plot3_4 <- ggplot(data_plot3_4, aes(paths, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(4.1, 4.8)

plot3_4



## 3.5


endtimevaluesmatrix2 <- matrix(NA, n_sim, 5)
putoptpricematrix2 <- matrix(NA, n_sim, 5)
priceeuput2 <- matrix(NA, n_sim, 5)

for (i in 1:5) {
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


grid.arrange(plot3_3, plot3_4)

grid.arrange(plot3_5, plot3_6)

