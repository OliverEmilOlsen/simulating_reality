


############# 4.4 #############

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
dt <- 1 
time_vector <- seq(0, end_time, by = dt)



# Delta t = 1
simulated_paths1 <- simulate_GBM2(x0 = x0,
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

x0 <- 100 
mu <- 0.03
sigma <- 0.1
n_sim <- 10
end_time <- 5
dt <- 1 / 10
time_vector <- seq(0, end_time, by = dt)



# Delta t = 1 / 10
simulated_paths2 <- simulate_GBM2(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)

data_plot2 <- data.frame('time' = time_vector, "GBM" = simulated_paths2)
data_plot2 <- melt(data_plot2,  id = c('time'))

plot_gbm2 <- ggplot(data_plot2, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm2

x0 <- 100 
mu <- 0.03
sigma <- 0.1
n_sim <- 10
end_time <- 5
dt <- 1 / 100
time_vector <- seq(0, end_time, by = dt)



# Delta t = 1 / 100

simulated_paths3 <- simulate_GBM2(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)

data_plot3 <- data.frame('time' = time_vector, "GBM" = simulated_paths3)
data_plot3 <- melt(data_plot3,  id = c('time'))

plot_gbm3 <- ggplot(data_plot3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm3

grid.arrange(plot_gbm1, plot_gbm2, plot_gbm3)



