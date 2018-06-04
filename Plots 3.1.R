
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

######## Høj mu

x0 <- 100 
mu <- 0.2
sigma <- 0.1
n_sim <- 10
end_time <- 5
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)


simulated_paths_høj <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)


######## Lav mu

x0 <- 100 
mu <- -0.2
sigma <- 0.1
n_sim <- 10
end_time <- 5
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)


simulated_paths_lav <- simulate_GBM(x0 = x0,
                                    mu = mu,
                                    sigma = sigma,
                                    n_sim = n_sim,
                                    time_vector = time_vector)





data_plot1 <- data.frame('time' = time_vector, "GBM" = simulated_paths_lav)
data_plot1 <- melt(data_plot1,  id = c('time'))

plot_gbm1 <- ggplot(data_plot1, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm1


data_plot2 <- data.frame('time' = time_vector, "GBM" = simulated_paths_høj)
data_plot2 <- melt(data_plot2,  id = c('time'))

plot_gbm2 <- ggplot(data_plot2, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")

plot_gbm2

grid.arrange(plot_gbm1, plot_gbm2)



######## Høj sigma

x0 <- 100 
mu <- 0.05
sigma <- 0.05
n_sim <- 10
end_time <- 5
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)


simulated_paths_høj <- simulate_GBM(x0 = x0,
                                    mu = mu,
                                    sigma = sigma,
                                    n_sim = n_sim,
                                    time_vector = time_vector)


######## Lav sigma

x0 <- 100 
mu <- 0.05
sigma <- 0.3
n_sim <- 10
end_time <- 5
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)


simulated_paths_lav <- simulate_GBM(x0 = x0,
                                    mu = mu,
                                    sigma = sigma,
                                    n_sim = n_sim,
                                    time_vector = time_vector)





data_plot1 <- data.frame('time' = time_vector, "GBM" = simulated_paths_lav)
data_plot1 <- melt(data_plot1,  id = c('time'))

plot_gbm1 <- ggplot(data_plot1, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(0, 400)

plot_gbm1


data_plot2 <- data.frame('time' = time_vector, "GBM" = simulated_paths_høj)
data_plot2 <- melt(data_plot2,  id = c('time'))

plot_gbm2 <- ggplot(data_plot2, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none") +
  ylim(0, 400)

plot_gbm2

grid.arrange(plot_gbm1, plot_gbm2)







