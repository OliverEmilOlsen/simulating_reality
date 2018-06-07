

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














