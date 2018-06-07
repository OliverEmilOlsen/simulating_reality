library(quantmod)
library(ggplot2)
library(ggfortify)
require(gridExtra)

getSymbols('^OEX', src='yahoo', return.class = 'matrix')

tail(OEX)

value_close <- OEX[,4]

length((value_close))

n <- length(OEX[,4])
T_ = n/251 #251 obs pr år

u <- rep(NA, length(OEX[,4])-1)

for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
}

sigma_hat <- sqrt(1/T_ * sum(u^2))
mu_hat <- (log(OEX[[n,4]]/OEX[[1,4]]) + sigma_hat^2/2)/T_


x0 <- OEX[[1,4]]
mu <- mu_hat
sigma <- sigma_hat
n_sim <- 10000
end_time <- T_
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)



simulated_paths <- simulate_GBM2(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)


u2 <- matrix(NA, length(simulated_paths[,1])-1, 10000)
sigma_hat_vec <- rep(NA, 10000)
mu_hat_vec <- rep(NA, 10000)

for (j in 1:n_sim) {
  for (i in 2:length(simulated_paths[,1])) {
    u2[i-1,j] <- log(simulated_paths[i,j]/simulated_paths[i-1,j])
  }
  sigma_hat_vec[j] <- sqrt(1/T_ * sum(u2[,j]^2))
  mu_hat_vec[j] <- (log(simulated_paths[length(simulated_paths[,1]),j]/simulated_paths[1,j]) + sigma_hat_vec[i]^2/2)/T_
}

df1 <- data.frame(sigma_hat_vec)
df2 <- data.frame(mu_hat_vec)


hist_sigma <- (ggplot(data=df1, aes(df1$sigma_hat_vec), geom="histogram") + 
               geom_histogram(aes(y = ..density..), breaks=seq(0.188, 0.212, by = 0.001), col="black", fill="grey") + 
               geom_density(col=2) +
               labs(title="Histogram af ML-estimater af sigma") +
               labs(x="Sigma", y="Count"))

hist_mu <- (ggplot(data=df2, aes(df2$mu_hat_vec), geom="histogram") + 
            geom_histogram(aes(y = ..density..), breaks=seq(-0.2, 0.3, by = 0.025), col="black", fill="grey") + 
            geom_density(col=2) +
            labs(title="Histogram af ML-estimater af mu") +
            labs(x="Mu", y="Count"))


grid.arrange(hist_mu, hist_sigma)

