#' Simulate asian call option
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_asian_call <- function(S0, K, sigma, r, n_sim, T) {
  dt = 1/1000
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, 1000, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- S0
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)

  for (k in 1:n_sim) {
    for (i in 2:1000) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
    }
    riemann_value[k] <- 1/2*(pmax(mean(simulated_paths[,k])-K,0) + pmax(mean(simulated_paths2[,k])-K,0))
    asiancallprice[k] <- exp(-r*(T))*riemann_value[k]
  }
  price <- mean(asiancallprice)
  return(price)
}








simulate_asian_call(S0 = 200, K = 20, r = 0.02, sigma = 0.2, n_sim = 1000, T = 1)

sim_european_call(S0 = 200, K = 20, r = 0.02, T = 1, n_sim = n_sim, sigma = 0.2)


