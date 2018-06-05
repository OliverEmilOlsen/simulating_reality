#' Simulate asian put option
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_asian_put <- function(S0, K, sigma, r, n_sim, T) {
  dt = 1/200
  simulated_paths <- simulated_paths2 <- matrix(NA, 200*T, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- S0
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:(200*T)) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
    }
    riemann_value[k] <- 1/2*(pmax(K-mean(simulated_paths[,k]),0) + pmax(K-mean(simulated_paths2[,k]),0))
    asiancallprice[k] <- exp(-r*(T))*riemann_value[k]
  }
  price <- mean(asiancallprice)
  return(price)
}
