#' Simulate GBM
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_asian_call <- function(x0, r, sigma, K, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- x0
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      dx <- mu * simulated_paths[i - 1, k] * dt + simulated_paths[i - 1, k] * sigma * sqrt(dt) * rnorm(1)
      simulated_paths[i, k] <- simulated_paths[i - 1, k] + dx
    }
    riemann_value[k] <- sum(simulated_paths[,k]*dt)
    asiancallprice[k] <- exp(-r*T)*pmax(riemann_value[k]-K,0)
  }
  
  price <- mean(asiancallprice)
  
  return(price)
  
}
