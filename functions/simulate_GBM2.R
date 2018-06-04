#' Simulate GBM metode 2
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_GBM2 <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- x0
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * rnorm(1, 0, 1))
    }
  }
  
  return(simulated_paths)
  
}



