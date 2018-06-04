#' Simulate GBM antithetic method
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_GBM_AV <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- x0
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
      AV_paths[i, k] <- (simulated_paths[i, k] + simulated_paths2[i, k])/2
    }
  }
  
  return(AV_paths)
  
}


