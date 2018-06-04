#### Black-Scholes 
epsilon <- 1/365

bs_call <- function(S, K, T, r, sigma) {
  d1 <- (log(S/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  if (T < epsilon) { # Small time to expiry
    return(max(S-K,0))
  } else {
    return(S*pnorm(d1) - K*exp(-r*(T))*pnorm(d2))
  }
}

bs_call(S = 40, K = 40, r = 0.08, T = 0.25, sigma = 0.3)


bs_put <- function(S, K, T, r, sigma, q) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  if (T < epsilon) { # Small time to expiry
    return(max(K-S,0))
  } 
  else {
    return(exp(-r*(T))*(exp((r-q)*T)*S*pnorm(d1)-K*pnorm(d2)))
  }
}

bs_put(S = 40, K = 40, r = 0.08, T = 0.25, sigma = 0.3, q=0)

library(qrmtools)
Black_Scholes(t = 0, S = 40, K = 40, r = 0.08, T = 0.25, sigma = 0.3, type = c("put"))
