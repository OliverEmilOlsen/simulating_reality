#### Black-Scholes 

# Call
bscall <- function(S0, K, T, r, sigma, q) {
  d1 <- (log(S0/K) + (r - q + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  return(S0*pnorm(d1) - K*exp(-r*(T))*pnorm(d2))
}

bscall(S0 = 100, K = 110, r = 0.05, T = 1, sigma = 0.2, q=0)


# Put
bsput <- function(S0, K, T, r, sigma, q) {
  bscall(S0, K, T, r, sigma, q) - exp(-q*T)*S0 + K*exp(-r*T)
}

bsput(S0 = 100, K = 110, r = 0.05, T = 1, sigma = 0.2, q=0)
