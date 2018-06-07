library(quantmod)
library(ggplot2)
library(ggfortify)
require(gridExtra)
require(histogram)
theme_set(theme_bw())

# 3.6
getSymbols('^OEX',src='yahoo', from = '2007-01-03', to = '2018-01-03')
tail(OEX)
plot(OEX[,4])
OEX$OEX.Close

# Plot 
autoplot(OEX$OEX.Close) + 
  labs(title="S&P 100 Closing Prices", y = "Price", x = "Year") + 
  theme(plot.title = element_text(hjust=0.5)) 
  
  
# Tjek normalfordeling
getSymbols('^OEX',src='yahoo', return.class = 'matrix', from = '2008-01-01', to = '2018-01-01')

log.value <- log(OEX[,4])
log.ret <- rep(NA, length(OEX[,4])-1)

for (i in 2:length(OEX[,4])) {
  log.ret[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
}

log.ret.data <- data.frame(log.ret)


qqplot.data <- function (vec) { 
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
    labs(title = "QQ-plot af logaritmisk afkast")
}



qq1 <- qqplot.data(log.ret)


hist1 <- (ggplot(data=log.ret.data, aes(log.ret.data$log.ret), geom="histogram") + 
  geom_histogram(aes(y = ..density..), breaks=seq(-0.05, 0.025, by = 0.005), col="black", fill="grey") + 
  geom_density(col=2) +
  labs(title="Histogram for logaritmisk afkast") +
  labs(x="Log return", y="Count"))


# Arrange the four plots 
grid.arrange(hist1, qq1)


# S&P 100 ticker: ^OEX

getSymbols('^OEX',src='yahoo', from = '2008-01-01', to = '2018-01-01')

OEX

n <- length(OEX[,4])
T_ = n/252
dt <- 1/252

plot(OEX[,4])


## 3.8

u <- rep(NA, length(OEX[,4])-1)
si <- rep(NA, length(OEX[,4])-1)
sit <- rep(NA, length(OEX[,4])-1)

for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
  si[i-1] <- OEX[[i-1,4]]
  sit[i-1] <- OEX[[i,4]]
}


loglike <- function(pars, si. = si, sit. = sit) {
  mu <- pars[1]
  sigma2 <- pars[2]^2
  val.log.like <- n/2*log(2*pi) + n/2*log(sigma2*dt) + 1/(2*sigma2*dt) * sum((log(sit./si.)-(mu-sigma2/2)*dt)^2)
  return(val.log.like)
}
opti <- optim( c(0, 1), loglike)
opti


sigma_hat <- sqrt(1/T_ * sum(u^2))
mu_hat <- (log(OEX[[n,4]]/OEX[[1,4]]) + sigma_hat^2/2)/T_

sigma_hat
mu_hat


## 3.9

getSymbols('^OEX',src='yahoo', from = "2011-01-01", to = '2018-01-01')

n <- length(OEX[,4])
T_ <- n/252 # 251 obs pr år

u <- rep(NA, length(OEX[,4])-1)
si <- rep(NA, length(OEX[,4])-1)
sit <- rep(NA, length(OEX[,4])-1)

for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
  si[i-1] <- OEX[[i-1,4]]
  sit[i-1] <- OEX[[i,4]]
}


loglike <- function(pars, si. = si, sit. = sit) {
  mu <- pars[1]
  sigma2 <- pars[2]^2
  val.log.like <- n/2*log(2*pi) + n/2*log(sigma2*dt) + 1/(2*sigma2*dt) * sum((log(sit./si.)-(mu-sigma2/2)*dt)^2)
  return(val.log.like)
}
opti <- optim( c(0, 1), loglike)
opti

sigma_hat_2011 <- sqrt(1/T_ * sum(u^2))
sigma_hat_2011

mu_hat_2011 <- 1/T_ * log(OEX[[n,4]]/OEX[[1,4]]) + sigma_hat_2011^2/2
mu_hat_2011



