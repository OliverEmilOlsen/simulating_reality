library(ggplot2) # sweet plots
library(reshape2) # melt function 
require(gridExtra)
require(quantmod)
require(pracma)

############################ Opgave 6 ############################

# Vasicek interest model:
S0 <- 100
r0 <- 0.01
b <- 0.042
a <- 0.15
sigma_r <- 0.01
rho <- -0.15
lambda <- -0.23
sigma_s <- 0.2
mu <- 0.06
B_vas <- (1 / a) * (1 - exp(-a * 1:10))
x_ij <- rep(1/10, 10)

#Porteføljevægte
x_beta <- 9/10
xS <- 1/10
xB <- 0

# simulate short rate paths
n <- 10000  # MC simulation trials
T <- 10    # total time
m <- 120   # subintervals
dt <- T/m  # difference in time each subinterval
time_vector <- seq(0, T, by = dt)

r <- matrix(NA,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0
s <- matrix(NA, m+1, n) 
s[1,] <- S0
A <- matrix(NA, m+1, n)
A_hedge <- matrix(NA, m+1, n)

# Liabilities
q <- 0.025
premium <- 1000
L_T <- premium*(1+q)^T

VasicekZCBprice <- function(r0, a, b, sigma_r, lambda, t, T){
  b_vas <- (1/a)*(1-exp(-(T-t)*a)) 
  a_vas <- (sigma_r^2/(2*a^2) - b + lambda*sigma_r/a)*((T-t)-b_vas) - (sigma_r^2)/(4*a)*b_vas^2
  return(exp(a_vas-b_vas*r0))
}



   
# Uden hedging  
A[1,] <- 1000

for(j in 1:n){
  z1 <- rnorm(m+1,0,1)
  z2 <- rnorm(m+1,0,1)
  z3 <- rho*z1 + sqrt(1-rho^2)*z2
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + sigma_r*sqrt(dt)*z1[i-1]
    r[i,j] <- r[i-1,j] + dr
    dA <- A[i-1,j]*(x_beta * r[i-1,j] * dt + xS * (mu*dt + sigma_s*sqrt(dt)*z3[i-1]) + 
      sum(x_ij * xB * ((r[i-1,j] - lambda * sigma_r * B_vas) * dt - sigma_r * B_vas * sqrt(dt) * z1[i-1])))
    A[i,j] <- A[i-1,j] + dA
  }
}

meanvector_A <- rep(NA, 121)
for (i in 1:121) {
  meanvector_A[i] <- mean(A[i,])
}
meanvector_A


L <- function(i) {
  VasicekZCBprice(r0 = r[i,j], a = a, b = b, sigma_r = sigma_r, lambda = lambda, t = (i-1)/12, T=10)*L_T
}

L(1:121)

## P(A(T) < L(T)) ##
1 - sum(A[121,] > L(121))/10000



### Plots over aktivernes værdiudvikling samt fordelinger  ###
qs <- apply(A, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_A))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

time_vector<-seq(0,T,by=dt)
data_plot_6.3<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.3<- melt(data_plot_6.3,  id = c('time'))

plot_6.3 <- ggplot(data_plot_6.3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("33% aktier, 33% obligationer, 33% i banken")

plot_6.3+scale_color_hue(labels = c("0.5%","50%", "99.5%","Mean"))

asset_dist1 <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
asset_plot1 <- ggplot(asset_dist1, aes(x=value, fill=year)) + geom_density(alpha=.3) +
  ggtitle("33% aktier, 33% obligationer, 33% i banken")



data_plot_6.32<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.32<- melt(data_plot_6.32,  id = c('time'))

plot_6.32 <- ggplot(data_plot_6.32, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("10% aktier, 90% obligationer, 0 i banken")

plot_6.32+scale_color_hue(labels = c("0.5%","50%", "99.5%","Mean"))

asset_dist2 <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
asset_plot2 <- ggplot(asset_dist2, aes(x=value, fill=year)) + geom_density(alpha=.3) +
  ggtitle("10% aktier, 90% obligationer, 0 i banken")



data_plot_6.33<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.33<- melt(data_plot_6.33,  id = c('time'))

plot_6.33 <- ggplot(data_plot_6.33, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("100% aktier, 0 obligationer, 0 i banken")

plot_6.33+scale_color_hue(labels = c("0.5%","50%", "99.5%","Mean"))

asset_dist3 <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
asset_plot3 <- ggplot(asset_dist3, aes(x=value, fill=year)) + geom_density(alpha=.3) +
  ggtitle("100% aktier, 0 obligationer, 0 i banken")



data_plot_6.34<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.34<- melt(data_plot_6.34,  id = c('time'))

plot_6.34 <- ggplot(data_plot_6.34, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("0 aktier, 50% obligationer, 50% i banken")

plot_6.34+scale_color_hue(labels = c("0.5%","50%", "99.5%","Mean"))

asset_dist4 <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
asset_plot4 <- ggplot(asset_dist4, aes(x=value, fill=year)) + geom_density(alpha=.3) +
  ggtitle("0 aktier, 50% obligationer, 50% i banken")



grid.arrange(plot_6.3, plot_6.32, plot_6.33, plot_6.34)

grid.arrange(asset_plot1, asset_plot2, asset_plot3, asset_plot4)


# Med ZCB-hedging  
A[1,] <- 1000 - L(1)

for(j in 1:n){
  z1 <- rnorm(m+1,0,1)
  z2 <- rnorm(m+1,0,1)
  z3 <- rho*z1 + sqrt(1-rho^2)*z2
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + sigma_r*sqrt(dt)*z1[i-1]
    r[i,j] <- r[i-1,j] + dr
    dA <- A[i-1,j]*(x_beta * r[i-1,j] * dt + xS * (mu*dt + sigma_s*sqrt(dt)*z3[i-1]) + 
                    sum(x_ij * xB * ((r[i-1,j] - lambda * sigma_r * B_vas) * dt - sigma_r * B_vas * sqrt(dt) * z1[i-1])))
    A[i,j] <- A[i-1,j] + dA
  }
}

for(j in 1:n){
  for(i in 1:(m+1)){
    A[i,j] <- A[i,j] + L(i) 
  }
}

meanvector_A <- rep(NA, 121)
for (i in 1:121) {
  meanvector_A[i] <- mean(A[i,])
}
meanvector_A

qs <- apply(A, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_A))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

time_vector<-seq(0,T,by=dt)
data_plot<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot<- melt(data_plot,  id = c('time'))

plot_zcbhedge <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot af aktiverne med ZCB-hedge")

plot_zcbhedge+scale_color_hue(labels = c("0.5%","50%", "99.5%","Mean"))

asset_dist_zcbh <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                          value = c(A[13,], A[37,], A[73,], A[121,]))
asset_plot_zcbh <- ggplot(asset_dist_zcbh, aes(x=value, fill=year)) + geom_density(alpha=.3) +
  ggtitle("Fordelingen af aktiver med ZCB-hedge")

grid.arrange(plot_zcbhedge, asset_plot_zcbh)

# Med ZC swap-hedging  
A[1,] <- 1000

payments <- matrix(NA, m+1, n)
payments[1,] <- L(1)*r0*dt 

for(j in 1:n){
  z1 <- rnorm(m+1,0,1)
  z2 <- rnorm(m+1,0,1)
  z3 <- rho*z1 + sqrt(1-rho^2)*z2
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + sigma_r*sqrt(dt)*z1[i-1]
    r[i,j] <- r[i-1,j] + dr
    dA <- A[i-1,j]*(x_beta * r[i-1,j] * dt + xS * (mu*dt + sigma_s*sqrt(dt)*z3[i-1]) + 
                      sum(x_ij * xB * ((r[i-1,j] - lambda * sigma_r * B_vas) * dt - sigma_r * B_vas * sqrt(dt) * z1[i-1])))
    payments[i,j] <- L(1)*dt*r[i-1,j]
    A[i,j] <- A[i-1,j] + dA - payments[i,j]
  }
}

L_matrix <- matrix(NA, m+1, n)

for(j in 1:n){
  A[1,j] <- 1000 + L(1)
  L_matrix[1,j] <- L_matrix[1,] <- 2*VasicekZCBprice(r0 = r0, a = a, b = b, sigma_r = sigma_r, 
                                                                lambda = lambda, t = 0, T=10)*L_T 
  for(i in 2:(m+1)){
    A[i,j] <- A[i,j] + L(i)
    L_matrix[i,j] <- L(i) + VasicekZCBprice(r0 = r0, a = a, b = b, sigma_r = sigma_r, lambda = lambda, t = 0, T=10)*L_T
  }
}

meanvector_A <- rep(NA, 121)
for (i in 1:121) {
  meanvector_A[i] <- mean(A[i,])
}
meanvector_A


meanvector_L <- rep(NA, 121)
for (i in 1:121) {
  meanvector_L[i] <- mean(L_matrix[i,])
}
meanvector_L


B_swap <- matrix(NA, m+1, n)
for (j in 1:n) {
  for (i in 1:(m+1)) {
    B_swap[i,j] <- pmax(A[i,j] - L_matrix[i,j], 0)
  }
}

meanvector_B_swap <- rep(NA, 121)
for (i in 1:121) {
  meanvector_B_swap[i] <- mean(B_swap[i,])
}
meanvector_B_swap


Loss_swap <- matrix(NA, 109, n)
for (j in 1:n) {
  for (i in 1:109) {
    Loss_swap[i,j] <- B_swap[i,j] - exp(-mean((r[i:(i+11),j])))*B_swap[i+12,j]
  }
}


meanvector_Loss_swap <- rep(NA, 109)
for (i in 1:109) {
  meanvector_Loss_swap[i] <- mean(Loss_swap[i,])
}
meanvector_Loss_swap


qs <- apply(Loss_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles <- t(rbind(qs,meanvector_Loss_swap))

SCR_swap <- quantiles[,3]
CR_swap <- matrix(NA, 109, n)

for (j in 1:n) {
  for (i in 1:109) {
    CR_swap[i,j] <- B_swap[i,j]/SCR_swap[i]
  }
}

meanvector_CR_swap <- rep(NA, 109)
for (i in 1:109) {
  meanvector_CR_swap[i] <- mean(CR_swap[i,])
}
meanvector_CR_swap



true_false_matrix_swap <- CR_swap < 1

for (j in 1:n) {
  for (i in 1:109) {
    if (true_false_matrix_swap[i,j] == TRUE){
      true_false_matrix_swap[i:109,j] = TRUE
    }
  }
}

sum(true_false_matrix_swap[109,])


insolv_prob_swap <- rep(NA, 109)
for (i in 1:109) {
  insolv_prob_swap[i] <- sum(true_false_matrix_swap[i,])/n  
}

ins_prob_plot_swap <- qplot(seq(0, 9, 1/12), insolv_prob_swap, xlab = "Tid",
                       ylab = "Sandsynlighed for insolvens", main = "Sandsynlighed for insolvens for ZCB-swap hedge")

plot_SCR_swap <- qplot(seq(0, 9, 1/12), SCR_swap, xlab = "Tid", ylab = "SCR", 
                       main = "Solvenskapitalkrav for ZCB-swap hedge")


qs_swap <- apply(B_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles_swap <- t(rbind(qs_swap,meanvector_B_swap))
colnames(quantiles) <- c("0.5%","50%", "99.5%","Mean")

time_vector <- seq(0,T,by=dt)
data_plot_6.6_swap <- data.frame("time"=time_vector, "quantiles" = quantiles_swap)
data_plot_6.6_swap <- melt(data_plot_6.6_swap,  id = c('time'))

plot_6.6_swap <- ggplot(data_plot_6.6_swap, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot for B(t) for ZCB-swap hedge") +
  scale_color_hue(name = "Fraktiler",labels = c("0.5%","50%", "99.5%","Mean"))

plot_6.6_swap

qs <- apply(CR_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_CR_swap))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

time_vector <- seq(0,T-1,by=dt)
data_plot_CR_swap <- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_CR_swap <- melt(data_plot_CR_swap,  id = c('time'))

plot_CR_swap <- ggplot(data_plot_CR_swap, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot af CR(t) for ZCB-swap hedge") +
  scale_color_hue(name = "Fraktiler",labels = c("0.5%","50%", "99.5%","Mean"))

plot_CR_swap


grid.arrange(plot_6.6_swap, plot_SCR_swap, plot_CR_swap, ins_prob_plot_swap)


# Plot of asset distributions (6.3)
asset_dist <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                  value = c(A[13,], A[37,], A[73,], A[121,]))
ggplot(asset_dist, aes(x=value, fill=year)) + geom_density(alpha=.3)
      
# Plot assets
data_plot <- data.frame('time' = time_vector, "Value" = A[,1:50])
data_plot <- melt(data_plot,  id = c('time'))
plot_A <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")
plot_A
 
data_plot_L <- data.frame('time' = time_vector, "Value" = L_matrix[,1:50])
data_plot_L <- melt(data_plot_L,  id = c('time'))
plot_L <- ggplot(data_plot_L, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")
plot_L


# Bonus
B <- matrix(NA, m+1, n)
for (j in 1:n) {
  for (i in 1:(m+1)) {
    B[i,j] <- pmax(A[i,j] - L(i), 0)
  }
}



meanvector_B <- rep(NA, 121)
for (i in 1:121) {
  meanvector_B[i] <- mean(B[i,])
}
meanvector_B

data_plot <- data.frame('time' = time_vector, "Value" = B)
data_plot <- melt(data_plot,  id = c('time'))
plot_B <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")
plot_B


qs <- apply(B, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_B))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

time_vector<-seq(0,T,by=dt)
data_plot_6.6<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.6<- melt(data_plot_6.6,  id = c('time'))

plot_6.6 <- ggplot(data_plot_6.6, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot for B(t)")

plot_6.6+scale_color_hue(name = "Fraktiler",labels = c("0.5%","50%", "99.5%","Mean"))



# Loss

Loss <- matrix(NA, 109, n)
for (j in 1:n) {
  for (i in 1:109) {
    Loss[i,j] <- B[i,j] - exp(-mean((r[i:(i+11),j])))*B[i+12,j]
  }
}



meanvector_Loss <- rep(NA, 109)
for (i in 1:109) {
  meanvector_Loss[i] <- mean(Loss[i,])
}
meanvector_Loss


qs <- apply(Loss, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_Loss))

SCR <- quantiles[,3]

plot_SCR <- qplot(time_vector, SCR, xlab = "Tid", ylab = "SCR", main = "Solvenskapitalkrav")


CR <- matrix(NA, 109, n)

for (j in 1:n) {
  for (i in 1:109) {
    CR[i,j] <- B[i,j]/SCR[i]
  }
}


meanvector_CR <- rep(NA, 109)
for (i in 1:109) {
  meanvector_CR[i] <- mean(CR[i,])
}
meanvector_CR




qs <- apply(CR, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_CR))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

time_vector<-seq(0,T-1,by=dt)
data_plot_CR<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_CR<- melt(data_plot_CR,  id = c('time'))

plot_CR <- ggplot(data_plot_CR, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot af CR(t)") +
  scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

plot_CR




true_false_matrix <- CR < 1
true_false_matrix[100,]



for (j in 1:n) {
  for (i in 1:109) {
    if (true_false_matrix[i,j] == TRUE){
      true_false_matrix[i:109,j] = TRUE
    }
  }
}



insolv_prob <- rep(NA, 109)
for (i in 1:109) {
  insolv_prob[i] <- sum(true_false_matrix[i,])/n  
}

ins_prob_plot <- qplot(time_vector, insolv_prob, xlab = "Tid",
                    ylab = "Sandsynlighed for insolvens", main = "Sandsynlighed for insolvens")



grid.arrange(plot_6.6, plot_SCR, plot_CR, ins_prob_plot)





##### 6.9 #####

b_Q <- b-lambda*sigma_r/a 

A[1,] <- 1000

for(j in 1:n){
  z1 <- rnorm(m+1,0,1)
  z2 <- rnorm(m+1,0,1)
  z3 <- rho*z1 + sqrt(1-rho^2)*z2
  for(i in 2:(m+1)){
    dr <- a*(b_Q-r[i-1,j])*dt + sigma_r*sqrt(dt)*z1[i-1]
    r[i,j] <- r[i-1,j] + dr
    dA <- A[i-1,j]*(x_beta * r[i-1,j] * dt + xS * (r[i,j]*dt + sigma_s*sqrt(dt)*z3[i-1]) + 
                      sum(x_ij * xB * ((r[i-1,j]*dt - sigma_r * B_vas * sqrt(dt) * z1[i-1]))))
    A[i,j] <- A[i-1,j] + dA
  }
}

meanvector_A <- rep(NA, 121)
for (i in 1:121) {
  meanvector_A[i] <- mean(A[i,])
}
meanvector_A

L2 <- function(t) {
  1000*(1+0.025)^(t/12)
}


B <- matrix(NA, m+1, n)
for (j in 1:n) {
  for (i in 1:(m+1)) {
    B[i,j] <- max(A[i,j] - L2(i), 0)
  }
}

meanvector_B <- rep(NA, 121)
for (i in 1:121) {
  meanvector_B[i] <- mean(B[i,])
}
meanvector_B

option_price <- VasicekZCBprice(r0 = r0, a = a, b = b, sigma_r = sigma_r, lambda = lambda, t = (i-1)/12, T=10)*meanvector_B

option_price[121]

y_price <- option_price[seq(13,121,12)]

sum(option_price[seq(13,121,12)])



##### 6.10 #####

exp_Q <- function(eta){
  return(VasicekZCBprice(r0 = r0, a = a, b = b, sigma_r = sigma_r, lambda = lambda, t = 0, T = 10)
         *(L2(121) + eta * option_price[121]) - 1000)
}


bisect(exp_Q, 0, 1)















