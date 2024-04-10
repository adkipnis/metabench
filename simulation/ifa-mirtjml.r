# load dependencies
rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(mirtjml)
setMIRTthreads(-1)

#===============================================================================
n <- 4000
d <- 1000
l <- 1 # this method only works well with l=1
data <- simulate_wide(seed=12, n=n, d=d, l=l, rho=0.3)

# initial values
A0 <- matrix(1, d, l)
d0 <- rep(0, d)

# run the jml analysis
result <- mirtjml_expr(as.matrix(data$responses), l,
                       A0=A0, d0=d0)
list2env(result, .GlobalEnv)

# inspect estimates
par(mfrow=c(3,1))
plot(data$difficulty, d_hat)
plot(data$A, A_hat)
plot(data$theta, theta_hat)

# correlation
cor(data$A, A_hat)
cor(data$difficulty, d_hat)
cor(data$theta, theta_hat)
