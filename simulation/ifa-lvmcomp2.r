# load dependencies
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(lvmcomp2)

#===============================================================================
n <- 2000
d <- 200
l <- 2
data <- simulate_wide(seed=1, n=n, d=d, l=l, rho=0.4)

# Initial values
Q <- matrix(1, d, l)
A0 <- Q
d0 <- rep(0, d)
B0 <- diag(l)
theta0 <- matrix(rnorm(n*l), n, l)

## perform estimation
result_sa_conf <- sa_mirt_conf(as.matrix(data$responses), A0, Q, d0, B0, theta0,
                               alpha=0.51, step=1, ave_from=500, max_steps=1000)

# estimates
df_A <- data.frame("est_A" = result_sa_conf$A_hat, "true_A" = data$A)
plot(df_A)
df_d <- data.frame("est_d" = result_sa_conf$d_hat, "true_d" = data$difficulty)
plot(df_d)

## standard errors for estimated parameters can be obtained
se_all <- sqrt(diag(solve(result_sa_conf$oakes)))
