# load dependencies
rm(list=ls())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(TAM)

#===============================================================================
n <- 4000
d <- 1000
l <- 3 
data <- simulate_wide(seed=12, n=n, d=d, l=l, rho=0.3)

# design matrix
Q <- array(1, dim=c(d, l))
rownames(Q) <- colnames(data$responses) 

# the heavy lifting
result <- tam.mml.2pl(data$responses, Q=Q,
                      control=list(maxiter=50, snodes=1000))

result$WLEreliability
plot(mod1)
wrightMap(mod1$WLE, mod1$xsi, item.side=itemClassic)

#-------------------------------------------------------------------------------
# item parameters
d_hat <- -result$item$xsi.item
A_hat <- cbind(result$item$B.Cat1.Dim1,
               result$item$B.Cat1.Dim2,
               result$item$B.Cat1.Dim3)
A_reordered <- reorder_cols(data$A, A_hat)

# summary stats
(mse <- mean((A_reordered - A_hat)^2))
(cor <- cor(as.vector(A_reordered), as.vector(A_hat)))
(cor(data$difficulty, d_hat))

# vector comparisons
par(mfrow=c(2,1))
plot(as.vector(A_reordered), as.vector(A_hat))
plot(data$difficulty, d_hat)
cor(A_reordered, A_hat)

#-------------------------------------------------------------------------------
# person parameters
abil <- tam.mml.wle(result, WLE=F, Msteps=40, convM=1e-04)
theta_hat <- cbind(abil$theta.Dim01,
                   abil$theta.Dim02,
                   abil$theta.Dim03)
theta_reordered <- reorder_cols(data$theta, theta_hat)
plot(as.vector(theta_reordered), as.vector(theta_hat))
cor(as.vector(theta_reordered), as.vector(theta_hat))
