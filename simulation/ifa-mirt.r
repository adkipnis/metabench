# load dependencies
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("sim-methods.r")
library(mirt)

#===============================================================================
n <- 4000
d <- 800
l <- 2 
data <- simulate_wide(seed=12, n=n, d=d, l=l, rho=0.3)

# starting guesses and constraints
pars <- mirt(data$responses, l, itemtype = '2PL', pars = 'values')
pars_mod <- pars %>%
  mutate(est = ifelse(name=="COV_21", T, est)) %>% # allow factor covariance
  mutate(lbound = ifelse(startsWith(name, "a"), 0, lbound)) %>% # lower bound loadings
  mutate(value = ifelse(startsWith(name, "a"), runif(d), value)) %>%
  mutate(value = ifelse(name == "d", rnorm(d), value)) # difficulty is called d in pars

# fit
mirtCluster()
mirtCluster(remove = T)
res <- mirt(data$responses, l, itemtype = '2PL', method = 'EM',
            pars = pars_mod,
            verbose = T,
            
            )
estimates <-coef(res, simplify=T, rotate="none")$items
d_hat <- estimates[,"d"]
A_hat <- estimates[,1:l]
A_reordered <- reorder_cols(data$A, A_hat)

# summary stats
(mse <- mean((A_reordered - A_hat)^2))
(cor <- cor(as.vector(A_reordered), as.vector(A_hat)))

# vector comparisons
par(mfrow=c(2,1))
plot(as.vector(A_reordered), as.vector(A_hat), xlim=c(0, 10), ylim=c(0,10))
plot(data$difficulty, -d_hat)

cor(A_reordered, A_hat)

# matrix comparisons
par(mfrow=c(1,2))
plot(A_reordered, xlab="Factor", ylab="Item", main="Original")
plot(A_hat, xlab="Factor", ylab="Item", main="Estimate")

