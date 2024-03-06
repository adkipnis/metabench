library(mirt)
library(dplyr)

# simulation parameters
set.seed(1)
n <- 200 # subjects
m <- 50 # items
l <- 3 # latent abilities
rho <- 0.5 # correlation between abilities 

# latent data
R <- matrix(rho, l, l) + diag(l) * (1 - rho)
theta <- MASS::mvrnorm(n, mu=rep(0,l), Sigma=R)
A <- matrix(runif(m * l, 0.5, 1.5), m, l)
delta <- rnorm(m)

# manifest data
linear_term <- theta %*% t(A) + delta
probabilities <- plogis(linear_term)
p <- function(x) rbinom(1,1,x)
responses <- data.frame(apply(probabilities, c(1,2), p))

# constraints
pars <- mirt(responses, l, itemtype = '2PL', pars = 'values')
pars_mod <- pars %>%
  mutate(est = ifelse(name=="COV_21", T, est)) %>% # allow factor covariance
  mutate(lbound = ifelse(startsWith(name, "a"), 0, lbound)) %>% # lower bound loadings
  mutate(value = ifelse(startsWith(name, "a"), runif(m), value)) %>%
  mutate(value = ifelse(name == "d", rnorm(m), value))

# fit
res <- mirt(responses, l, itemtype = '2PL', method = 'MHRM', pars = pars_mod)
A_hat <- coef(res, simplify=T)$items[,1:l]

# summary stats
(mse <- mean((A - A_hat)^2))
(cor <- cor(as.vector(A), as.vector(A_hat)))
