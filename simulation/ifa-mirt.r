library(mirt)
library(dplyr)

# simulation parameters
set.seed(1)
n <- 1000 # subjects
m <- 25 # items
l <- 2 # latent abilities
rho <- 0. # correlation between abilities 

# latent data
R <- matrix(rho, l, l) + diag(l) * (1 - rho)
theta <- MASS::mvrnorm(n, mu=rep(0,l), Sigma=R)
A <- matrix(runif(m * l), m, l)
delta <- rnorm(m)

# manifest data
linear_term <- theta %*% t(A) + delta
probabilities <- plogis(linear_term)
p <- function(x) rbinom(1,1,x)
responses <- data.frame(apply(probabilities, c(1,2), p))

# starting guesses and constraints
pars <- mirt(responses, l, itemtype = '2PL', pars = 'values')
pars_mod <- pars %>%
  mutate(est = ifelse(name=="COV_21", T, est)) %>% # allow factor covariance
  mutate(lbound = ifelse(startsWith(name, "a"), 0, lbound)) %>% # lower bound loadings
  mutate(value = ifelse(startsWith(name, "a"), runif(m), value)) %>%
  mutate(value = ifelse(name == "d", rnorm(m), value))

# fit
res <- mirt(responses, l, itemtype = '2PL', method = 'MHRM',
            pars = pars_mod,
            )
summary(res)

# summary stats
A_hat <- coef(res, simplify=T)$items[,1:l]
(mse <- mean((A - A_hat)^2))
(cor <- cor(as.vector(A), as.vector(A_hat)))

# summary plots
# plot(as.vector(A), as.vector(A_hat))
# theta_hat <- fscores(res)
# plot(as.vector(theta), as.vector(theta_hat))
