library(lvmcomp)

# simulation parameters
set.seed(1)
n <- 200 # subjects
m <- 10 # items
l <- 2 # latent abilities
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
responses <- apply(probabilities, c(1,2), p)

# starting values
Q <- matrix(T, m, l)
delta0 <- rep(0, m)
theta0 <- matrix(rnorm(n*l, 0, 1), n)
sigma0 <- diag(1, l)

# fit
res <- StEM_mirt(responses, # observed data
                 Q, # design matrix (all true for exploratory analysis)
                 Q, # A starting values (needs to be boolean for some reason)
                 delta0, theta0, sigma0)

# summary stats
(mse <- mean((A - res$A_hat)^2))
(cor <- cor(as.vector(A), as.vector(res$A_hat)))
