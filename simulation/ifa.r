library(mirt)

# simulate data
set.seed(1)
n <- 50 # number of subjects
d <- 100 # number of items
l <- 2 # number of latent dimensions
zcor <- 0.5 # correlation between latent dimensions

# simulate d times l item parameters form uniform
loadings <- matrix(runif(d*l), d, l)
difficulties <- rnorm(d)

# simulate n subject parameters from multivariate normal
R <- matrix(c(1, zcor, zcor, 1), l, l)
z <- MASS::mvrnorm(n, mu = rep(0, l), Sigma = R)

# simulate responses
x <- matrix(NA, n, d)
for (i in 1:n) {
  x[i, ] <- rbinom(d, 1, plogis(difficulties + loadings %*% z[i, ]))
}

data <- data.frame(x)

# fit IFA model
fit <- mirt(data, l, itemtype = '2PL', method = 'EM')
summary(fit)
