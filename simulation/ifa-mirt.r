library(mirt)
library(clue)
library(dplyr)
library(plot.matrix)

#===============================================================================
# Helper Functions

# wrapper for Hungarian algorithm for matrix matching
hungarian <- function(corr_mat) {
  nonneg_mat <- corr_mat + 1
  matching <- solve_LSAP(nonneg_mat, maximum=T)
  selected_entries <- corr_mat[cbind(1:nrow(corr_mat), matching)]
  max_sum <- sum(selected_entries)
  return(list(columns=matching, selected_entries=selected_entries))
}

reorder_cols <- function(mat1, mat2){
  cor <- cor(mat1, mat2)
  result <- hungarian(cor)
  mat1_reordered <- mat1[, result$columns]
  return(mat1_reordered)
}
#===============================================================================

# simulation parameters
set.seed(1)
n <- 100 # subjects
m <- 200 # items
l <- 3 # latent abilities
rho <- 0.3 # correlation between abilities 

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
  # mutate(ubound = ifelse(startsWith(name, "a"), 1, ubound)) %>% # upper bound loadings
  mutate(value = ifelse(startsWith(name, "a"), runif(m), value)) %>%
  mutate(value = ifelse(name == "d", rnorm(m), value))

# fit
res <- mirt(responses, l, itemtype = '2PL', method = 'MHRM',
            pars = pars_mod,
            )
A_hat <- coef(res, simplify=T, rotate="none")$items[,1:l]
A_reordered <- reorder_cols(A, A_hat)

# summary stats
(mse <- mean((A_reordered - A_hat)^2))
(cor <- cor(as.vector(A_reordered), as.vector(A_hat)))

# summary plots
par(mfrow=c(1,1))
plot(as.vector(A_reordered), as.vector(A_hat), xlim=c(0, 1.5), ylim=c(0,1.5)) 
par(mfrow=c(1,2))
plot(A_reordered, xlab="Factor", ylab="Item", main="Original")
plot(A_hat, xlab="Factor", ylab="Item", main="Estimate")


