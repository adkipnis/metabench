library(dplyr)
library(tidyr)
library(plot.matrix)

simulate_wide <- function(seed=1, n=100, d=10, l=1, rho=0.0){
  # simulates a dataframe (n by d) of M2PL responses
  #
  # seed (int) - for reproducibility
  # n (int) - number of subjects
  # d (int) - number of items
  # l (int) - number of latent abilities
  # rho (float) - correlation of latent abilities
  # -------------------------------------------------------
  set.seed(seed)
  
  # person parameters
  R <- matrix(rho, l, l) + diag(l) * (1 - rho)
  theta <- MASS::mvrnorm(n, mu=rep(0,l), Sigma=R)
  
  # item parameters
  A <- matrix(runif(d * l), d, l)
  difficulty <- rnorm(d, sd=1+log(l))
  
  # manifest data
  linear_term <- theta %*% t(A) - difficulty
  probabilities <- plogis(linear_term)
  p <- function(x) rbinom(1,1,x)
  responses <- data.frame(apply(probabilities, c(1,2), p))
  
  # output
  out <- list(theta=theta, A=A, difficulty=difficulty, responses=responses)
  return(out)
}

simulate_long <- function(seed=1, n=100, d=10, l=1, rho=0.0){
  # long version of the above
  out <- simulate_wide(seed=seed, n=n, d=d, l=l, rho=rho)
  long <- out$responses %>%
    mutate(subject = row_number()) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "item",
      names_prefix = "X",
      values_to = "correct",
    )
  out$responses <- long
  return(out)  
}

plot_simulated <- function(example){
  # plots parameters and responses of example simulation dataset as matrix heatmaps
  
  # item parameters
  par(mfrow=c(1,2))
  plot(example$A, main="loadings", ylab="item", xlab="ability")
  plot(as.matrix(example$difficulty), main="difficulties", ylab="item", xlab="")
  
  # responses and person params
  par(mfrow=c(2,1))
  plot(t(as.matrix(example$responses)), main="Responses", ylab="item", xlab="subject")
  plot(t(example$theta), main="abilities", xlab="subject", ylab="ability")
}

plot_simulated(simulate_wide(l=2))

