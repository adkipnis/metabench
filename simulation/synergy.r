#================================================================================
# simulate 2 sets of items for 2 latent abilities
# show that if the latent abilities are highly correlated,
# there is more synergy in reconstructing the score of one test for the other
#================================================================================
box::use(../analysis/utils[gprint, gpath, mytheme])
box::use(doParallel[...], foreach[...])
here::i_am("simulation/synergy.R")

# globals
n <- 500 # number of subjects
d <- 100 # number of items per test
l <- 2 # number of latent abilities

# =============================================================================
# helper functions
simulate.theta <- function(n, l, rho){
  box::use(MASS[mvrnorm])
  R <- matrix(rho, l, l)
  diag(R) <- 1
  thetas <- mvrnorm(n, mu=rep(0,l), Sigma=R)
  matrix(thetas, n, l)
}

simulate.items <- function(d, l){
  box::use(stats[runif, rnorm, rbinom])
  loadings <- matrix(runif(d*l, min=0, max=3), d, l)
  difficulties <- matrix(rnorm(d), d, 1)
  list(A=loadings, D=difficulties)
}

reweigh.loadings <- function(items, p){
   A <- items$A
   D <- items$D
   W <- diag(p)
   A.new <- A %*% W
   list(A=A.new, D=D)
}

