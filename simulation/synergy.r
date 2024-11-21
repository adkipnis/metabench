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

simulate.responses <- function(test, thetas){
  A <- test$A
  D <- test$D
  n <- nrow(thetas)
  d <- nrow(A)
  inner <- cbind(rep(1,n), thetas) %*% t(cbind(D, A))
  # sanity check:
  # i = 4
  # j = 20
  # thetas[j,] %*% A[i,] + D[i,1] == inner[j,i]
  prob <- 1/(1+exp(-inner))
  responses <- rbinom(n*d, 1, prob = prob)
  data.frame(matrix(responses, n, d))
}

rmse <- function(df){
   df$error <- df$p - df$score
   sqrt(mean(df$error^2))
}

evaluate <- function(data.train, data.test, model){
   data.train$p <- predict(model, data.train)
   data.test$p <- predict(model, data.test)
   rmse.train <- rmse(data.train)
   rmse.test <- rmse(data.test)
   # gprint("train: {round(rmse.train, 3)} - test: {round(rmse.test, 3)}")
   list(train=rmse.train, test=rmse.test)
}

run <- function(rho, alpha, seed, show.plots=F){
   set.seed(seed)
   # ---------------------------------------------------------------------------
   # latent ability
   thetas <- simulate.theta(n, l, rho)
   if (show.plots){
      par(mfrow=c(1,1))
      plot(thetas)
   }

   # tests
   items.1 <- simulate.items(d, l)
   items.2 <- simulate.items(d, l)
   items.1 <- reweigh.loadings(items.1, c(1, alpha))
   items.2 <- reweigh.loadings(items.2, c(0, 1))
   
   # responses
   responses.1 <- simulate.responses(items.1, thetas)
   responses.2 <- simulate.responses(items.2, thetas)

   # scores
   scores <- matrix(NA, n, 2)
   scores[,1] <- rowMeans(responses.1) * 100
   scores[,2] <- rowMeans(responses.2) * 100
   cor(scores)

   # relationship between scores and thetas
   if (show.plots){
      par(mfrow=c(2,2))
      plot(scores[,1], thetas[,1])
      plot(scores[,1], thetas[,2])
      plot(scores[,2], thetas[,1])
      plot(scores[,2], thetas[,2])
   }

   # ---------------------------------------------------------------------------
   # IRT
   fit.1 <- mirt::mirt(responses.1, 1, itemtype = '2PL', method = 'EM')
   fit.2 <- mirt::mirt(responses.2, 1, itemtype = '2PL', method = 'EM')
   coefs.1 <- as.data.frame(mirt::coef(fit.1, simplify=T)$items)
   coefs.2 <- as.data.frame(mirt::coef(fit.2, simplify=T)$items)

   # plot parameter recovery
   if (show.plots){
      par(mfrow=c(3,1))
      plot(coefs.1$a1, items.1$A[,1])
      plot(coefs.1$a1, items.1$A[,2])
      plot(coefs.1$d, items.1$D)
   }

   # estimate thetas
   thetas.est <- matrix(NA, n, 2)
   thetas.est[,1] <- mirt::fscores(fit.1, method = 'MAP')
   thetas.est[,2] <- mirt::fscores(fit.2, method = 'MAP')
   if (show.plots){
      par(mfrow=c(2,1))
      plot(thetas[,1], thetas.est[,1])
      plot(thetas[,2], thetas.est[,2])
   }

