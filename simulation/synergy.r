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

   # ---------------------------------------------------------------------------
   # perpare data
   data <- data.frame(score = scores[,1],
                     theta.1 = thetas.est[,1],
                     theta.2 = thetas.est[,2],
                     test = 1,
                     subject = 1:nrow(scores))
   idx <- caret::createDataPartition(scores[,1], p = 0.1, list = F)
   data.train <- data[-idx, ]
   data.test <- data[idx, ]

   # fit specific gam
   gam.1 <- mgcv::gam(score ~ s(theta.1, bs="ad"),
                     data = data.train)
   res.1 <- evaluate(data.train, data.test, gam.1)

   # fit joint gam
   gam.2 <- mgcv::gam(score ~ s(theta.1, bs="ad") + s(theta.2, bs="ad"), 
                     data = data.train)
   res.2 <- evaluate(data.train, data.test, gam.2)

   # output
   data.frame(rho=rho, alpha=alpha,
              single.train=res.1$train,
              single.test=res.1$test,
              joint.train=res.2$train,
              joint.test=res.2$test)
}

plot.mat <- function(results){
  # results.mat <- xtabs(gain ~ rho + alpha, data = results)
  # heatmap(results.mat)
  box::use(ggplot2[...])
  results |> ggplot(aes(rho, alpha, fill=Boost)) +
    geom_tile() +
    scale_fill_gradient2(low = "tan3",
                         mid = "white",
                         high = "cornflowerblue",
                         midpoint = 0,
                         limits = c(-1, 1))+
    scale_x_continuous(breaks = unique(results$rho), expand = c(0,0)) +
    scale_y_continuous(breaks = unique(results$alpha), expand = c(0,0)) +
    mytheme() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = "Cor(Abilities)", y = "Dep(2nd Ability)")
}


# =============================================================================
# main
rhos <- seq(0, 1, length.out = 11) # correlation between latent abilities
alphas <- seq(0, 1, length.out = 11) # uniqueness per test
df.index <- expand.grid(rho = rhos, alpha = alphas, seeds = 1:5) |>
            dplyr::arrange(rho, alpha)
niter <- nrow(df.index)

# setup parallel processing and progress bar
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(mu.cluster)
doSNOW::registerDoSNOW(mu.cluster)
pb <- utils::txtProgressBar(max = niter, style = 3)
progress <- function(n) utils::setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# run grid
res.list <- foreach(i = 1:niter, .options.snow = opts) %dopar% {
  rho <- df.index$rho[i]
  alpha <- df.index$alpha[i]
  seed <- df.index$seeds[i]
  run(rho, alpha, seed)
}
close(pb)
parallel::stopCluster(mu.cluster)

# tidy up and plot results
results <- do.call(rbind, res.list) |> 
   dplyr::mutate(boost = single.test - joint.test) |>
   dplyr::group_by(alpha, rho) |>
   dplyr::summarize(Boost = mean(boost))
(p.mat <- plot.mat(results))
outpath <- gpath("figures/f.synergy.pdf")
ggplot2::ggsave(outpath, p.mat, width = 7, height = 6)

# save rest
outpath <- gpath("simulation/synergy.rds")
out <- list(p.mat = p.mat, res.list = res.list)
saveRDS(out, outpath)
