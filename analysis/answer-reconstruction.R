# Score recovery without IRT?

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, rowmerge, mytheme, cbPalette])
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
parse.args(
  names = c("BM" ),
  defaults = c("arc"),
  legal = list(
    BM = benchmarks
  )
)
here::i_am("analysis/answer-reconstruction.R")
set.seed(1)

# =============================================================================
# helper functions  
# sample.items <- function(d, target, names, seed){
#   set.seed(seed)
#   names <- names[!names == target]
#   sort(sample(names, d, replace = F))
# }


logistic.train <- function(target, responses){
  formula <- as.formula(glue::glue("{target} ~ ."))
  glm(formula, data = responses, family = binomial(link = "logit"))
}

accuracy <- function(mod.log, responses){
  target <- mod.log[["terms"]][[2]]
  y <- responses[, as.character(target)]
  p <- round(predict(mod.log, type = "response", responses))
  sum(p == y) / length(p)
}

best.coefs <- function(coefs, k, threshold = 0){
  coefs.sorted <- sort(abs(coefs[-1]), decreasing = T)
  above <- coefs.sorted > threshold
  coefs.thresholded <- coefs.sorted[above]
  k <- min(k, sum(above))
  names(coefs.thresholded[1:k])
}

logistic.wrapper <- function(target, predictors = NULL, save.mod = F){
  if (!is.null(predictors)){
    data.train.r <- data.train[, c(target, predictors)]
    data.test.r <- data.test[, c(target, predictors)]
  } else {
    data.train.r <- data.train
    data.test.r <- data.test
  }
  mod.log <- logistic.train(target, data.train.r)
  acc.train <- accuracy(mod.log, data.train.r)
  acc.test <- accuracy(mod.log, data.test.r)
  if (save.mod){
    mod.out = mod.log
  } else {
    mod.out = mod.log$coefficients
  }
  list(target = target,
       acc.train = acc.train,
       acc.test = acc.test,
       mod = mod.out)
}

rmse <- function(df){
  df |> dplyr::mutate(error = p - grand) |>
    dplyr::summarize(rmse = sqrt(mean(error^2))) |> as.numeric()
}


# =============================================================================
# prepare data
gprint("🚰 Loading preprocessed {BM} data...")
datapath <- gpath("data/{BM}-preproc-split.rds")
# datapath <- gpath("data/{BM}-sub-350.rds")

preproc <- readRDS(datapath)
data.train <- preproc$data.train
data.test <- preproc$data.test
colnames(data.train) <- colnames(data.test) <- paste0("i.", colnames(data.train))
# scores.train <- 100 * preproc$scores.train / preproc$max.points.orig
# scores.test <- 100 * preproc$scores.test / preproc$max.points.orig
scores.train <- rowMeans(data.train) * 100
scores.test <- rowMeans(data.test) * 100
items <- colnames(data.train)

# =============================================================================
# train linear model
df.train <- data.frame(grand = scores.train, data.train)
df.test <- data.frame(grand = scores.test, data.test)
mod.lm <- lm("grand ~  .", data = df.train)
df.test$p <- predict(mod.lm, df.test)
rmse(df.test)

# reduce linear model iteratively
target.d <- 100
remaining.d <- ncol(df.train) - target.d
accs <- matrix(NA, nrow = remaining.d, ncol = 5)
mod.lm.r <- mod.lm
for (i in 1:remaining.d){
  # linear model
  weights <- mod.lm.r$coefficients
  coefs <- best.coefs(weights, length(weights) - 2)
  accs[i,1] <- length(coefs)
  gprint("{accs[i,1]} coefs")
  df.train.r <- df.train[, c("grand", coefs)]
  df.test.r <- df.test[, c("grand", coefs)]
  mod.lm.r <- lm("grand ~  .", data = df.train.r)
  df.train.r$p <- predict(mod.lm.r, df.train.r)
  df.test.r$p <- predict(mod.lm.r, df.test.r)
  accs[i,2] <- rmse(df.train.r)
  accs[i,3] <- rmse(df.test.r)
  gprint("LM - {round(accs[i,2],3)} (train), {round(accs[i,3],3)} (test)")
  
  # GAM
  df.train.gam <- df.train.r |> dplyr::mutate(p.lin = p)
  df.test.gam <- df.test.r |> dplyr::mutate(p.lin = p)
  df.train.gam$sub <- df.train.r |> dplyr::select(-c(p, grand)) |> rowMeans()
  df.test.gam$sub <- df.test.r |> dplyr::select(-c(p, grand)) |> rowMeans()
  mod.gam <- mgcv::gam(grand ~ s(p.lin, bs = "ad") + s(sub, bs = "ad"),
                       data = df.train.gam)
  df.train.gam$p <- predict(mod.gam, df.train.gam)
  df.test.gam$p <- predict(mod.gam, df.test.gam)
  accs[i,4] <- rmse(df.train.gam)
  accs[i,5] <- rmse(df.test.gam)
  gprint("GAM - {round(accs[i,4],3)} (train), {round(accs[i,5],3)} (test)")
}
plot(accs[,1], accs[,3], type = "l", col = "red")
lines(accs[,1], accs[,2], type = "l", col = "black")
lines(accs[,1], accs[,5], type = "l", col = "orange")
lines(accs[,1], accs[,4], type = "l", col = "grey")

plot(df.test.r$p, df.test.r$grand, ylim=c(0,100), xlim=c(0,100))
plot(df.test.gam$p, df.test.gam$grand, ylim=c(0,100), xlim=c(0,100))


# =============================================================================
# train lasso model
data.train.lasso <- as.matrix(data.train)
mod.lasso.cv <- glmnet::cv.glmnet(data.train.lasso, scores.train, alpha = 1)
lambda <- 0.9 # mod.lasso.cv$lambda.min
mod.lasso <- glmnet::glmnet(data.train.lasso, scores.train, lambda = lambda, alpha = 1)
p <- predict(mod.lasso, s = lambda, newx = as.matrix(data.test))
plot(p, scores.test)
sqrt(mean((scores.test - p)^2))
mod.lasso

# =============================================================================
# setup parallel processing
box::use(doParallel[...], foreach[...])
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(mu.cluster)

# setup progress bar
niter <- 25 #ncol(data.train)
doSNOW::registerDoSNOW(mu.cluster)
pb <- utils::txtProgressBar(max = niter, style = 3)
progress <- function(n) utils::setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# =============================================================================
# estimate full logistic model for each item
res.full <- foreach(t = items[1:niter], .options.snow = opts) %dopar% {
  logistic.wrapper(t, save.mod = F)
}
close(pb)

# plot accuracies
acc.train <- sapply(res.full, function(x) x$acc.train)
acc.test <- sapply(res.full, function(x) x$acc.test)
hist(acc.train, breaks = 30)
hist(acc.test, breaks = 30)
mean(acc.test)

# =============================================================================
# estimate reduced logistic model

# get best predictors
best.predictors <- lapply(res.full, function(x)
  best.coefs(x$mod, 25, 10))
best.predictors <- na.omit(unique(Reduce(c, best.predictors)))
gprint("{length(best.predictors)} predictors found...") 

# retrain models
remaining.targets <- setdiff(items, best.predictors)
pb <- utils::txtProgressBar(max = length(remaining.targets), style = 3)
progress <- function(n) utils::setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res.reduced <- foreach(t = remaining.targets, .options.snow = opts) %dopar% {
  logistic.wrapper(t, best.predictors, save.mod = T)
}
close(pb)
parallel::stopCluster(mu.cluster)

# plot accuracies
acc.train.r <- sapply(res.reduced, function(x) x$acc.train)
acc.test.r <- sapply(res.reduced, function(x) x$acc.test)
hist(acc.train.r, breaks = 30)
hist(acc.test.r, breaks = 30)
mean(acc.test.r)

# =============================================================================
# check score recovery
points.recon <- sapply(res.reduced, function(x)
  predict(x$mod, type = "response", data.test))
colnames(points.recon) <- sapply(res.reduced, function(x) x$target)
points.known <- data.test[,best.predictors]
points.all <- rowmerge(points.known, points.recon)
df.points <- rowmerge(rowMeans(points.recon) * 100, rowMeans(data.test) * 100) |>
  dplyr::mutate(error = x - y)
sqrt(mean(df.points$error^2))
plot(df.points$x, df.points$y, xlab = "predicted", ylab = "true")

