# Sample random subsets of a given size from a benchmark and test the score reconstruction
# usage: Rscript random.R {benchmark} {n}

# =============================================================================
box::use(./utils[mkdir, gprint, gpath, parse.args])
here::i_am("analysis/random.R")
parse.args(
   names = c("BM", "N", "seed"),
   defaults = c("gsm8k", 350, 0),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     N = seq(0, 817, 1)
   )
)
N <- as.numeric(N)
set.seed(seed)

# =============================================================================
# helper functions

predict.scores <- function(df.scores, mod.score){
   preds <- predict(mod.score, df.scores, se.fit = T)
   df.scores$p <- preds$fit
   df.scores$p.low <- preds$fit + qnorm(0.005) * preds$se.fit
   df.scores$p.high <- preds$fit + qnorm(0.995) * preds$se.fit
   df.scores |>
      dplyr::mutate(error = means - p,
                    ci.size = p.high - p.low,
                    in.ci = (means >= p.low) & (means <= p.high))
}

subsample <- function(data, k){
  n <- ncol(data)
  sort(sample(1:n, k, replace = F))
}

subsample.wrapper <- function(seed){
   set.seed(seed)
   
   # subsample same items from train and test data
   indices.rand <- subsample(data.train, N)
   data.train.r <- data.train[,indices.rand]
   data.val.r <- data.val[,indices.rand]
   data.test.r <- data.test[,indices.rand]

   # recalculate mean scores and prepare score dfs
   scores.train.r <- rowMeans(data.train.r)
   scores.val.r <- rowMeans(data.val.r)
   scores.test.r <- rowMeans(data.test.r)
   df.train <- data.frame(sub.score = scores.train.r, means = scores.train)
   df.val <- data.frame(sub.score = scores.val.r, means = scores.val)
   df.test <- data.frame(sub.score = scores.test.r, means = scores.test)

   # train GAM on train data and predict on val/test set
   mod.score <- mgcv::gam(means ~ s(sub.score, bs = "ps"), data = df.train)
   df.val <- predict.scores(df.val, mod.score)
   df.test <- predict.scores(df.test, mod.score)

   # export results
   list(
      indices.rand = indices.rand,
      mod.score = mod.score,
      rmse.val = sqrt(mean(df.val$error^2)),
      rmse.val.ci.size = mean(df.val$ci.size),
      rmse.val.ci.coverage = mean(df.val$in.ci),
      rmse.test = sqrt(mean(df.test$error^2)),
      rmse.test.ci.size = mean(df.test$ci.size),
      remse.test.ci.coverage = mean(df.test$in.ci),
      seed = seed
   )
}

bind.results <- function(results){
  results.tmp <- results
  results.tmp <- lapply(results.tmp, function(x) x[-c(1, 2)])
  out <- as.data.frame(do.call(rbind, results.tmp))
  out <- as.data.frame(lapply(out, as.numeric))
  out 
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM}...")
full <- readRDS(gpath("data/{BM}-preproc-split.rds"))
data <- full$data.train
nc <- full$max.points.orig
scores <- full$scores.train / nc * 100

# split data
indices <- caret::createDataPartition(scores, p = 0.1, list = F)
data.train <- data[indices,]
data.val <- data[-indices,]
scores.train <- scores[indices]
scores.val <- scores[-indices]

# test on test data
data.test <- full$data.test
scores.test <- full$scores.test / nc * 100

if (N > ncol(data.train)){
  gprint("Benchmark only has {ncol(data.train)} items but {N} were specified. Aborting...")
  quit()
}

# =============================================================================
# setup parallel processing
box::use(doParallel[...], foreach[...])
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(mu.cluster)

# =============================================================================
# run subsampling
gprint("🔁 Running 10000 subsampling iterations with {N} items...")
res.full <- foreach(i = 1:10000) %dopar% {
   subsample.wrapper(i)
}
parallel::stopCluster(mu.cluster)
res <- bind.results(res.full)

# plot distribution of RMSEs
# plot(density(res$rmse.test))

# get best result
min.index <- which.min(res$rmse.val)
gprint("📊 Best Validation RMSE: {round(res$rmse.val[min.index], 3)}, Median: {round(median(res$rmse.val), 3)}")
gprint("📊 Best Test RMSE: {round(min(res$rmse.test), 3)}, Median: {round(median(res$rmse.test), 3)}")
rmse.test <- res$rmse.test[min.index]
rmse.test.ci <- res$rmse.test.ci.size[min.index]
rmse.test.coverage <- res$remse.test.ci.coverage[min.index]
gprint("📊 Test RMSE of chosen set: {round(rmse.test, 3)}, mean 99%-CI size: {round(rmse.test.ci,3)}, CI-coverage: {round(100 * rmse.test.coverage, 1)}% ")
indices.rand <- res.full[[min.index]]$indices.rand
mod.score <- res.full[[min.index]]$mod.score

# collect results
data.train.s <- data[,indices.rand]
data.test.s <- data.test[,indices.rand]
item.indices <- which(full$items$item %in% colnames(data.train.s))
items.s <- full$items[item.indices,]
out <- list(
   data.train = data.train.s,
   data.test = data.test.s,
   scores.train = full$scores.train,
   scores.test = full$scores.test,
   max.points.orig = full$max.points.orig,
   items = items.s,
   mod.score = mod.score,
   rmses.val = res$rmse.val,
   rmses.test = res$rmse.test,
   rmse.test = rmse.test
)

# save data
outpath <- gpath("data/{BM}-sub-{N}.rds")
saveRDS(out, outpath)
gprint("🏁 Saved subset data to {outpath}")

