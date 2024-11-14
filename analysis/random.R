# Sample random subsets of a given size from a benchmark and test the score reconstruction
# usage: Rscript random.R {benchmark} {n}

# =============================================================================
box::use(./utils[mkdir, gprint, gpath, parse.args])
here::i_am("analysis/random.R")
parse.args(
   names = c("BM", "N", "seed"),
   defaults = c("arc", 350, 1),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     N = seq(0, 817, 1)
   )
)
N <- as.numeric(N)
seed <- as.numeric(seed)
set.seed(seed)
skip.reduced <- F # remove items used in original run

# =============================================================================
# helper functions
split <- function(fold){
   indices.tmp <- unlist(indices[fold])
   list(data.train = data[-indices.tmp,],
        data.val = data[indices.tmp,],
        scores.train = scores[-indices.tmp],
        scores.val = scores[indices.tmp])
}

predict.scores <- function(df.scores, mod.score){
   df.scores$p <- predict(mod.score, df.scores)
   df.scores |> dplyr::mutate(error = means - p)
}

subsample <- function(data, k){
  n <- ncol(data)
  sort(sample(1:n, k, replace = F))
}

subsample.wrapper <- function(seed.local, fold){
  # reproducible index sampling
  set.seed(seed.local)
  indices.rand <- subsample(data, N)
  
  # 5-fold data split
  data.split <- split(fold)
  data.train <- data.split$data.train
  data.val <- data.split$data.val
  scores.train <- data.split$scores.train
  scores.val <- data.split$scores.val
  
  # subsample same items from train and test data
  data.train.r <- data.train[,indices.rand]
  data.val.r <- data.val[,indices.rand]
  data.test.r <- data.test[,indices.rand]
  
  # train gam model
  df.train <- data.frame(means = scores.train, reduced = rowMeans(data.train.r))
  df.val <- data.frame(means = scores.val, reduced = rowMeans(data.val.r))
  df.test <- data.frame(means = scores.test, reduced = rowMeans(data.test.r))
  mod.score <- mgcv::gam(means ~ s(reduced, bs = 'ad'), data = df.train)

  # # alternatively train weighted average model (slower and not necessarily better)
  # df.train <- data.frame(means = scores.train, data.train.r)
  # df.val <- data.frame(means = scores.val, data.val.r)
  # df.test <- data.frame(means = scores.test, data.test.r)
  # mod.score <- lm(means ~ 0 + ., data = df.train)
  
  # predict on val/test set
  df.val <- predict.scores(df.val, mod.score)
  df.test <- predict.scores(df.test, mod.score)
  
  # output
  list(indices.rand = indices.rand,
       seed = seed.local,
       fold = fold,
       rmse.val = sqrt(mean((df.val$error)^2)),
       rmse.test = sqrt(mean((df.test$error)^2))
  )
}


bind.results <- function(results){
  results.tmp <- results
  results.tmp <- lapply(results.tmp, function(x) x[-1])
  out <- as.data.frame(do.call(rbind, results.tmp))
  out <- as.data.frame(lapply(out, as.numeric))
  out 
}

benchmarks <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
  winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
)

load.reduced <- function(bm, seed.old = 1){
   mod <- benchmarks[[bm]]$mod
   est <- benchmarks[[bm]]$est
   lam <- benchmarks[[bm]]$lam
   path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}-seed={seed.old}.rds")
   readRDS(path)$items$item
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM}...")
full <- readRDS(gpath("data/{BM}-preproc-split-seed={seed}.rds"))
data <- full$data.train
nc <- full$max.points.orig
scores <- full$scores.train / nc * 100

# test on test data
data.test <- full$data.test
scores.test <- full$scores.test / nc * 100

# optionally remove items used in previous runs
if (skip.reduced){
  gprint("Removing items used in previous runs with seed 1...")
  reduced <- load.reduced(BM, 1)
  data <- data[,!colnames(data) %in% reduced]
  data.test <- data.test[,!colnames(data.test) %in% reduced]
}

# check if N is valid
if (N > ncol(data)){
  gprint("Benchmark only has {ncol(data)} items but {N} were specified. Aborting...")
  quit()
}

# 5-fold cross-validation split
indices <- caret::createFolds(scores, k = 5, list = T) # row indices per fold
gprint("🔁 Running 10000 subsampling iterations with {N} items and 5 folds...")

# =============================================================================
# setup parallel processing
box::use(doParallel[...], foreach[...])
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(mu.cluster)
end.seed <- 1e4 * seed
start.seed <- end.seed - 1e4 + 1
df.index <- expand.grid(seed = start.seed:end.seed, fold = 1:5) |>
   dplyr::arrange(seed, fold)
niter <- nrow(df.index)

# setup progress bar
doSNOW::registerDoSNOW(mu.cluster)
pb <- utils::txtProgressBar(max = niter, style = 3)
progress <- function(n) utils::setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# =============================================================================
# run subsampling
res.full <- foreach(i = 1:niter, .options.snow = opts) %dopar% {
  seed <- df.index$seed[i]
  fold <- df.index$fold[i]
  subsample.wrapper(seed, fold)
}
close(pb)
parallel::stopCluster(mu.cluster)

# tidy up results
res <- bind.results(res.full) |>
   # make data frame wide by seed (rmse.val.1, rmse.val.2, ...)
   tidyr::pivot_wider(names_from = fold,
                      values_from = c(rmse.val, rmse.test),
                      names_sep = ".")
res <- res |> dplyr::mutate(
   max.rmse.val = apply(dplyr::select(res, starts_with("rmse.val")), 1, max),
   max.rmse.test = apply(dplyr::select(res, starts_with("rmse.test")), 1, max),
   mean.rmse.val = apply(dplyr::select(res, starts_with("rmse.val")), 1, mean),
   mean.rmse.test = apply(dplyr::select(res, starts_with("rmse.test")), 1, mean)
  )

# # plot results
# par(mfrow = c(1,2))
# plot(res$mean.rmse.val, res$mean.rmse.test,
#      xlab = "mean(Validation)", ylab = "mean(Test)")
# plot(res$max.rmse.val, res$mean.rmse.test,
#      xlab = "max(Validation)", ylab = "mean(Test)")

# get best result (minimax)
min.index <- which.min(res$max.rmse.val)
gprint("📊 Minimax validation RMSE: {round(res$max.rmse.val[min.index], 3)}, Median: {round(median(res$max.rmse.val), 3)}")
gprint("📊 Minimax test RMSE: {round(min(res$max.rmse.test), 3)}, Median: {round(median(res$max.rmse.test), 3)}")
rmse.test <- res$mean.rmse.test[min.index]
gprint("📊 Mean test RMSE of chosen set: {round(rmse.test, 3)}")

# find entry in which res.full has the seed == min.index
idx.tmp <- which(df.index$seed == min.index)[1]
indices.rand <- res.full[[idx.tmp]]$indices.rand

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
   rmses.val = res$mean.rmse.val,
   rmses.val.max = res$max.rmse.val,
   rmses.test = res$mean.rmse.test,
   rmses.test.max = res$max.rmse.test,
   rmse.test = rmse.test
)

# save data
suffix <- ifelse(skip.reduced, glue::glue("-v2"), "")
outpath <- gpath("data/{BM}-sub-{N}-seed={seed}{suffix}.rds")
saveRDS(out, outpath)
gprint("🏁 Saved subset data to {outpath}")

