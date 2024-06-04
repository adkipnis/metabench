# Sample random subsets of a given size from a benchmark and test the score reconstruction
# usage: Rscript random.R {benchmark} {n}

# =============================================================================
box::use(./utils[mkdir, gprint, gpath, parse.args])
here::i_am("analysis/random.R")
parse.args(
   names = c("BM", "N"), 
   defaults = c("mmlu", 400),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     N = seq(0, 400, 1)
   )
)
N <- as.numeric(N)
SEED <- 1
set.seed(SEED)

# =============================================================================
# helper functions

predict.scores <- function(df.scores, mod.score){
   df.scores$p <- predict(mod.score, df.scores)
   df.scores |>
      dplyr::mutate(error = means - p)
}

evaluate.prediction <- function(df.scores){
  df.scores |> 
    dplyr::summarise(RMSE = sqrt(mean(error^2)) )
}

subsample <- function(data, k){
  n <- ncol(data)
  sort(sample(1:n, k, replace = F))
}


# =============================================================================
# prepare hellaswag
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

if (N > ncol(data.train)){
  gprint("Benchmark only has {ncol(data.train)} items but {N} were specified. Aborting...")
  quit()
}

# check with random subset of equal size
rmses <- matrix(NA, 1000)
index.list <- list()
mod.list <- list()

gprint("🔁 Running 1000 subsampling iterations with {N} items with seed {SEED}...")
for (i in 1:1000){
  # subsample same items from train and test data
  indices.rand <- subsample(data.train, N)
  data.train.r <- data.train[,indices.rand]
  data.val.r <- data.val[,indices.rand]

  # recalculate mean scores and prepare score dfs
  scores.train.r <- rowMeans(data.train.r)
  df.train <- data.frame(sub.score = scores.train.r, means = scores.train)
  scores.val.r <- rowMeans(data.val.r)
  df.val <- data.frame(sub.score = scores.val.r, means = scores.val)

  # train GAM on train data and predict on test data
  mod.score <- mgcv::gam(means ~ s(sub.score, bs = "ad"), data = df.train)
  df.val <- predict.scores(df.val, mod.score)

  # save results
  rmses[i,] <- sqrt(mean(df.val$error^2))
  index.list[[i]] <- indices.rand
  mod.list[[i]] <- mod.score
}

# get smallest RMSE
min.index <- which.min(rmses)
indices.rand <- index.list[[min.index]]
mod.score <- mod.list[[min.index]]
gprint("📊 Best Validation RMSE: {round(rmses[min.index], 3)}")

# test on test data
data.test <- full$data.test
scores.test <- full$scores.test / nc * 100
data.test.r <- data.test[,indices.rand]
scores.test.r <- rowMeans(data.test.r)
df.test <- data.frame(sub.score = scores.test.r, means = scores.test)
df.test <- predict.scores(df.test, mod.score)
rmse.test <- sqrt(mean(df.test$error^2))
gprint("📊 Test RMSE: {round(rmse.test, 3)}")

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
   rmses.val = rmses,
   rmse.test = rmse.test
)

 # save data
 outpath <- gpath("analysis/reduced/{BM}-sub.rds")
 saveRDS(out, outpath)
 gprint("🏁 Saved subset data to {outpath}")

