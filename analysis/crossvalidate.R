# cross-validated IRT fitting of preprocessed data
# goal: determine the best IRT model for the given benchmark
# usage: Rscript crossvalidate.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(
   names = c("BM"),
   defaults = c("arc"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/crossvalidate.R")
mkdir("analysis/models")
set.seed(1)

# =============================================================================
# helper functions  

subset.score <- function(df.score, indices, theta) {
  df <- df.score[indices, ]
  df$theta <- theta[, 1]
  df |>
    dplyr::arrange(theta) |>
    dplyr::mutate(rank.theta = rank(theta),
                  perc.theta = rank.theta / max(rank.theta))
}


plot.prediction <- function(df.score, set) {
  p <- ggplot(df.score, aes(x = theta, y = score)) +
    geom_point() +
    geom_line(aes(y = p), color = 'red') +
    labs(x = expression(theta), y = 'Score') +
    ggtitle(glue("Score recovery ({set} set)"))
  return(p)
}


cv.fold <- function(fold, itemtype) {
  # prepare data
  train <- data[-fold, ]
  test <- data[fold, ]
  std.train <- apply(train, 2, sd)
  std.test <- apply(test, 2, sd)
  item.ids <- which(std.train > 0 & std.test > 0)
  gprint("🧹 Removing {ncol(train) - length(item.ids)} items with zero variance.")
  train <- train[, item.ids]
  test <- test[, item.ids]
  
  # fit model
  gprint("⚙️ Fitting {itemtype} model to training fold...")
  model <- run.mirt(train, itemtype)
  
  # train performance
  theta.train <- get.theta(model, method = "MAP")
  df.train <- subset.score(df.score, -fold, theta.train)
  mod.score <- mgcv::gam(score ~ s(theta), data = df.train)
  df.train$p <- predict(mod.score)
  
  # test performance
  theta.test <- get.theta(model, method = "MAP", resp = test)
  df.test <- subset.score(df.score, fold, theta.test)
  df.test$p <- predict(mod.score, newdata = df.test)

  # collaps both dataframes
  df.train$set <- "train"
  df.test$set <- "test"
  list(df = rbind(df.train, df.test), model = model)
}


cv.wrapper <- function(folds, itemtype) {
  results <- list()
  for (i in 1:length(folds)) {
    gprint("🔁 Cross-validation fold {i}...")
    result <- cv.fold(folds[[i]], itemtype)
    results[[i]] <- result
  }
  return(results)
}

# =============================================================================
# prepare data
gprint("🚰 Loading preprocessed {BM} data...")
if (BM %in% c("hellaswag", "mmlu")){
   datapath <- gpath("data/{BM}-sub.rds")
} else {
   datapath <- gpath("data/{BM}-preproc-split.rds")
}
preproc <- readRDS(datapath)
data <- preproc$data
scores <- 100 * preproc$scores.orig / preproc$max.points.orig

# init df.score
df.score <- data.frame(score = scores) |>
  dplyr::mutate(rank.score = rank(score),
                perc.score = rank.score / max(rank.score))

# init 10-fold CV split (stratified wrt. scores)
folds <- caret::createFolds(scores, k = 10, list = T)

# =============================================================================
# cv models
cv.3pl <- cv.wrapper(folds, "3PL")
cv.3plu <- cv.wrapper(folds, "3PLu")
cv.4pl <- cv.wrapper(folds, "4PL")
cvs <- list(`3PL`=cv.3pl, `3PLu`=cv.3plu, `4PL`=cv.4pl)
outpath <- gpath("analysis/models/{BM}-cv.rds")
saveRDS(cvs, outpath)
gprint("💾 Saved to '{outpath}'.")
