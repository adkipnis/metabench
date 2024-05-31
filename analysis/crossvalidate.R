# cross-validated IRT fitting of preprocessed data
# goal: determine the best IRT model for the given benchmark
# usage: Rscript crossvalidate.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(
   names = c("BM"),
   defaults = c("winogrande"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/crossvalidate.R")
mkdir("analysis/models")
set.seed(1)

# =============================================================================
# helper functions  

make.df.score <- function(scores, theta) {
   data.frame(score = scores, theta = theta[,1]) |>
      dplyr::mutate(rank.score = rank(score),
                    perc.score = rank.score / max(rank.score),
                    rank.theta = rank(theta),
                    perc.theta = rank.theta / max(rank.theta))
}

cross.validate <- function(itemtype){
  # fit model
  gprint("⚙️ Fitting {itemtype} model to training fold...")
  model <- run.mirt(data.train, itemtype)
 
  # train performance
  theta.train <- get.theta(model, method = "MAP")
  df.train <- make.df.score(scores.train, theta.train)
  mod.score <- mgcv::gam(score ~ s(theta), data = df.train)
  df.train$p <- predict(mod.score)
  
  # test performance
  theta.test <- get.theta(model, method = "MAP", resp = data.test)
  df.test <- make.df.score(scores.test, theta.test)
  df.test$p <- predict(mod.score, newdata = df.test)

  # collaps both dataframes
  df.train$set <- "train"
  df.test$set <- "test"
  list(df = rbind(df.train, df.test), model = model)
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
data.train <- preproc$data.train
data.test <- preproc$data.test
scores.train <- 100 * preproc$scores.train / preproc$max.points.orig
scores.test <- 100 * preproc$scores.test / preproc$max.points.orig

# =============================================================================
# cv models
cv.2pl <- cross.validate("2PL")
cv.3pl <- cross.validate("3PL")
cv.3plu <- cross.validate("3PLu")
cv.4pl <- cross.validate("4PL")
cvs <- list(`2PL`=cv.2pl, `3PL`=cv.3pl, `3PLu`=cv.3plu, `4PL`=cv.4pl)
outpath <- gpath("analysis/models/{BM}-cv.rds")
saveRDS(cvs, outpath)
gprint("💾 Saved to '{outpath}'.")
