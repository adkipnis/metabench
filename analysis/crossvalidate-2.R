# cross-validated IRT fitting of preprocessed data
# goal: determine the best IRT model for the given benchmark
# usage: Rscript crossvalidate2.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(
   names = c("BM", "MOD"),
   defaults = c("arc", "2PL"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     MOD = c("2PL", "3PL")
   )
)
here::i_am("analysis/crossvalidate-2.R")
mkdir("analysis/models")
set.seed(1)

# =============================================================================
# helper functions  

quick.eval <- function(df.test){
  df.test |>
    dplyr::mutate(error = score - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2)))
}

cross.validate <- function(itemtype){
  # fit model
  gprint("⚙️ Fitting {itemtype} model to training fold...")
  model <- mirt::mirt(data.train, 2, itemtype=itemtype, method = "MHRM")
  
  # train performance
  theta.train <- get.theta(model, method = "MAP")
  df.train <- data.frame(score = scores.train, theta.train)
  mod.score <- mgcv::gam(score ~ s(F1) + s(F2), data = df.train)
  df.train$p <- predict(mod.score)
  gprint("RMSE train: {round(quick.eval(df.train)$rmse, 3)}")
  
  # test performance
  theta.test <- get.theta(model, method = "MAP", resp = data.test)
  df.test <- data.frame(score = scores.test, theta.test)
  df.test$p <- predict(mod.score, newdata = df.test)
  gprint("RMSE test: {round(quick.eval(df.test)$rmse, 3)}")
  
  # collaps both dataframes
  df.train$set <- "train"
  df.test$set <- "test"
  list(df = rbind(df.train, df.test), model = model, mod.score = mod.score)
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
cv <- cross.validate(MOD)
outpath <- gpath("analysis/models/{BM}-{MOD}-cv-2.rds")
saveRDS(cv, outpath)
gprint("💾 Saved to '{outpath}'.")
