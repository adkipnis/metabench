# cross-validated IRT fitting of preprocessed data
# goal: determine the best IRT model for the given benchmark
# usage: Rscript crossvalidate.R {benchmark} {model} {dimension}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, get.theta, run.mirt])

parse.args(
   names = c("BM", "MOD", "D", "seed"),
   defaults = c("arc", "4PL", 1, 2024),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     MOD = c("2PL", "3PL", "4PL"),
     D = c(1, 2)
   )
)
here::i_am("analysis/crossvalidate.R")
mkdir("analysis/models")
set.seed(as.numeric(seed))

skip.reduced <- T # load v2

# =============================================================================
# helper functions  

quick.eval <- function(df.test){
  df.test |>
    dplyr::mutate(error = score - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2)))
}

fit.gam <- function(df.train){
   # get columns that start with F
   if ("F2" %in% colnames(df.train)){
      formula <- "score ~ s(F1, bs = 'ad') + s(F2, bs = 'ad')"
   } else {
      formula <- "score ~ s(F1, bs = 'ad')"
   }
   mgcv::gam(as.formula(formula), data = df.train)
}

cross.validate <- function(){
  # fit model
  gprint("⚙️ Fitting {MOD} model with {D}-dimensional ability to training fold...")
  gprint("RMSE raw: {round(preproc$rmse.test,3)}")
  model <- run.mirt(data.train, as.numeric(D), MOD)

  # train performance
  theta.train <- get.theta(model, method = "MAP")
  df.train <- data.frame(score = scores.train, theta.train)
  mod.score <- fit.gam(df.train)
  df.train$p <- predict(mod.score)
  gprint("RMSE train: {round(quick.eval(df.train)$rmse, 3)}")
  
  # test performance
  theta.test <- get.theta(model, method = "MAP", resp = data.test)
  # remove any columns that start with "SE_"
  theta.test <- theta.test[, !grepl("^SE_", colnames(theta.test)),drop=F]
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
suffix <- ifelse(skip.reduced, glue::glue("-{seed}-v2"), "")
datapath <- gpath("data/{BM}-sub-350{suffix}.rds")
preproc <- readRDS(datapath)
data.train <- preproc$data.train
data.test <- preproc$data.test
nc <- preproc$max.points.orig
scores.train <- preproc$scores.train / nc * 100
scores.test <- preproc$scores.test / nc * 100

# =============================================================================
# cv models
cv <- cross.validate()
suffix <- ifelse(skip.reduced, "-v2", "")
outpath <- gpath("analysis/models/{BM}-{MOD}-{D}-cv{suffix}.rds")
saveRDS(cv, outpath)
gprint("💾 Saved to '{outpath}'.")
