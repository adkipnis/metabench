# evalute item selection via clustering
box::use(./utils[parse.args, mkdir, gprint, gpath, napply, rowmerge, run.mirt, get.theta])
here::i_am("analysis/evaluate.clust.R")
seed = 1

benchmarks <- list(
 arc=145,
 gsm8k=237,
 hellaswag=93,
 mmlu=96,
 truthfulqa=154,
 winogrande=133
)

# =============================================================================
# helper functions
load.data <- function(BM){
   datapath <- gpath("data/{BM}-sub-350-seed={seed}.rds")
   full <- readRDS(datapath)
   data <- full$data.train
   scores <- full$scores.train / full$max.points.orig * 100
   indices <- caret::createDataPartition(scores, p = 0.1, list = F)
   data.train <- data[-indices,]
   data.test <- full$data.test
   scores.train <- scores[-indices]
   scores.test <- full$scores.test / full$max.points.orig * 100
   list(data.train=data.train, data.test=data.test,
        scores.train=scores.train, scores.test=scores.test)
}

load.results <- function(BM){
   d <- benchmarks[[BM]]
   path <- gpath("analysis/clustering/{BM}-{d}-seed={seed}.rds")
   readRDS(path)
}

get.scores <- function(bm, type){
   if (type == "train"){
      data <- data.list[[bm]]$data.train
      scores <- data.list[[bm]]$scores.train
   } else {
      data <- data.list[[bm]]$data.test
      scores <- data.list[[bm]]$scores.test
   }
   scores <- data.frame(scores)
   colnames(scores) <- bm
   rownames(scores) <- rownames(data)
   list(scores = scores, data = data)
}

get.grand <- function(type){
   out <- list()
   for (bm in names(benchmarks)){
      tmp <- get.scores(bm, type)
      out[[bm]] <- tmp$scores
   }
   scores <- Reduce(rowmerge, out)
   data.frame(grand = rowSums(scores) / ncol(scores))
}

prepare.lm.data <- function(bm, type){
   tmp <- get.scores(bm, type)
   scores <- tmp$scores
   data <- tmp$data
   items <- results.list[[bm]]$best$indices
   data.sub <- data[,colnames(data) %in% items]
   colnames(scores) <- "grand"
   rowmerge(scores, data.sub)
}

train.lm <- function(bm){
  gprint("🚀 Training linear model for {bm}...")
  data.train <- prepare.lm.data(bm, "train")
  data.test <- prepare.lm.data(bm, "test")
  nc <- ncol(data.train)
  sub.train <- rowSums(data.train[,-1]) / nc * 100
  sub.test <- rowSums(data.test[,-1]) / nc * 100
  mod.lin <- lm(grand ~ ., data = data.train)
  data.train$p <- predict(mod.lin, data.train)
  data.test$p <- predict(mod.lin, data.test)
  rmse.train <- data.train |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
  rmse.test <- data.test |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
  pred.train <- data.frame(data.train$p)
  pred.test <- data.frame(data.test$p)
  rownames(pred.train) <- rownames(data.train)
  rownames(pred.test) <- rownames(data.test)
  list(rmse.train = rmse.train,
       rmse.test = rmse.test,
       pred.train = pred.train, 
       pred.test = pred.test,
       sub.train = sub.train,
       sub.test = sub.test)
}

setup.df <- function(bm, type){
   if (type == "train"){
      theta <- results.list[[bm]]$theta.train[,1]
      lin.pred <- lm.list[[bm]]$pred.train
   } else {
      theta <- results.list[[bm]]$theta.test[,1]
      lin.pred <- lm.list[[bm]]$pred.test
   }
   theta <- data.frame(theta)
   lin.pred <- data.frame(lin.pred)
   colnames(theta) <- paste0(bm)
   colnames(lin.pred) <- paste0(bm, ".l")
   rowmerge(theta, lin.pred)
}

rmse <- function(df){
   df |> dplyr::mutate(error = grand - p) |>
     dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
}

fit.full <- function(){
  gprint("🚀 Fitting GAM for full model...")
  mod.sub <- mgcv::gam(grand ~
                         s(grand.l, bs="ad") +
                         s(arc, bs="ad") +
                         s(gsm8k, bs="ad") +
                         s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") +
                         s(truthfulqa, bs="ad") +
                         s(winogrande, bs="ad"),
                       data = df.train)
  df.train$p <- predict(mod.sub, df.train)
  df.test$p <- predict(mod.sub, df.test)
  rmse.train <- rmse(df.train)
  rmse.test <- rmse(df.test)
  gprint("Full Model 📊 RMSE train: {rmse.train}, RMSE test: {rmse.test}")
  list(rmse.train = rmse.train,
       rmse.test = rmse.test,
       gam = mod.sub)
}

fit.specific <- function(bm){
  scores.train <- data.frame(get.scores(bm, "train")$scores)
  scores.test <- data.frame(get.scores(bm, "test")$scores)
  colnames(scores.train) <- colnames(scores.test) <- "grand"
  df.train$grand <- NULL
  df.test$grand <- NULL
  df.train <- rowmerge(df.train, scores.train)
  df.test <- rowmerge(df.test, scores.test)
  this.l.str <- paste0(bm, ".l")
  df.train$this.l <- df.train[[this.l.str]]
  df.test$this.l <- df.test[[this.l.str]]
  gprint("🚀 Fitting GAM for {bm}...")
  mod.sub <- mgcv::gam(grand ~
                         s(grand.l, bs="ad") +
                         s(this.l, bs="ad") +
                         s(arc, bs="ad") +
                         s(gsm8k, bs="ad") +
                         s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") +
                         s(truthfulqa, bs="ad") +
                         s(winogrande, bs="ad"),
                       data = df.train)
  df.train$p <- predict(mod.sub, df.train)
  df.test$p <- predict(mod.sub, df.test)
  rmse.train <- rmse(df.train)
  rmse.test <- rmse(df.test)
  gprint("{bm} 📊 RMSE train: {rmse.train}, RMSE test: {rmse.test}")
  list(rmse.train = rmse.train,
       rmse.test = rmse.test,
       gam = mod.sub)
}
# =============================================================================
# prepare data
data.list <- napply(names(benchmarks), load.data)

# load results
results.list <- napply(names(benchmarks), load.results)

# fit linear models
lm.list <- napply(names(benchmarks), train.lm)

# setup predictors
df.train.list <- napply(names(benchmarks), function(bm) setup.df(bm, "train"))
df.test.list <- napply(names(benchmarks), function(bm) setup.df(bm, "test"))
df.train <- Reduce(rowmerge, df.train.list)
df.test <- Reduce(rowmerge, df.test.list)
df.train <- rowmerge(df.train, get.grand("train"))
df.test <- rowmerge(df.test, get.grand("test"))
df.train$grand.l <- (df.train$arc.l + df.train$gsm8k.l +
                     df.train$hellaswag.l + df.train$mmlu.l) / 4
df.test$grand.l <- (df.test$arc.l + df.test$gsm8k.l +
                    df.test$hellaswag.l + df.test$mmlu.l) / 4

# fit generalized additive models
full <- fit.full()
arc <- fit.specific("arc")
gsm8k <- fit.specific("gsm8k")
hellaswag <- fit.specific("hellaswag")
mmlu <- fit.specific("mmlu")
truthfulqa <- fit.specific("truthfulqa")
winogrande <- fit.specific("winogrande")

# save results
outpath <- gpath("analysis/clustering/summary.rds")
out <- list(
  full = full,
  arc = arc,
  gsm8k = gsm8k,
  hellaswag = hellaswag,
  mmlu = mmlu,
  truthfulqa = truthfulqa,
  winogrande = winogrande
)
saveRDS(out, outpath)
