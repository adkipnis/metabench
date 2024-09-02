# Get the information per item and construct a reduced test set:
#   1. load data and fits
#   2. (remove itemfit outliers)
#   3. evaluate full score prediction (upper bound)
#   4. get item information
#   5. run hyperparameter search for subtest creation:
#      - select items based on test information
#      - fit subtest
#      - evaluate subtest parameter recovery
#      - evaluate subtest score prediction
#   6. final run with best hyperparameters
# usage: Rscript reduce.R {benchmark} {lambda} {number of theta partitions}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, run.mirt, get.theta])
box::use(./reduce.utils[...])
parse.args(
   names = c("BM", "LAMBDA", "N_QUANT", "seed"),
   defaults = c("arc", 0.005, 200, 2024),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     LAMBDA = seq(0, 1, 0.001), # penalty for subtest size (0 = no penalty)
     N_QUANT = seq(100, 500, 1)
   )
)
Saveplots <- T
here::i_am("analysis/reduce.R")
mkdir("analysis/reduced")
set.seed(as.numeric(seed))
skip.reduced <- T # load v2

# for Bayesian Optimization
N_INIT <- 15 # initial pass
N_ITER <- 60 # number of search iterations after initial pass
N_QUANT <- as.numeric(N_QUANT)
LAMBDA <- as.numeric(LAMBDA)
model.types <- c("2PL", "3PL", "4PL")
theta.types <- c("MAP", "EAPsum")
default.hyperparams <- list(model.type=1L, theta.type=1L, grid.type=1L, threshold=0.0)

# =============================================================================
# helper functions

merge.params <- function(items, model){
   mirt::coef(model, simplify=T, rotate="none")$items |>
      data.frame() |>
      tibble::rownames_to_column(var='item') |>
      dplyr::left_join(items, by="item")
}

collect.theta.map <- function(results, indices){
   theta.map <- results$df |> dplyr::filter(set == "train") |>
      dplyr::select(F1) |> as.matrix()
   theta.map.test <- results$df |> dplyr::filter(set == "test") |>
      dplyr::select(F1) |> as.matrix()
   colnames(theta.map) <- colnames(theta.map.test) <- "F1"
   theta.map.train <- theta.map[-indices, , drop=F]
   theta.map.val <- theta.map[indices, , drop=F]
   list(train = theta.map.train,
        val = theta.map.val,
        test = theta.map.val)
}

collect.theta.eapsum <- function(model, indices, data.train, data.test){
   theta <- get.theta(model, method="EAPsum", resp=data)
   theta.train <- theta[-indices, , drop=F]
   theta.val <- theta[indices, , drop=F]
   theta.test <- get.theta(model, method="EAPsum", resp=data.test)
   list(train = theta.train,
        val = theta.val,
        test = theta.test)
}

collect.all <- function(model.type){
   suffix <- ifelse(skip.reduced, "-v2", "")
   fitpath <- gpath("analysis/models/{BM}-{model.type}-1-cv{suffix}.rds")
   results <- readRDS(fitpath)
   model <- results$model
   thetas.map <- collect.theta.map(results, indices)
   tryCatch({
       thetas.eapsum <- collect.theta.eapsum(model, indices, data.train, data.test)
   }, error = function(e){
      print(e)
      thetas.eapsum <- NULL
   })
   items.full <- merge.params(items, model)
   list(model = model, MAP = thetas.map, EAPsum = thetas.eapsum, items = items.full)
}

# -----------------------------------------------------------------------------
# Score prediction
make.df.score <- function(scores, theta) {
   data.frame(score = scores, theta = theta[,1]) |>
      dplyr::mutate(rank.score = rank(score),
                    perc.score = rank.score / max(rank.score),
                    rank.theta = rank(theta),
                    perc.theta = rank.theta / max(rank.theta))
}

get.score.table <- function(theta.train, theta.test, scores.train, scores.test){
   df.train <- make.df.score(scores.train, theta.train)
   df.test <- make.df.score(scores.test, theta.test)
   mod.score <- mgcv::gam(score ~ s(theta, bs = "ad"), data = df.train)
   df.train$p <- predict(mod.score, df.train)
   df.test$p <- predict(mod.score, df.test)
   df.train$set <- "train"
   df.test$set <- "test"
   rbind(df.train, df.test) |>
      dplyr::mutate(error = score - p)
}

# -----------------------------------------------------------------------------
# Item info

collect.item.info <- function(model, theta, itemnames){
   box::use(mirt[extract.item, iteminfo])

   # create list of item info
   theta <- sort(theta)
   infos <- lapply(1:length(itemnames), function(i){
      iteminfo(extract.item(model, i), Theta=theta)
   })

   # combine into data frame
   info.items <- data.frame(theta=theta, do.call(cbind, infos))
   colnames(info.items) <- c("theta", itemnames)
   info.items
}

get.info.quantiles <- function(info.items, theta.grid, steps=40){
  theta.quantiles <- quantile(theta.grid, probs = 0:steps/steps, type=4)
  data.frame(quantile=theta.quantiles) |>
    tibble::rownames_to_column(var="percent") |>
    dplyr::mutate(index = findInterval(theta.quantiles, info.items$theta))
}

# -----------------------------------------------------------------------------
# Subtest creation

summarize.info <- function(info.items){
   theta <- info.items$theta
   info.items$theta <- NULL
   data.frame(item=colnames(info.items)) |>
      dplyr::mutate(
         info.argmax.index = sapply(info.items, which.max),
         info.argmax = theta[info.argmax.index],
         info.max = sapply(info.items, max),
         info.sd = sapply(info.items, sd)
      )
}

select.items <- function(items, info.items, info.quantiles, threshold=0.1){
  info.tmp <- info.items
  rownames(info.tmp) <- NULL
  index.list <- list()
  n <- nrow(info.quantiles)
  for (i in 1:n){
    q0 <- info.quantiles$quantile[i]
    q1 <- info.quantiles$quantile[i+1]
    # select rows in info.items that are in the current quantile
    info.sub <- info.tmp |>
      dplyr::filter(theta >= q0 & theta < q1) |> # select rows in the current quantile
      dplyr::select(-theta)
    maxinfo <- apply(info.sub, 2, max)
    maxinfo <- maxinfo[maxinfo >= threshold]
    maxinfo <- sort(maxinfo, decreasing = T)
    current.best <- maxinfo[1]
    if (length(maxinfo) > 0) {
        index <- which(items$item == names(current.best))
        index.list[[i]] <- items[index,]
        info.tmp[, names(current.best)] <- NULL
    }
  }
  do.call(rbind, index.list)
}

# -----------------------------------------------------------------------------
# Hyperparameter search

create.subtest <- function(data, items, info.items, info.quantiles, hyper) {
   index.set <- select.items(items, info.items, info.quantiles, threshold=hyper$threshold)
   data.sub <- data[, as.character(index.set$item)]
   list(data=data.sub, items=index.set)
}

hyperparam.wrapper <- function(hyperparams, internal=T){
   # 0. unpack
   model.type <- model.types[[hyperparams$model.type]]
   theta.type <- theta.types[[hyperparams$theta.type]]
   all <- models[[model.type]] 
   model <- all$model
   thetas <- all[[theta.type]]
   theta.train <- thetas$train
   theta.val <- thetas$val
   items <- all$items

   # fail gracefully if thetas are missing
   if (is.null(theta.train)){
     return(list(sfs = data.frame(rmse = 9999), items = items[1,]))
   }

   # 1. get theta grid
   if (hyperparams$grid.type == 1){
     theta.grid <- rbind(theta.train, theta.val)[,1]
   } else {
     theta.range <- range(rbind(theta.train, theta.val)[,1])
     theta.grid <- seq(theta.range[1], theta.range[2], length.out = N_QUANT)
   }

  # alternative: rank absolute errors and find the corresponding thetas
  # get 68th percentile of absolute errors by size
  # df.score.base$se <- df.score.base$error^2
  # critical.thetas <- df.score.base |> dplyr::arrange(se) |> head(50) |>
  #  dplyr::select(theta, se) |> dplyr::arrange(theta)

   info.items <- collect.item.info(model, theta.grid, colnames(data))
   items <- merge(items, summarize.info(info.items), by="item")
  
   # 2. create subtest
   info.quantiles <- get.info.quantiles(info.items, theta.grid, steps=N_QUANT)
   subtest <- create.subtest(data.train, items, info.items, info.quantiles,
                             hyperparams)
   data.train.sub <- subtest$data
   data.val.sub <- data.val[colnames(data.train.sub)]
   items.sub <- subtest$items
   
   # fail gracefully if the set becomes too small
   if (ncol(data.frame(data.train.sub)) < 10){
     return(list(sfs = data.frame(rmse = 9999), items = items[1,]))
   }
   
   # 3. fit subtest
   ncycles <- ifelse(internal, 500, 1000)
   model.sub <- run.mirt(data.train.sub, 1, model.type,
                         tol=1e-4, ncycles=ncycles)
   theta.train.sub <- get.theta(model.sub, theta.type, data.train.sub)
   theta.val.sub <- get.theta(model.sub, theta.type, data.val.sub)
   rownames(theta.train.sub) <- rownames(data.train.sub)
   rownames(theta.val.sub) <- rownames(data.val.sub)

   # 4. evaluate subtest
   score.table.sub <- get.score.table(
     theta.train.sub, theta.val.sub, scores.train, scores.val)
   sfs.sub <- score.stats(score.table.sub)
     
   # 5. return results
   list(items = items.sub,
        model = model.sub,
        theta.train = theta.train.sub,
        theta.val = theta.val.sub,
        info.items = info.items,
        info.quantiles = info.quantiles,
        df.score = score.table.sub,
        sfs = sfs.sub)
}

# grid.search <- function(){
#   results <- list()
#   i <- 1
#   for (gridtype in c(1,2)){
#     for (threshold in seq(0, 0.3, 0.05)){
#       hyperparams <- list(threshold=threshold, gridtype=gridtype)
#       res <- hyperparam.wrapper(hyperparams, internal=T)
#       n.items <- nrow(res$items)
#       score <- res$sfs$rmse + as.numeric(LAMBDA) * n.items
#       results[[i]] <- data.frame(n = n.items, RMSE = res$sfs$rmse, score = score,
#                            gridtype = gridtype, threshold = threshold)
#       gprint("n = {n.items}, RMSE = {round(res$sfs$rmse, 2)}, score = {round(score,2)}, threshold = {round(threshold,4)}, gridtype = {gridtype}")
#       i <- i + 1
#     }
#   }
#   do.call(rbind, results)
# }

optimize.hyperparameters <- function(){
   box::use(rBayesianOptimization[...])
   objective <- function(model.type, theta.type, grid.type, threshold) {
     hyperparams <- list(model.type=model.type, theta.type=theta.type,
                         grid.type=grid.type, threshold=threshold)
     res <- hyperparam.wrapper(hyperparams, internal=T)
     sfs <- res$sfs
     score <- sfs$rmse + as.numeric(LAMBDA) * nrow(res$items) # minimize this
     list(Score = -score, Pred = 0)
  }
  BayesianOptimization(
   objective,
   bounds = list(
     model.type = c(1L, 3L),
     theta.type = c(1L, 2L),
     grid.type = c(1L, 2L),
     threshold = c(0, 5)
     ),
   init_points = N_INIT,
   n_iter = N_ITER,
   acq = "ucb", 
   kappa = 2.576,
   eps = 0,
   verbose = T)
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
suffix <- ifelse(skip.reduced, glue::glue("-{seed}-v2"), "")
datapath <- gpath("data/{BM}-sub-350{suffix}.rds")
full <- readRDS(datapath)
items <- full$items
items$item <- as.character(items$item)
data <- full$data.train
scores <- full$scores.train / full$max.points.orig * 100
indices <- caret::createDataPartition(scores, p = 0.1, list = F)
data.train <- data[-indices,]
data.val <- data[indices,]
data.test <- full$data.test
scores.train <- scores[-indices]
scores.val <- scores[indices]
scores.test <- full$scores.test / full$max.points.orig * 100
rm(full)

# prepare model and thetas
gprint("🚰 Loading {BM} fits...")
models <- setNames(lapply(model.types, collect.all), model.types)

# run hyperparameter search using rBayesianOptimization
if (LAMBDA == 0){
   gprint("Skipping hyperparameter search (LAMBDA = 0)")
   hyperparams <- default.hyperparams
   opt.results <- NULL
} else {
   gprint("🔍 Running hyperparameter search...")
   opt.results <- optimize.hyperparameters()
   hyperparams <- as.list(opt.results$Best_Par)
  #opt.results <- grid.search()
  #mindex <- which.min(opt.results$score)
  #hyperparams <- as.list(opt.results[mindex,])
}
final <- hyperparam.wrapper(hyperparams, internal = F)

# unpack results
model.type <- model.types[[hyperparams$model.type]]
theta.type <- theta.types[[hyperparams$theta.type]]
model <- models[[model.type]]$model
theta.train <- models[[model.type]][[theta.type]]$train
theta.val <- models[[model.type]][[theta.type]]$val
items <- models[[model.type]]$items
info.items <- final$info.items

# summarize base score
df.score.base <- get.score.table(theta.train, theta.val,
                                 scores.train, scores.val)
sfs.base <- score.stats(df.score.base)

# evaluation on test set
data.test.sub <- data.test[,as.character(final$items$item)]
theta.test.sub <- get.theta(final$model, method=theta.type, resp=data.test.sub)
rownames(theta.test.sub) <- rownames(data.test.sub)
df.score.test <- get.score.table(final$theta.train, theta.test.sub,
                                 scores.train, scores.test)
sfs.test <- score.stats(df.score.test)

# save results
out <- list(
   items = merge.params(final$items, final$model),
   model = final$model,
   theta.train = final$theta.train,
   theta.val = final$theta.val,
   theta.test = theta.test.sub,
   info.items.orig = info.items,
   hyperparams = hyperparams,
   opt.results = opt.results,
   sfs.base = sfs.base,
   sfs.sub = sfs.test,
   df.score.base = df.score.base,
   df.score.sub = df.score.test
)

gprint("🎉 Reduced test to {nrow(final$items)} items (using a penalty coefficient of {LAMBDA}).
       RMSE = {round(sfs.test$rmse, 3)}")
version <- ifelse(skip.reduced, "-v2", "")
outpath <- gpath("analysis/reduced/{BM}-{model.type}-{theta.type}-{LAMBDA}{version}.rds")
saveRDS(out, outpath)
gprint("💾 Saved results to {outpath}")

# =============================================================================
# misc plots
title <- cowplot::ggdraw() +
  cowplot::draw_label(
    glue::glue("{BM} ({model.type} {theta.type}, lambda={LAMBDA})"),
    fontface = 'bold', size = 24) +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 7),
                 plot.background = ggplot2::element_rect(
                   fill="white", color="white"))

# 1. test info and parameter recovery
ceiling <- max(rowSums(info.items[,-1]))
p.testinfo <- cowplot::plot_grid(
  plot.expected.testinfo(info.items, items, ceiling,
                         glue::glue("Full Testinfo (n = {nrow(items)})")),
  plot.expected.testinfo(info.items, final$items, ceiling,
                         glue::glue("Expected Testinfo (n = {nrow(final$items)})")),
  plot.testinfo(final$model, final$theta.train[,1,drop=F], ceiling,
                glue::glue("Reduced Testinfo (n = {nrow(final$items)})")),
  nrow = 1
)
p.estimates <- plot.estimates(model, final$model, theta.train, final$theta.train)
p.misc <- cowplot::plot_grid(
  p.testinfo,
  p.estimates,
  ncol = 1
)
p.misc <- cowplot::plot_grid(title, p.misc, ncol = 1, rel_heights = c(0.05, 1))
plotpath <- gpath("analysis/reduced/{BM}-{model.type}-{theta.type}-{LAMBDA}-info{version}.png")
ggplot2::ggsave(plotpath, p.misc, width = 16, height = 16)


# 2. score prediction plots
p.ts <- cowplot::plot_grid(
  plot.theta.score(df.score.base, "\n(Validation - Full)"),
  plot.theta.score(final$df.score, "\n(Validation - Subset)"),
  plot.theta.score(df.score.test, "\n(Test - Subset)"),
  nrow = 1
)
p.perc <- cowplot::plot_grid(
  plot.perc(df.score.base, "\n(Validation - Full)"),
  plot.perc(final$df.score, "\n(Validation - Subset)"),
  plot.perc(df.score.test, "\n(Test - Subset)"),
  nrow = 1
)
p.score <- cowplot::plot_grid(
  plot.score(df.score.base, "\n(Validation - Full)"),
  plot.score(final$df.score, "\n(Validation - Subset)"),
  plot.score(df.score.test, "\n(Test - Subset)"),
  nrow = 1
)
ceiling <- max(abs(df.score.test$error))
p.error <- cowplot::plot_grid(
  plot.score.error(df.score.base, "\n(Validation - Full)", ceiling),
  plot.score.error(final$df.score, "\n(Validation - Subset)", ceiling),
  plot.score.error(df.score.test, "\n(Test - Subset)", ceiling),
  nrow = 1
) 
p.pred <- cowplot::plot_grid(
  p.ts, p.perc, p.score, p.error, ncol = 1
)
p.pred <- cowplot::plot_grid(title, p.pred, ncol = 1, rel_heights = c(0.05, 1))
plotpath <- gpath("analysis/reduced/{BM}-{model.type}-{theta.type}-{LAMBDA}-pred{version}.png")
ggplot2::ggsave(plotpath, p.pred, width = 16, height = 19)
