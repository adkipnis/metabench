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
# usage: Rscript reduce.R {benchmark} {number of theta partitions} {lambda}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, run.mirt, get.theta])
box::use(./reduce.utils[...])
parse.args(
   names = c("BM", "N_QUANT", "LAMBDA"),
   defaults = c("winogrande", 100, 0.0),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     N_QUANT = seq(100, 500, 1),
    LAMBDA = seq(0, 1, 0.001) # penalty for subtest size (0 = no penalty)
   )
)
Saveplots <- T
here::i_am("analysis/reduce.R")
mkdir("analysis/reduced")
set.seed(1)
# for Bayesian Optimization
N_ITER <- 15
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
   mod.score <- mgcv::gam(score ~ s(theta), data = df.train)
   df.train$p <- predict(mod.score, df.train)
   df.test$p <- predict(mod.score, df.test)
   df.train$set <- "train"
   df.test$set <- "test"
   rbind(df.train, df.test) |>
      dplyr::mutate(error = score - p)
}

score.stats <- function(df.score){
  df.score |>
    dplyr::filter(set == "test") |>
    dplyr::summarise(
      mae = mean(abs(error)),
      ub = mean(abs(error)) + 1.96 * sd(abs(error)),
      rmse = sqrt(mean(error^2)),
      sse = sum(error^2),
      r = cor(theta, score, method = "spearman")
    )
}

compare.score.stats <- function(sfs, sfs.sub){
   out <- list()
   for (key in names(sfs)) {
      out[[key]] <- sfs.sub[[key]] - sfs[[key]]
   }
   gprint("📊 Score error change (subtest - full, negative means improvement):
          Δ RMSE: {round(out$rmse, 3)}
          Δ MAE: {round(out$mae, 3)}
          Δ (MAE + 1.96 SD): {round(out$ub, 3)}
          Δ Total SSE: {round(out$sse, 3)}")
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
# parameter recovery

compare.parameters <- function(model, model.sub){
   get.estimates <- function(model){
      mirt::coef(model, simplify=T, rotate="none")$items |>
         data.frame() |>
         tibble::rownames_to_column(var='item')
   }
   estimates <- get.estimates(model)
   estimates.sub <- get.estimates(model.sub)
   df.comparison <- merge(estimates, estimates.sub, by='item')
   r1 <- cor(df.comparison$d.x, df.comparison$d.y)
   r2 <- cor(df.comparison$a1.x, df.comparison$a1.y)
   gprint("Correlation difficulty: {round(r1, 2)}")
   gprint("Correlation loading: {round(r2, 2)}")
   df.comparison
}

# -----------------------------------------------------------------------------
# hyperparameter search

create.subtest <- function(data, items, info.items, info.quantiles, hyper) {
   index.set <- select.items(items, info.items, info.quantiles, threshold=hyper$threshold)
   data.sub <- data[, as.character(index.set$item)]
   list(data=data.sub, items=index.set)
}

hyperparam.wrapper <- function(hyperparams, internal=T){
   # 0. prepare
   if (hyperparams$gridtype == 1){
     theta.grid <- theta[,1]
   } else {
     theta.range <- range(theta[,1])
     theta.grid <- seq(theta.range[1], theta.range[2], length.out = N_QUANT)
   }
   info.items <- collect.item.info(model, theta.grid, colnames(data))
   items <- merge(items, summarize.info(info.items), by="item")
  
   # 1. create subtest
   info.quantiles <- get.info.quantiles(info.items, theta.grid, steps=N_QUANT)
   subtest <- create.subtest(data.train, items, info.items, info.quantiles, hyperparams)
   data.train.sub <- subtest$data
   data.test.sub <- data.test[colnames(data.train.sub)]
   items.sub <- subtest$items
   
   # fail gracefully if the set becomes too
   if (ncol(data.frame(data.train.sub)) < 10){
     return(list(sfs = data.frame(rmse = 9999), items = items[1,]))
   }
   
   # 2. fit subtest
   ncycles <- ifelse(internal, 500, 1000)
   model.sub <- run.mirt(data.train.sub, 1, MOD, tol = 1e-4, ncycles=ncycles)
   theta.train.sub <- get.theta(model.sub, method=METH, resp=data.train.sub)
   rownames(theta.train.sub) <- rownames(data.train.sub)
   theta.test.sub <- get.theta(model.sub, method=METH, resp=data.test.sub)
   rownames(theta.test.sub) <- rownames(data.test.sub)

   # 3. evaluate subtest
   score.table.sub <- get.score.table(theta.train.sub, theta.test.sub, scores.train, scores.test)
   sfs.sub <- score.stats(score.table.sub)
     
   # 4. return results
   list(items = items.sub,
        model = model.sub,
        theta.train = theta.train.sub,
        theta.test = theta.test.sub,
        info.items = info.items,
        info.quantiles = info.quantiles,
        df.score = score.table.sub,
        sfs = sfs.sub)
}

grid.search <- function(){
  results <- list()
  i <- 1
  for (gridtype in c(1,2)){
    for (threshold in seq(0, 0.3, 0.05)){
      hyperparams <- list(threshold=threshold, gridtype=gridtype)
      res <- hyperparam.wrapper(hyperparams, internal=T)
      n.items <- nrow(res$items)
      score <- res$sfs$rmse + as.numeric(LAMBDA) * n.items
      results[[i]] <- data.frame(n = n.items, RMSE = res$sfs$rmse, score = score,
                           gridtype = gridtype, threshold = threshold)
      gprint("n = {n.items}, RMSE = {round(res$sfs$rmse, 2)}, score = {round(score,2)}, threshold = {round(threshold,4)}, gridtype = {gridtype}")
      i <- i + 1
    }
  }
  do.call(rbind, results)
}

optimize.hyperparameters <- function(){
   box::use(rBayesianOptimization[...])
   objective <- function(threshold, gridtype) {
     hyperparams <- list(threshold=threshold, gridtype=gridtype)
     res <- hyperparam.wrapper(hyperparams, internal=T)
     sfs <- res$sfs
     score <- sfs$rmse + as.numeric(LAMBDA) * nrow(res$items) # minimize this
     list(Score = -score, Pred = 0)
  }
  BayesianOptimization(
   objective,
   bounds = list(threshold = c(0, 5), gridtype = c(1L, 2L)),
   init_points = 5,
   n_iter = N_ITER,
   acq = "ucb", 
   kappa = 2.576,
   eps = 0,
   verbose = T)
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
datapath <- gpath("data/{BM}-sub-350.rds")
full <- readRDS(datapath)
items <- full$items
items$item <- as.character(items$item)
data <- full$data.train
scores <- full$scores.train / full$max.points.orig * 100
indices <- caret::createDataPartition(scores, p = 0.1, list = F)
data.train <- data[-indices,]
data.test <- data[indices,]
data.val <- full$data.test
scores.train <- scores[-indices]
scores.test <- scores[indices]
scores.val <- full$scores.test / full$max.points.orig * 100
rm(full)

# prepare model and thetas
gprint("🚰 Loading {BM} fits...")
fitpath <- gpath("analysis/models/{BM}-{MOD}-{1}-cv.rds")
results <- readRDS(fitpath)
model <- results$model
items <- merge.params(items, model)
if (METH == "MAP") {
   theta <- results$df |> dplyr::filter(set == "train") |>
     dplyr::select(F1) |> as.matrix()
   colnames(theta) <- "F1"
   theta.train <- theta[-indices, , drop=F]
   theta.test <- theta[indices, , drop = F]
   theta.val <- results$df |> dplyr::filter(set == "test") |>
     dplyr::select(F1) |> as.matrix()
   colnames(theta.val) <- "F1"
} else {
   theta <- get.theta(model, method=METH)
   # split theta but keep column dims
   theta.train <- theta[-indices, , drop=F]
   theta.test <- theta[indices, , drop=F]
   theta.val <- get.theta(model, method=METH, resp=data.val)
}
rm(results)

# summarize score
df.score.base <- get.score.table(theta.train, theta.val, scores.train, scores.val)
sfs.base <- score.stats(df.score.base)

# rank absolute errors and find the corresponding thetas
# get 68th percentile of absolute errors by size
# df.score.base$se <- df.score.base$error^2
# critical.thetas <- df.score.base |> dplyr::arrange(se) |> head(50) |>
#  dplyr::select(theta, se) |> dplyr::arrange(theta)


# run hyperparameter search using rBayesianOptimization
if (LAMBDA == 0){
   gprint("Skipping hyperparameter search (LAMBDA = 0)")
   hyperparams <- default.hyperparams
   opt.results <- hyperparams
} else {
   gprint("🔍 Running hyperparameter search...")
   opt.results <- optimize.hyperparameters()
   hyperparams <- as.list(opt.results$Best_Par)
  #opt.results <- grid.search()
  #mindex <- which.min(opt.results$score)
  #hyperparams <- as.list(opt.results[mindex,])
}
final <- hyperparam.wrapper(hyperparams, internal = F)
info.items <- final$info.items

# evaluation on validation set
data.val.sub <- data.val[,as.character(final$items$item)]
theta.val.sub <- get.theta(final$model, method=METH, resp=data.val.sub)
rownames(theta.val.sub) <- rownames(data.val.sub)
df.score.val <- get.score.table(final$theta.train, theta.val.sub, scores.train, scores.val)
sfs.val <- score.stats(df.score.val)

# misc plots
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
# TODO: scatterplot of theta with marginal distributions
p.estimates <- plot.estimates(final$model, final$theta.train)
p.misc <- cowplot::plot_grid(
  p.testinfo,
  p.estimates,
  ncol = 1
)
plotpath <- gpath("analysis/reduced/{BM}-{MOD}-{METH}-{LAMBDA}-info.png")
ggplot2::ggsave(plotpath, p.misc, width = 16, height = 16)

# prediction plots
p.ts <- cowplot::plot_grid(
  plot.theta.score(df.score.base, "(Full)"),
  plot.theta.score(final$df.score, "(Subset)"),
  plot.theta.score(df.score.val, "(Validation)"),
  nrow = 1
)
p.perc <- cowplot::plot_grid(
  plot.perc(df.score.base, "(Full)"),
  plot.perc(final$df.score, "(Subset)"),
  plot.perc(df.score.val, "(Validation)"),
  nrow = 1
)
p.score <- cowplot::plot_grid(
  plot.score(df.score.base, "(Full)"),
  plot.score(final$df.score, "(Subset)"),
  plot.score(df.score.val, "(Validation)"),
  nrow = 1
)
ceiling <- max(abs(df.score.val$error))
p.error <- cowplot::plot_grid(
  plot.score.error(df.score.base, "(Full)", ceiling),
  plot.score.error(final$df.score, "(Subset)", ceiling),
  plot.score.error(df.score.val, "(Validation)", ceiling),
  nrow = 1
) 
p.pred <- cowplot::plot_grid(
  p.ts, p.perc, p.score, p.error, ncol = 1
)
plotpath <- gpath("analysis/reduced/{BM}-{MOD}-{METH}-{LAMBDA}-pred.png")
ggplot2::ggsave(plotpath, p.pred, width = 16, height = 16)


# save results
out <- list(
   items = merge.params(final$items, final$model),
   model = final$model,
   theta.train = final$theta.train,
   theta.test = final$theta.test,
   theta.val = theta.val.sub,
   info.items.orig = info.items,
   hyperparams = hyperparams,
   opt.results = opt.results,
   sfs.full = sfs.base,
   sfs.sub = sfs.val,
   df.score.base = df.score.base,
   df.score.val = df.score.val
)

#compare.score.stats(sfs.base, sfs.val)
gprint("🎉 Reduced test to {nrow(final$items)} items (using a penalty coefficient of {LAMBDA}).
       RMSE = {round(sfs.val$rmse, 2)}")
outpath <- gpath("analysis/reduced/{BM}-{MOD}-{METH}-{LAMBDA}.rds")
saveRDS(out, outpath)
gprint("💾 Saved results to {outpath}")
p.pred
