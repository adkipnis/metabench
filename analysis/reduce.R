# Get the information per item and construct a reduced test set:
#   1. load data and fits
#   2. remove itemfit outliers
#   3. evaluate full score prediction (upper bound)
#   4. get item information
#   5. run hyperparameter search for subtest creation:
#      - select items based on test information
#      - fit subtest
#      - evaluate subtest parameter recovery
#      - evaluate subtest score prediction
#   6. final run with best hyperparameters
# usage: Rscript reduce.R {benchmark} {model} {method} {lambda}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, mytheme, run.mirt, get.theta])
parse.args(
   names = c("BM", "MOD", "METH", "LAMBDA"),
   defaults = c("hellaswag", "3PL", "MAP", 0.0),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     MOD = c("2PL", "3PL", "3PLu", "4PL"),
     METH = c("MAP", "EAPsum"), # for theta estimation
     LAMBDA = seq(0, 1, 0.1) # penalty for subtest size (0 = no penalty)
   )
)
Saveplots <- T
here::i_am("analysis/reduce.R")
mkdir("plots")
mkdir("analysis/reduced")
set.seed(1)
N_ITER <- 15 # for Bayesian Optimization

# =============================================================================
# helper functions

merge.params <- function(items, model){
   mirt::coef(model, simplify=T, rotate="none")$items |>
      data.frame() |>
      tibble::rownames_to_column(var='item') |>
      dplyr::mutate(item = as.numeric(item)) |>
      dplyr::left_join(items, by="item")
}

printorsave <- function(p, outsuffix){
   if (!is.null(outsuffix)) {
      outpath <- gpath("plots/{BM}-{MOD}-{METH}-{outsuffix}.png")
      ggplot2::ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Saved plot to {outpath}")
   } else {
      print(p)
   }
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

plot.theta.score <- function(df.score, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |> dplyr::filter(set == "test")
   sfs <- score.stats(df.plot)
   text <- glue::glue(
     "RMSE = {round(sfs$rmse, 3)}\nMAE = {round(sfs$mae, 3)}\nr = {round(sfs$r, 3)}")
   x.label <- 0.8 * diff(range(df.plot$theta)) + min(df.plot$theta)
   y.label <- 0.1 * diff(range(df.plot$score)) + min(df.plot$score)
   ggplot(df.plot, aes(x = theta, y = score)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = p), color = "red") +
      ylim(0,100) +
      annotate("text", x = x.label, y = y.label, label = text, size = 3) +
      labs(
         title = glue::glue("Theta vs. Score {suffix}"),
         x = TeX("$\\theta$"),
         y = "Score",
      ) +
      mytheme()
}

plot.perc <- function(df.score, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
     dplyr::filter(set == "test")
   ggplot(df.plot, aes(x = 100 * perc.theta, y = 100 * perc.score)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0,
                  slope = 1,
                  linetype = "dashed") +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Percentile Comparison {suffix}"),
            x = TeX("$\\% \\theta$"),
            y = "% Score",
            ) +
         mytheme()
}

plot.score <- function(df.score, suffix = ""){
   box::use(ggplot2[...])
   df.plot <- df.score |>
      dplyr::filter(set == "test")
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Score Reconstruction {suffix}"),
            x = "Score",
            y = "Predicted",
            ) +
         mytheme()
}

plot.score.error <- function(df.score, suffix = "", ylim = NULL){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
      dplyr::filter(set == "test")
   ymax <- ifelse(is.null(ylim), max(abs(df.plot$error)), ylim)
   ggplot(df.plot, aes(x = p, y = error)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 0,
                     linetype = "dashed") +
         coord_cartesian(xlim = c(0, 100), ylim = c(-ymax, ymax)) +
         labs(
            title = glue::glue("Predicted vs. Error {suffix}"),
            x = "Predicted Score",
            y = "Error",
            ) +
         mytheme()
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
   theta <- sort(theta[,1])
   infos <- lapply(1:length(itemnames), function(i){
      iteminfo(extract.item(model, i), Theta=theta)
   })

   # combine into data frame
   info.items <- data.frame(theta=theta, do.call(cbind, infos))
   colnames(info.items) <- c("theta", itemnames)
   info.items
}

plot.theta <- function(theta, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   as.data.frame(theta) |> 
      ggplot(aes(x = F1)) +
         geom_density(color="black") +
         labs(
            title = glue::glue("Theta Distribution ({suffix})"),
            x = TeX("$\\theta$"),
            y = TeX("$f(\\theta)$"),
            ) +
         mytheme()
}

plot.testinfo <- function(model, theta, ylim=NULL) {
   box::use(ggplot2[...], latex2exp[TeX])
   info.test <- mirt::testinfo(model, Theta = theta)
   ymax <- ifelse(is.null(ylim), max(info.test), ylim)
   data.frame(theta=theta, info=info.test) |>
      ggplot(aes(x = F1, y = info)) +
      # increase linewidth
         geom_line(linewidth = 1) +
         ylim(0, ymax) +
         labs(
            title = "Sub Testinfo",
            x = TeX("$\\theta$"),
            y = TeX("$I(\\theta)$"),
            ) +
         mytheme()
}

plot.expected.testinfo <- function(info.items, index.set, ylim=NULL, title="Expected Testinfo"){
   box::use(ggplot2[...], latex2exp[TeX])
   quantiles <- info.items$theta
   info.sub <- info.items[, as.character(index.set$item)]
   info.sub$cum <- rowSums(info.sub)
   info.sub$theta <- quantiles
   ymax <- ifelse(is.null(ylim), max(info.sub$cum), ylim)
   info.sub |>
      ggplot(aes(x = theta, y = cum)) +
         geom_line() +
         ylim(0, ymax) +
         labs(
            title = title,
            x = TeX("$\\theta$"),
            y = TeX("$I(\\theta)$"),
            ) +
         mytheme()
}

get.info.quantiles <- function(info.items, steps=40){
  theta.quantiles <- quantile(info.items$theta, probs = 0:steps/steps, type=4)
  data.frame(quantile=theta.quantiles) |>
    tibble::rownames_to_column(var="percent") |>
    dplyr::mutate(index = findInterval(theta.quantiles, info.items$theta))
}

plot.quantiles <- function(info.quantiles, theta) {
   box::use(ggplot2[...], latex2exp[TeX])
   n <- nrow(info.quantiles)
   info.ecdf <- info.quantiles
   info.ecdf$F <- ecdf(theta)(info.ecdf$quantile)
   info.ecdf$type <- "ecdf"
   info.quantiles$F <- 1:n/n
   info.quantiles$type <- "quantile"
   rbind(info.ecdf, info.quantiles) |> 
      ggplot(aes(x=quantile, y=F, color=type)) +
         geom_line() +
         scale_color_manual(values=c("darkorange", "black")) +
         labs(
            x = TeX("$\\theta$"),
            y = TeX("$F(\\theta)$"),
            title = "Quantiles vs. ECDF",
            fill = "source"
         ) +
         mytheme()
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

select.items <- function(items, info.quantiles, n_max=6L, threshold=3.0){
   index.set <- list()
   # iterate over quantiles (get the current and next quantile)
   for (i in 1:nrow(info.quantiles)) {
      q0 <- info.quantiles$quantile[i]
      q1 <- info.quantiles$quantile[i+1]
      selection <- items |>
         dplyr::filter(info.argmax >= q0 & info.argmax < q1 & info.max >= threshold) |>
         dplyr::arrange(dplyr::desc(info.max)) |>
         utils::head(n_max)
      index.set[[i]] <- selection
   }
   do.call(rbind, index.set) |>
      dplyr::distinct() |>
      dplyr::arrange(info.argmax)
}

# -----------------------------------------------------------------------------
# parameter recovery

compare.parameters <- function(model, model.sub){
   get.estimates <- function(model){
      mirt::coef(model, simplify=T, rotate="none")$items |>
         data.frame() |>
         tibble::rownames_to_column(var='item') |>
         dplyr::mutate(item = as.numeric(item))
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

plot.recovery.d <- function(df.comparison){
   box::use(ggplot2[...], latex2exp[TeX])
   df.comparison |> 
      ggplot(aes(x = d.x, y = d.y)) +
         geom_point(alpha = 0.5) +
         labs(
            title = "Difficulty Recovery",
            x = "Full Difficulty",
            y = "Subtest Difficulty",
            ) +
         mytheme()
}

plot.recovery.a1 <- function(df.comparison){
   box::use(ggplot2[...], latex2exp[TeX])
   df.comparison |> 
      ggplot(aes(x = a1.x, y = a1.y)) +
         geom_point(alpha = 0.5) +
         labs(
            title = "Loading Recovery",
            x = "Full Loading",
            y = "Subtest Loading",
            ) +
         mytheme()
}

# -----------------------------------------------------------------------------
# hyperparameter search

create.subtest <- function(data, items, info.quantiles, hyper) {
   index.set <- select.items(items, info.quantiles, n_max=hyper$n_max, threshold=hyper$threshold)
   data.sub <- data[, as.character(index.set$item)]
   list(data=data.sub, items=index.set)
}

evaluate.selection <- function(theta, info.quantiles, info.items, items, index.set){
   # evaluation 0: before fitting
   ceiling <- max(rowSums(info.items[,-1]))
   cowplot::plot_grid(
      plot.theta(theta),
      plot.quantiles(info.quantiles, theta),
      plot.expected.testinfo(info.items, items, ceiling, "Full Testinfo"),
      plot.expected.testinfo(info.items, index.set, ceiling, "Expected Testinfo"),
      align = "v"
   )
}

plot.estimates <- function(model.sub, theta.sub){
   param.compare <- compare.parameters(model, model.sub)
   cowplot::plot_grid(
      plot.theta(theta.train, "Original"),
      plot.recovery.d(param.compare),
      plot.theta(theta.sub, "Reduced"),
      plot.recovery.a1(param.compare),
      align = "v"
   )
}

hyperparam.wrapper <- function(hyperparams, internal=T){
   # 1. create subtest
   info.quantiles <- get.info.quantiles(info.items, steps=hyperparams$n_quant)
   subtest <- create.subtest(data.train, items, info.quantiles, hyperparams)
   data.train.sub <- subtest$data
   data.test.sub <- data.test[colnames(data.train.sub)]
   items.sub <- subtest$items
   
   # fail gracefully if the set becomes too
   if (ncol(data.frame(data.train.sub)) < 10){
     return(list(sfs = data.frame(rmse = 9999), items = items.sub))
   }
   
   # 2. fit subtest
   ncycles <- ifelse(internal, 1000, 5000)
   tol <- ifelse(internal, 1e-4, 1e-5)
   model.sub <- run.mirt(data.train.sub, MOD, tol = tol, ncycles=ncycles)
   theta.train.sub <- get.theta(model.sub, method=METH, resp=data.train.sub)
   theta.test.sub <- get.theta(model.sub, method=METH, resp=data.test.sub)

   # 3. evaluate subtest
   score.table.sub <- get.score.table(theta.train.sub, theta.test.sub, scores.train, scores.test)
   sfs.sub <- score.stats(score.table.sub)
     
   # 4. return results
   list(items = items.sub,
        model = model.sub,
        theta.train = theta.train.sub,
        theta.test = theta.test.sub,
        info.quantiles = info.quantiles,
        df.score = score.table.sub,
        sfs = sfs.sub)
}

optimize.hyperparameters <- function(){
   box::use(rBayesianOptimization[...])
   objective <- function(n_max, threshold, n_quant) {
     hyperparams <- list(n_max=n_max, threshold=threshold, n_quant=n_quant)
     res <- hyperparam.wrapper(hyperparams, internal=T)
     sfs <- res$sfs
     score <- sfs$rmse + as.numeric(LAMBDA) * nrow(res$items) # minimize this
     list(Score = -score, Pred = 0)
  }
  BayesianOptimization(
   objective,
   bounds = list(n_max = c(1L, 7L),
                 threshold = c(0, 2),
                 n_quant = c(25L, 50L)),
   init_points = 5,
   n_iter = N_ITER,
   acq = "ucb", 
   kappa = 2.576,
   eps = 0.0,
   verbose = T)
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
if (BM %in% c("hellaswag", "mmlu")){
   datapath <- gpath("data/{BM}-sub.rds")
} else {
   datapath <- gpath("data/{BM}-preproc-split.rds")
}
full <- readRDS(datapath)
items <- full$items
items$item <- as.numeric(items$item)
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

# append itemfits to items
itemfitpath <- gpath("analysis/itemfits/{BM}.rds")
itemfits <- readRDS(itemfitpath) |>
   dplyr::filter(itemtype == MOD) |>
   dplyr::select(-itemtype)
if (!all(itemfits$item == items$item)){
  gprint("itemfits and items do not match!")
  quit()
}
items <- merge(items, itemfits, by="item")
rm(itemfits)

# prepare model
gprint("🚰 Loading {BM} fits...")
fitpath <- gpath("analysis/models/{BM}-{MOD}-cv.rds")
results <- readRDS(fitpath)
model <- results$model
items <- merge.params(items, model)
if (METH == "MAP") {
   theta <- results$df |> dplyr::filter(set == "train") |>
     dplyr::pull(theta) |> as.matrix()
   theta.train <- as.matrix(theta[-indices])
   theta.test <- as.matrix(theta[indices])
   theta.val <- results$df |> dplyr::filter(set == "test") |>
     dplyr::pull(theta) |> as.matrix()
} else {
   theta.train <- get.theta(model, method=METH, resp=data.train)
   theta.test <- get.theta(model, method=METH, resp=data.test)
   theta.val <- get.theta(model, method=METH, resp=data.val)
}
rm(results)

# summarize score
score.table <- get.score.table(theta.train, theta.val, scores.train, scores.val)
sfs.base <- score.stats(score.table)

# get item infos, remove outliers and plot distributions
info.items <- collect.item.info(model, theta.train, colnames(data))
info.items <- info.items |>
   dplyr::select(!as.character(items$item[items$outlier]))
items <- merge(items, summarize.info(info.items), by="item")

# run hyperparameter search using rBayesianOptimization
opt.results <- optimize.hyperparameters()
hyperparams <- as.list(opt.results$Best_Par)
final <- hyperparam.wrapper(hyperparams, internal = F)
compare.score.stats(sfs.base, final$sfs)
gprint("🎉 Reduced test to {nrow(final$items)} items (using a penalty coefficient of {LAMBDA}).")

out <- list(
   items = merge.params(final$items, final$model),
   model = final$model,
   theta.train = final$theta.train,
   theta.test = final$theta.test,
   info.items.orig = info.items,
   hyperparams = hyperparams,
   opt.results = opt.results,
   sfs.full = sfs.base,
   sfs.sub = final$sfs
)

outpath <- gpath("analysis/reduced/{BM}-{MOD}-{LAMBDA}.rds")
saveRDS(out, outpath)
gprint("💾 Saved results to {outpath}")
