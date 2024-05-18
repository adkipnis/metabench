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
# usage: Rscript reduce.R {benchmark} {model} {method}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, mytheme, run.mirt, get.theta])
parse.args(
   names = c("BM", "Model"),
   defaults = c("hellaswag", "2PL"),
   names = c("BM", "Model", "Method"),
   defaults = c("gsm8k", "3PLu", "MAP"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande"),
     Model = c("2PL", "3PL", "3PLu", "4PL")
     Model = c("2PL", "3PL", "3PLu", "4PL"),
     Method = c("MAP", "EAPsum") # for theta estimation
   )
)
Saveplots <- T
here::i_am("analysis/reduce.R")
mkdir("plots")
mkdir("analysis/reduced")
set.seed(1)

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
      outpath <- gpath("plots/{BM}-{Model}-{Method}-{outsuffix}.png")
      ggplot2::ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Saved plot to {outpath}")
   } else {
      print(p)
   }
}

# -----------------------------------------------------------------------------
# Score prediction
get.score.table <- function(theta, scores){
   colnames(theta) <- 'theta'
   df <- data.frame(theta=theta, score=scores) |>
      dplyr::arrange(theta) |>
      dplyr::mutate(rank.score = rank(score),
                    rank.theta = rank(theta),
                    perc.score = rank.score/max(rank.score),
                    perc.theta = rank.theta/max(rank.theta))
   mod.score <- mgcv::gam(score ~ s(theta), data = df)
   df$p <- predict(mod.score)
   df |> dplyr::mutate(error = score - p)
}

plot.theta.score <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   p <- df.score |> 
      ggplot(aes(x = theta, y = score)) +
         geom_point(alpha = 0.5) +
         labs(
            title = glue::glue("Theta vs. Score"),
            x = TeX("$\\theta$"),
            y = "Full Score",
            ) +
         mytheme() 

   # print prediction line
   if ("p" %in% colnames(df.score)) {
      p + geom_line(aes(y = p), color = "red")
   } else {
      p
   }
}

plot.perc <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   df.score |> 
      ggplot(aes(x = perc.theta, y = perc.score)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         labs(
            title = glue::glue("Percentile comparison"),
            x = TeX("$\\% \\theta$"),
            y = "% Full Score",
            ) +
         mytheme()
}

plot.score.error <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   df.score |> 
      ggplot(aes(x = p, y = error)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 0,
                     linetype = "dashed") +
         labs(
            title = glue::glue("Predicted vs. Error"),
            x = "Predicted Score",
            y = "Error",
            ) +
         mytheme()
}

plot.error.dist <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   max_score <- max(df.score$score)
   df.score |> 
      ggplot(aes(x = abs(error)/max_score)) +
         geom_histogram(bins = 50, fill="white", color="black") +
         xlim(NA, 1) +
         labs(
            title = glue::glue("Error Distribution"),
            x = "Points / Max Score",
            y = "Count",
            ) +
         mytheme()
}

evaluate.score.table <- function(score.table){
   r <- cor(score.table$theta, score.table$score, method = 'spearman')
   gprint("Spearman correlation Theta x Score: {round(r, 2)}")
   cowplot::plot_grid(
      plot.theta.score(score.table),
      plot.perc(score.table),
      plot.score.error(score.table),
      plot.error.dist(score.table))
}

score.stats <- function(df.score){
   abs.error <- abs(df.score$error)
   out <- list(
      mae = mean(abs.error),
      sd = sd(abs.error),
      max = max(abs.error),
      min = min(abs.error),
      total = sum(abs.error)
   )
   gprint("📊 Score absolute error:
          Mean: {round(out$mae, 2)}
          SD: {round(out$sd, 2)}
          Range: [{round(out$min, 2)}, {round(out$max, 2)}]
          Total: {round(out$total, 2)}")
   out
}

compare.score.stats <- function(sfs, sfs.sub){
   out <- list()
   for (key in names(sfs)) {
      out[[key]] <- sfs.sub[[key]] - sfs[[key]]
   }
   gprint("📊 Abs. Error Delta (subtest - full test):
          Mean: {round(out$mae, 2)}
          SD: {round(out$sd, 2)}
          Range: [{round(out$min, 2)}, {round(out$max, 2)}]
          Total: {round(out$total, 2)}")
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

plot.theta <- function(theta){
   box::use(ggplot2[...], latex2exp[TeX])
   as.data.frame(theta) |> 
      ggplot(aes(x = F1)) +
         geom_density(color="black") +
         labs(
            title = "Theta Distribution",
            x = TeX("$\\theta$"),
            y = TeX("$f(\\theta)$"),
            ) +
         mytheme()
}

plot.testinfo <- function(model, theta) {
   box::use(ggplot2[...], latex2exp[TeX])
   info.test <- mirt::testinfo(model, Theta = theta)
   data.frame(theta=theta, info=info.test) |>
      ggplot(aes(x = F1, y = info)) +
      # increase linewidth
         geom_line(linewidth = 1) +
         labs(
            title = "Full Testinfo",
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

create.subtest <- function(data, items, info.items, theta, info.quantiles, hyper) {
   index.set <- select.items(items, info.quantiles, n_max=hyper$n_max, threshold=hyper$threshold)
   cowplot::plot_grid(
      plot.theta(theta),
      plot.quantiles(info.quantiles, theta),
      plot.expected.testinfo(info.items, items, 900, "Full Testinfo"),
      plot.expected.testinfo(info.items, index.set, 900, "Expected Testinfo"),
      align = "v"
   )
   data.sub <- data[, as.character(index.set$item)]
   items.sub <- items[index.set$item,]
   list(data=data.sub, items=items.sub)
}

evaluate.subtest.params <- function(model.sub, theta.sub, outpath = NULL){
   # evaluation 1: parameter recovery + testinfo
   param.compare <- compare.parameters(model, model.sub)
   p <- cowplot::plot_grid(
      plot.theta(theta.sub),
      plot.recovery.d(param.compare),
      plot.testinfo(model.sub, theta.sub),
      plot.recovery.a1(param.compare),
      align = "v"
   )
   # save or print
   if (!is.null(outpath)) {
      ggplot2::ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Parameter recovery saved to {outpath}")
   } else {
      print(p)
   }
}

evaluate.subtest.score <- function(theta.sub, scores, outpath = NULL){
   # evaluation 2: score prediction
   score.table.sub <- get.score.table(theta.sub, scores)
   p <- evaluate.score.table(score.table.sub)

   # save or print
   if (!is.null(outpath)) {
      ggplot2::ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Score prediction saved to {outpath}")
   } else {
      print(p)
   }
   score.stats(score.table.sub)
}

hyperparam.wrapper <- function(hyperparams){
   # 1. create subtest
   info.quantiles <- get.info.quantiles(info.items, steps=hyperparams$n_quant)
   subtest <- create.subtest(data, items, info.items, theta, info.quantiles, hyperparams)
   data.sub <- subtest$data
   items.sub <- subtest$items

   # 2. fit subtest
   model.sub <- run.mirt(data.sub, Model)
   theta.sub <- get.theta(model.sub, method="MAP")

   # 3. evaluate subtest
   evaluate.subtest.params(model.sub, theta.sub)
   evaluate.subtest.score(theta.sub, scores)
}


# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
datapath <- gpath("data/{BM}_preproc.rds")
full <- readRDS(datapath)
data <- full$data
items <- full$items
scores <- full$scores
rm(full)

# append itemfits to items
itemfitpath <- gpath("analysis/itemfits/{BM}.rds")
itemfits <- readRDS(itemfitpath) |>
   dplyr::filter(itemtype == Model) |>
   dplyr::select(-itemtype)
items <- merge(items, itemfits, by="item")
rm(itemfits)

# prepare model
gprint("🚰 Loading {BM} fits...")
fitpath <- gpath("analysis/models/{BM}-all.rds")
results <- readRDS(fitpath)[[Model]]
theta <- results$theta
model <- results$model
items <- merge.params(items, model)
rm(results)

# summarize score
score.table <- get.score.table(theta, scores)
evaluate.score.table(score.table)
sfs <- score.stats(score.table)

# get item infos, remove outliers and plot distributions
info.items <- collect.item.info(model, theta, colnames(data))
info.items <- info.items |>
   dplyr::select(!as.character(items$item[items$outlier]))

# run hyperparameter search
items <- merge(items, summarize.info(info.items), by="item")
hyperparams <- list(n_max=6L, threshold=3.0, n_quant=40)
sfs.sub <- hyperparam.wrapper(hyperparams)

