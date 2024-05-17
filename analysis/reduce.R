# Get the information per item and construct a reduced test set:
#   1. 
# usage: Rscript reduce.R {benchmark} {model}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, run.mirt, get.theta])
parse.args(
   names = c("BM", "Model"),
   defaults = c("hellaswag", "2PL"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande"),
     Model = c("2PL", "3PL", "3PLu", "4PL")
   )
)
here::i_am("analysis/reduce.R")
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
   info.sub <- info.items[,as.character(index.set$item)]
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
r <- cor(score.table$theta, score.table$score, method = 'spearman')
gprint("Spearman correlation Theta x Score: {round(r, 2)}")
cowplot::plot_grid(
   plot.theta.score(score.table),
   plot.perc(score.table),
   plot.score.error(score.table),
   plot.error.dist(score.table))
sfs <- score.stats(score.table)

# get item infos, remove outliers and plot distributions
info.items <- collect.item.info(model, theta, colnames(data))
info.items <- info.items |>
   dplyr::select(!as.character(items$item[items$outlier]))
info.quantiles <- get.info.quantiles(info.items, steps=40)


# =============================================================================
# subtest creation
# 1. select up to n_max items with the highest information within each quantile
items <- merge(items, summarize.info(info.items), by="item")
index.set <- select.items(items, info.quantiles, n_max=5L, threshold=1.0)
cowplot::plot_grid(
  plot.theta(theta),
  plot.quantiles(info.quantiles, theta),
  plot.expected.testinfo(info.items, items, 900, "Full Testinfo"),
  plot.expected.testinfo(info.items, index.set, 900, "Expected Testinfo"),
  align = "v"
)

