# Further reduce MMLU to at max 1/4 of the number of LLMs
# 0. Perform exploratory FA on scores and discard non-unique subtests
# 1. Drop a random subset of MMLU items per scenario and recalculate the scores
# 2. Perform exploratory factor analysis on the scores
# 3. Try to predict the the normalized full score using the latent factor
# usage: Rscript prepare.mmlu.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, gprint, gpath, prop.indices, rowmerge, do.fa, mytheme])
here::i_am("analysis/prepare.mmlu.R")
set.seed(1)
SHOW <- F
KEEPRATE <- 0.98

# =============================================================================
# helper functions

df2list <- function(df){
  scenarios <- unique(gsub("\\..*", "", colnames(df)))
  data.list <- lapply(scenarios, function(s) df[, grepl(s, colnames(df))])
  names(data.list) <- scenarios
  data.list
}

get.scores <- function(data.list){
  scores.list <- lapply(data.list, function(d) data.frame(rowSums(d)))
  scores.df <- invisible(Reduce(rowmerge, scores.list))
  colnames(scores.df) <- names(scores.list)
  scores.df
}

n.data <- function(data.list){
  sum(sapply(data.list, function(d) ncol(d)))
}

plot.unique <- function(unique){
   box::use(ggplot2[...])
   data.frame(unique = unique, id = 1:length(unique)) |>
      ggplot(aes(x=id, y=unique)) +
         geom_bar(stat="identity", fill="white", color="black") +
         xlab("Test ID (sorted)") +
         ylab("Uniqueness") +
         ggtitle("Unique variance of MMLU subtests") +
         mytheme()
}
#
make.score.df <- function(scores, fa.res, means){
  fs <- psych::factor.scores(scores, fa.res)$scores
  colnames(fs) <- paste0("F", 1:ncol(fs))
  data.frame(fs, means)
}

train.scores <- function(df.scores){
  pred.names <- colnames(df.scores)[-ncol(df.scores)]
  formula <- glue::glue("means ~ {paste0('s(', pred.names, ')', collapse=' + ')}")
  mgcv::gam(as.formula(formula), data = df.scores)
}

predict.scores <- function(df.scores, mod.score){
   df.scores$p <- predict(mod.score, df.scores)
   df.scores |>
      dplyr::mutate(error = means - p,
                    p.rank = rank(p),
                    p.perc = 100* p.rank / max(p.rank),
                    means.rank = rank(means),
                    means.perc = 100 * means.rank / max(means.rank),
                    error.perc = means.perc - p.perc)
}

evaluate.prediction <- function(df.scores){
  df.scores |> 
    dplyr::summarise(
      RMSE = sqrt(mean((error.perc)^2)),
      MAE = mean(abs(error.perc)),
      r = cor(means, p, method = "spearman")
    )
}

evaluate.scores <- function(scores.train, scores.test, fa.train){
  df.train <- make.score.df(scores.train, fa.train, means.train)
  df.test <- make.score.df(scores.test, fa.train, means.test)
  mod.score <- train.scores(df.train)
  df.train <- predict.scores(df.train, mod.score)
  df.test <- predict.scores(df.test, mod.score)
  sfs.train <- evaluate.prediction(df.train)
  sfs.test <- evaluate.prediction(df.test)
  list(sfs.train = sfs.train, sfs.test = sfs.test,
       df.train = df.train, df.test = df.test)
}

plot.perc <- function(df.scores, sfs = NULL){
  text <- ''
  if (!is.null(sfs)){
    text <- text <- glue::glue(
      "RMSE = {round(sfs$RMSE, 2)}\nMAE = {round(sfs$MAE, 2)}\nr = {round(sfs$r, 2)}")
  }
  box::use(ggplot2[...], latex2exp[TeX])
  # get 0.9 of x range and 0.1 of y range
  x.label <- 0.9 * diff(range(df.scores$p.perc)) + min(df.scores$p.perc)
  y.label <- 0.1 * diff(range(df.scores$means.perc)) + min(df.scores$means.perc)
  ggplot(df.scores, aes(x = p.perc, y = means.perc)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   annotate("text", x = x.label, y = y.label, label = text, size = 50) +
  labs(
      x = "% Predicted",
      y = "% True",
      title = "Percentile Comparison"
   ) +
   mytheme()
}

plot.evaluation <- function(df.scores, sfs = NULL, labels = NULL){
  cowplot::plot_grid(
    # plot.scores(df.scores, sfs),
    plot.perc(df.scores, sfs),
    labels = labels,
    nrow = 1)
}

subsample <- function(dataset, percentage){
   n <- ncol(dataset)
   k <- round(n * percentage)
   indices <- sort(sample(1:n, k))
   dataset[, indices]
}

subsample.wrapper <- function(data.list, percentage = 0.95){
   subsample.p <- function(d) subsample(d, percentage)
   data.list.sample <- lapply(data.list, subsample.p)
   scores.sample <- Reduce(rowmerge, lapply(data.list.sample, collect.scores))
   fa.mmlu.sample <- do.fa(scores.sample, 1, verbose = F)
   sfs.sample <- evaluate.scores(scores.sample, fa.mmlu.sample,
                           full.points = rowSums(scores), justsummary = T)
   list(data.list = data.list.sample, scores = scores.sample,
        fa = fa.mmlu.sample, sfs = sfs.sample)
}

find.best.subset <- function(data.list, iters){
   sample.list <- list()
   for (i in 1:iters){
      sample.list[[i]] <- subsample.wrapper(data.list)
   }
   i <- which.min(sapply(sample.list, function(s) s$sfs$RMSE))
   out <- sample.list[[i]]
   gprint("Best RMSE: {round(out$sfs$RMSE, 3)}")
   gprint("Reduced dataset to {n.data(out$data.list)} items.")
   out
}

# =============================================================================
# prepare data
gprint("🚰 Loading MMLU data...")
mmlu.files <- list.files(gpath("data"), pattern="mmlu_.*csv", full.names=T)
mmlu.data <- mmlu.files[!grepl("prompts", mmlu.files)]
mmlu.prompts <- mmlu.files[grepl("prompts", mmlu.files)]
mmlu.names <- gsub("mmlu_", "", gsub(".csv", "", basename(mmlu.data)))
data.list <- lapply(mmlu.data, collect.data)
prompt.list <- lapply(mmlu.prompts, collect.prompts)
names(prompt.list) <- names(data.list) <- mmlu.names
scores <- Reduce(rowmerge, lapply(data.list, collect.scores))

# exploratory factor analysis
if (SHOW){
  cor(scores) |>
    corrplot::corrplot(method = "color",
                       order="hclust",
                       tl.cex = 0.3,
                       tl.col = "black",
                       col.lim = c(0,1))
}
fa.mmlu <- do.fa(scores, 1)
p.full <- evaluate.scores(scores, fa.mmlu)

# determine unique contribution of subtests
unique <- sort(fa.mmlu$uniquenesses, decreasing=T)
if (SHOW) plot.unique(unique)
keepers <- names(unique[1:30]) # keep first n
# keepers <- names(unique[unique > 0.1]) # alternatively: keep most informative
scores.sub <- scores[keepers]
fa.mmlu.sub <- do.fa(scores.sub, 1)
p.sub <- evaluate.scores(scores.sub, fa.mmlu.sub, full.points = rowSums(scores),
                         labels = c("C", "D"))
n.items <- n.data(data.list[keepers])
gprint("\n\nReduced dataset from {n.data(data.list)} to {n.items} items.")

# evolutionary algorithm to further reduce number of items
subsample.res <- list(data.list = data.list[keepers])
gprint("Starting evolutionary subsampling until at most {GOAL} items remain...")
while (n.items > GOAL){
  subsample.res <- find.best.subset(subsample.res$data.list, iters = 30)
  n.items <- n.data(subsample.res$data.list)
}
p.sample <- evaluate.scores(subsample.res$scores, subsample.res$fa,
                            full.points = rowSums(scores), labels = c("E", "F"))

# save plot
p <- cowplot::plot_grid(p.full, p.sub, p.sample, align = "v", nrow = 3)
outpath <- gpath("plots/mmlu_efa.png")
ggplot2::ggsave(outpath, p, width = 8, height = 8)
gprint("💾 Saved plot to {outpath}")

# subset data
data.sub <- Reduce(rowmerge, subsample.res$data.list)
prompts.sub <- Reduce(rbind, prompt.list[keepers]) |> 
   dplyr::filter(item %in% colnames(data.sub))
outpath <- gpath("data/mmlu_sub.rds")
out <- list(data = data.sub, prompts = prompts.sub,
            scores = scores, scores.sub = scores.sub)
saveRDS(out, outpath)
gprint("🏁 Saved subset data to {outpath}")

