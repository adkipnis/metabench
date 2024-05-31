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

# =============================================================================
# helper functions

df2list <- function(df){
  scenarios <- unique(gsub("\\..*", "", colnames(df)))
  data.list <- lapply(scenarios, function(s) df[, grepl(s, colnames(df))])
  names(data.list) <- scenarios
  data.list
}

get.scores <- function(data.list){
  scores.list <- lapply(names(data.list), function(n){
    out <- data.frame(rowSums(data.list[[n]]))
    colnames(out) <- n
    out
    })
  scores.df <- Reduce(rowmerge, scores.list)
  colnames(scores.df) <- names(data.list)
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
      RMSE = sqrt(mean(error^2)),
      MAE = mean(abs(error)),
      SSE = sum(error^2)
    )
}


evaluate.percentiles <- function(df.scores){
  df.scores |> 
    dplyr::summarise(
      RMSE = sqrt(mean(error.perc^2)),
      MAE = mean(abs(error.perc)),
      r = cor(means, p, method = "spearman")
    )
}

evaluate.scores <- function(scores.train, scores.test){
  df.train <- data.frame(scores.train, means = total.train)
  df.test <- data.frame(scores.test, means = total.test)
  mod.score <- lm(means ~ ., data = df.train)
  df.train <- predict.scores(df.train, mod.score)
  df.test <- predict.scores(df.test, mod.score)
  sfs.train <- evaluate.prediction(df.train)
  sfs.test <- evaluate.prediction(df.test)
  list(mod.score = mod.score,
       sfs.train = sfs.train,
       sfs.test = sfs.test,
       df.train = df.train,
       df.test = df.test)
}

plot.prediction <- function(df.scores, sfs = NULL, suffix = ""){
  text <- ''
  if (!is.null(sfs)){
    text <- text <- glue::glue(
      "RMSE = {round(sfs$RMSE, 2)}\nMAE = {round(sfs$MAE, 2)}")
  }
  box::use(ggplot2[...], latex2exp[TeX])
  x.label <- 0.9 * diff(range(df.scores$means)) + min(df.scores$means)
  y.label <- 0.1 * diff(range(df.scores$p)) + min(df.scores$p)
  ggplot(df.scores, aes(x = means, y = p)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    annotate("text", x = x.label, y = y.label, label = text, size = 3) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    labs(
      x = "True",
      y = "Predicted",
      title = glue::glue("Score Reconstruction {suffix}")
    ) +
    mytheme()
}

plot.perc <- function(df.scores, sfs = NULL, suffix = ""){
  text <- ''
  if (!is.null(sfs)){
    text <- text <- glue::glue(
      "RMSE = {round(sfs$RMSE, 2)}\nMAE = {round(sfs$MAE, 2)}\nr = {round(sfs$r, 2)}")
  }
  box::use(ggplot2[...], latex2exp[TeX])
  x.label <- 0.9 * diff(range(df.scores$means.perc)) + min(df.scores$means.perc)
  y.label <- 0.1 * diff(range(df.scores$p.perc)) + min(df.scores$p.perc)
  ggplot(df.scores, aes(y = p.perc, x = means.perc)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    annotate("text", x = x.label, y = y.label, label = text, size = 3) +
    labs(
      x = "% True",
      y = "% Predicted",
      title = glue::glue("Percentile Comparison {suffix}")
    ) +
    mytheme()
}

subsample <- function(data, remove = 10){
  n <- ncol(data)
  k <- n - remove
  sort(sample(1:n, k, replace = F))
}

apply.subsampling <- function(dl, indices.list){
  out <- lapply(names(dl), function(n){
    indices <- indices.list[[n]]
    dl[[n]][indices]
  })
  names(out) <- names(dl)
  out
}

subsample.wrapper <- function(dl.train, dl.test){
   # subsample
   indices.list <- lapply(dl.train, subsample)
   dl.train.sub <- apply.subsampling(dl.train, indices.list)
   dl.test.sub <- apply.subsampling(dl.test, indices.list)
   scores.train.sub <- get.scores(dl.train.sub)
   scores.test.sub <- get.scores(dl.test.sub)

   # evaluate
   out <- evaluate.scores(scores.train.sub, scores.test.sub)
   list(dl.train = dl.train.sub,
        dl.test = dl.test.sub,
        eval = out)
}

find.best.subset <- function(dl.train, dl.test, iters){
  sample.list <- list()
  for (i in 1:iters){
    sample.list[[i]] <- subsample.wrapper(dl.train, dl.test)
  }
  err.list <- sapply(sample.list, function(s) s$eval$sfs.test$SSE)
  i <- which.min(err.list)
  j <- which.max(err.list)
  best <- sample.list[[i]]
  worst <- sample.list[[j]]
  gprint("Test SSE (Range): {round(err.list[i], 3)} -- {round(err.list[j], 3)}")
  gprint("Reduced dataset to {n.data(best$dl.train)} items.")
  best
}


# =============================================================================
# prepare data
gprint("🚰 Loading MMLU data...")
mmlu <- readRDS(gpath("data/mmlu-preproc-split.rds"))
data <- mmlu$data.train
nc <- mmlu$max.points.orig
indices <- caret::createDataPartition(mmlu$scores.train, p = 0.1, list = F)
data.train <- data[-indices, ]
data.test <- data[indices, ]
goal <- round(1/4 * nrow(data))
data.list.train <- df2list(data.train)
data.list.test <- df2list(data.test)
scores.train <- get.scores(data.list.train)
scores.test <- get.scores(data.list.test)
total.train <- rowSums(scores.train) / nc * 100
total.test <- rowSums(scores.test) / nc * 100

# evolutionary algorithm to further reduce number of items
gprint("Starting evolutionary subsampling until at most {goal} items remain...")
dl.train.sub <- data.list.train
dl.test.sub <- data.list.test
while (n.data(dl.train.sub) > goal){
   subsample.res <- find.best.subset(dl.train.sub, dl.test.sub, iters = 5)
   dl.train.sub <- subsample.res$dl.train
   dl.test.sub <- subsample.res$dl.test
}

# check with validation set
data.val <- mmlu$data.test
data.list.val <- df2list(data.val)
scores.val <- get.scores(data.list.val)
total.val <- rowSums(scores.val) / nc * 100
df.val <- data.frame(scores.val, means = total.val)
df.val <- predict.scores(df.val, subsample.res$eval$mod.score)
sfs.val <- evaluate.prediction(df.val)

# plot final result
p.final <- cowplot::plot_grid(
  plot.prediction(subsample.res$eval$df.train, subsample.res$eval$sfs.train, "(Train)"),
  plot.prediction(subsample.res$eval$df.test, subsample.res$eval$sfs.test, "(Test)"),
  plot.prediction(df.val, sfs.val, "(Validation)"),
  nrow = 1, labels = "AUTO"
)
outpath <- gpath("plots/mmlu-reduced.png")
ggplot2::ggsave(outpath, p.final, width = 8, height = 8)
gprint("💾 Saved plot to {outpath}")

# subset data
data.train.sub <- Reduce(rowmerge, dl.train.sub)
data.test.sub <- Reduce(rowmerge, dl.test.sub)
mmlu$data <- rbind(data.train.sub, data.test.sub)
mmlu$data.val <- mmlu$data.val[colnames(mmlu$data)]
mmlu$items <- mmlu$items |> 
  dplyr::filter(item %in% colnames(mmlu$data))

# save data
outpath <- gpath("data/mmlu-sub.rds")
saveRDS(mmlu, outpath)
gprint("🏁 Saved subset data to {outpath}")
