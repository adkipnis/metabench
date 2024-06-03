# Further reduce HellaSwag to at max 1/4 of the number of LLMs
# 1. Drop a random subset of HellaSwag items and recalculate the scores
# 2. Perform exploratory factor analysis on the scores including HellaSwag
# 3. Try to predict the the normalized full score using the latent factors
# usage: Rscript prepare.hellaswag.R

# =============================================================================
box::use(./utils[mkdir, gprint, gpath, rowmerge, prop.indices, do.fa, mytheme])
here::i_am("analysis/prepare.hellaswag.R")
set.seed(1)

# =============================================================================
# helper functions
predict.scores <- function(df.scores, mod.score){
   df.scores$p <- predict(mod.score, df.scores)
   df.scores |>
      dplyr::mutate(error = means - p,
                    p.rank = rank(p),
                    p.perc = 100 * p.rank / max(p.rank),
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
  # prepare data
  df.train <- data.frame(sub.score = scores.train, means = means.train)
  df.test <- data.frame(sub.score = scores.test, means = means.test)
 
  # train GAM and predict scores from latent factors
  # mod.score <- lm(means ~ ., data = df.train)
  mod.score <- mgcv::gam(means ~ s(sub.score), data = df.train)
  df.train <- predict.scores(df.train, mod.score)
  df.test <- predict.scores(df.test, mod.score)
 
  # evaluate and save
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

subsample.wrapper <- function(data.train, data.test){
   # subsample and g
   indices <- subsample(data.train)
   data.train.sub <- data.train[,indices]
   data.test.sub <- data.test[,indices]
   scores.train.sub <- rowSums(data.train.sub) / nc * 100
   scores.test.sub <- rowSums(data.test.sub) / nc * 100

   # analyze and evaluate
   out <- evaluate.scores(scores.train.sub, scores.test.sub)
   list(data.train = data.train.sub,
        data.test = data.test.sub,
        eval = out)
}

find.best.subset <- function(data.train, data.test, iters){
  sample.list <- list()
  for (i in 1:iters){
    sample.list[[i]] <- subsample.wrapper(data.train, data.test)
  }
  err.list <- sapply(sample.list, function(s) s$eval$sfs.test$SSE)
  i <- which.min(err.list)
  j <- which.max(err.list)
  best <- sample.list[[i]]
  worst <- sample.list[[j]]
  gprint("Test SSE (Range): {round(err.list[i], 3)} -- {round(err.list[j], 3)}")
  gprint("Reduced dataset to {ncol(best$data.train)} items.")
  best
}

# =============================================================================
# prepare hellaswag
gprint("🚰 Loading HellaSwag...")
hs <- readRDS(gpath("data/hellaswag-preproc-split.rds"))
data <- hs$data.train
nc <- ncol(data)
scores <- rowSums(data) / nc * 100
goal <- round(1/4 * nrow(data))

# separate train:test split
indices <- caret::createDataPartition(scores, p = 0.1, list = F)
data.train <- data[-indices,]
data.test <- data[indices,]
means.train <- scores.train <- scores[-indices]
means.test <- scores.test <- scores[indices]

# start subsetting data
gprint("Starting evolutionary subsampling until at most {goal} items remain...")
data.train.sub <- data.train
data.test.sub <- data.test
while (ncol(data.train.sub) > goal){
   subsample.res <- find.best.subset(data.train.sub, data.test.sub, iters = 50)
   data.train.sub <- subsample.res$data.train
   data.test.sub <- subsample.res$data.test
}
p.train <- plot.prediction(subsample.res$eval$df.train, subsample.res$eval$sfs.train, "(Train)")
p.test <- plot.prediction(subsample.res$eval$df.test, subsample.res$eval$sfs.test, "(Test)")

# check with validation set
data.val <- hs$data.test
scores.val <- rowSums(data.val) / nc * 100
data.val.sub <- data.val[colnames(data.train.sub)]
scores.val.sub <- rowSums(data.val.sub) / nc * 100
df.val <- data.frame(sub.score = scores.val.sub, means = scores.val)
df.val <- predict.scores(df.val, subsample.res$eval$mod.score)
sfs.val <- evaluate.prediction(df.val)
p.val <- plot.prediction(df.val, sfs.val, "(Validation)")

# check with random subset of equal size
n.final <- ncol(data.val.sub)
indices.rand <- subsample(data.train, remove = ncol(hs$data.train) - n.final)
data.rand <- data.train[,indices.rand]
scores.rand <- rowSums(data.rand) / nc * 100
df.rand <- data.frame(sub.score = scores.rand, means = scores.train)
mod.score.rand <- mgcv::gam(means ~ s(sub.score), data = df.rand)
data.val.rand <- hs$data.test[colnames(data.rand)]
scores.val.rand <- rowSums(data.val.rand) / nc * 100
df.rand.val <- data.frame(sub.score = scores.val.rand, means = scores.val)
df.rand.val <- predict.scores(df.rand.val, mod.score.rand)
(sfs.rand.val <- evaluate.prediction(df.rand.val))
p.rand <- plot.prediction(df.rand.val, sfs.rand.val, "(Random)")

# plot final result
p <- cowplot::plot_grid(p.train, p.test, p.val, p.rand, nrow = 1, labels = "AUTO")
outpath <- gpath("plots/hellaswag-reduced.png")
ggplot2::ggsave(outpath, p, width = 18, height = 8)
saveRDS(list(p.train, p.test, p.val, p.rand), gpath("plots/hellaswag-reduced.rds"))
gprint("💾 Saved plot to {outpath}")

# subset data
data.sub <- rbind(data.train.sub, data.test.sub) 
out <- list(data.train = data.sub,
            data.test = hs$data.test[colnames(data.sub)],
            mod.score = subsample.res$eval$mod.score,
            scores.train = hs$scores.train[rownames(data.sub)],
            scores.test = hs$scores.test,
            max.points.orig = hs$max.points.orig,
            items = hs$items |> dplyr::filter(item %in% colnames(data.train.sub)))

# save data
outpath <- gpath("data/hellaswag-sub.rds")
saveRDS(out, outpath)
gprint("🏁 Saved subset data to {outpath}")

