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
collect.scores <- function(benchmark, val = F){
   all <- readRDS(gpath("data/{benchmark}-preproc-split.rds"))
   if (val){
     scores <- all$scores.orig.val
   } else {
     scores <- all$scores.orig
   }
   scores.norm <- as.data.frame(100 * scores / all$max.points.orig)
   colnames(scores.norm) <- benchmark
   scores.norm
}

make.score.df <- function(scores, fa.res, means){
  fs <- psych::factor.scores(scores, fa.res, method = "Bartlett")$scores
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
                    p.perc = p.rank / max(p.rank),
                    means.rank = rank(means),
                    means.perc = means.rank / max(means.rank))
}

evaluate.prediction <- function(df.scores){
  df.scores |> 
    dplyr::summarise(
      RMSE = sqrt(mean((means - p)^2)),
      MAE = mean(abs(means - p)),
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

plot.scores <- function(df.scores, sfs = NULL){
  text <- ''
  if (!is.null(sfs)){
    text <- glue::glue("RMSE = {round(sfs$RMSE, 2)}\nMAE = {round(sfs$MAE, 2)}")
  }
  box::use(ggplot2[...], latex2exp[TeX])
  x.label <- 0.9 * diff(range(df.scores$p)) + min(df.scores$p)
  y.label <- 0.1 * diff(range(df.scores$means)) + min(df.scores$means)
  ggplot(df.scores, aes(x = p, y = means)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
   annotate("text", x = x.label, y = y.label, label = text, size = 3) +
   labs(
      x = "Predicted",
      y = "True",
      title = "Mean Score Reconstruction"
   ) +
   mytheme()
}

plot.perc <- function(df.scores, sfs = NULL){
  text <- ''
  if (!is.null(sfs)){
    text <- glue::glue("r = {round(sfs$r, 2)}")
  }
  box::use(ggplot2[...], latex2exp[TeX])
  # get 0.9 of x range and 0.1 of y range
  x.label <- 0.9 * diff(range(df.scores$p.perc)) + min(df.scores$p.perc)
  y.label <- 0.1 * diff(range(df.scores$means.perc)) + min(df.scores$means.perc)
  ggplot(df.scores, aes(x = p.perc, y = means.perc)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   annotate("text", x = x.label, y = y.label, label = text, size = 3) +
  labs(
      x = "% Predicted",
      y = "% True",
      title = "Percentile Comparison"
   ) +
   mytheme()
}

plot.evaluation <- function(df.scores, sfs = NULL, labels = NULL){
  cowplot::plot_grid(
    plot.scores(df.scores, sfs),
    plot.perc(df.scores, sfs),
    labels = labels,
    nrow = 1)
}

subsample <- function(data, p){
  n <- ncol(data)
  k <- round(n * p)
  sort(sample(1:n, k))
}

subsample.wrapper <- function(data.train, data.test){
   # subsample
   sample.indices <- subsample(data.train, KEEPRATE)
   data.train.sub <- data.train[, sample.indices]
   data.test.sub <- data.test[, sample.indices]

   # adjust scores (use global variable)
   scores.train.sub <- scores.train
   scores.train.sub$hellaswag <- rowSums(data.train.sub)/nc * 100
   scores.test.sub <- scores.test
   scores.test.sub$hellaswag <- rowSums(data.test.sub)/ nc * 100

   # analyze and evaluate
   fa.sub <- do.fa(scores.train.sub, 2, verbose = F)
   out <- evaluate.scores(scores.train.sub, scores.test.sub, fa.sub)
   list(data.train = data.train.sub,
        data.test = data.test.sub,
        fa = fa.sub,
        eval = out)
}

find.best.subset <- function(data.train, data.test, iters){
  sample.list <- list()
  for (i in 1:iters){
    sample.list[[i]] <- subsample.wrapper(data.train, data.test)
  }
  rmse.list <- sapply(sample.list, function(s) s$eval$sfs.test$RMSE)
  i <- which.min(rmse.list)
  j <- which.max(rmse.list)
  best <- sample.list[[i]]
  worst <- sample.list[[j]]
  gprint("Test RMSE (Range): {round(rmse.list[i], 3)} -- {round(rmse.list[j], 3)}")
  gprint("Reduced dataset to {ncol(best$data.train)} items.")
  best
}

# =============================================================================
# prepare scores
benchmarks <- c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
gprint("🚰 Loading scores...")
score.list <- lapply(benchmarks, collect.scores)
scores <- Reduce(rowmerge, score.list)
means <- rowMeans(scores)
indices <- prop.indices(means, p = 0.1)
scores.train <- scores[-indices, ]
scores.test <- scores[indices, ]
means.train <- means[-indices]
means.test <- means[indices]

# prepare hellaswag
gprint("🚰 Loading HellaSwag...")
all <- readRDS(gpath("data/hellaswag-preproc-split.rds"))
nc <- all$max.points.orig
data <- all$data[rownames(scores), ] # only keep common LLMs
data.train <- data[-indices, ]
data.test <- data[indices, ]
goal <- round(1/4 * nrow(data))
rm(all)

# get baseline RMSE
fa.base <- do.fa(scores.train, 2, verbose = T)
# sfs <- evaluate.scores(scores.test, fa.base, means.test)
# gprint("Baseline RMSE: {round(sfs$RMSE, 2)}")

# start subsetting data
gprint("Starting evolutionary subsampling until at most {goal} items remain...")
out = subsample.wrapper(data.train, scores.train)



# while (ncol(data.sub) > goal){
#   subsample.res <- find.best.subset(data.sub, scores, iters = 25)
#   data.sub <- subsample.res$data
# }
