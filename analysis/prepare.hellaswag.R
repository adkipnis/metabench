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
                F1.rank = rank(F1),
                means.rank = rank(means),
                F1.perc = F1.rank / max(F1.rank),
                means.perc = means.rank / max(means.rank))
}

evaluate.prediction <- function(df.scores){
  df.scores |> 
    dplyr::summarise(
      RMSE = sqrt(mean((means - p)^2)),
      MAE = mean(abs(means - p)),
      r = cor(means, F1, method = "spearman")
    )
}

subsample <- function(data, p = 0.95){
  n <- ncol(data)
  k <- round(n * p)
  indices <- sort(sample(1:n, k))
  data[, indices]
}

subsample.wrapper <- function(data.train, scores.train){
  data.sub <- subsample(data.train, 0.95)
  scores.sub <- scores.train
  scores.sub$hellaswag <- rowSums(data.sub)/nc * 100
  fa.sub <- do.fa(scores.sub, 2, verbose = F)
  df.train <- make.score.df(scores.sub, fa.sub, means.train)
  df.test <- make.score.df(scores.test, fa.sub, means.test)
  mod.score <- train.scores(df.train)
  df.train <- predict.scores(df.train, mod.score)
  df.test <- predict.scores(df.test, mod.score)
  sfs.train <- evaluate.prediction(df.train)
  sfs.test <- evaluate.prediction(df.test)
  list(data = data.sub, fa = fa.sub,
       sfs.train = sfs.train, sfs.test = sfs.test)
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

