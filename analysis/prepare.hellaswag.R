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

