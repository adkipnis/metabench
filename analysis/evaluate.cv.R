# Evaluate the performance of the IRT models on the given benchmark
#   1. Score Recovery (Train Set)
#   2. Score Recovery (Test Set)
#
# usage: Rscript evaluate.cv.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme])
parse.args(
   names = c("BM"),
   defaults = c("hellaswag"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/evaluate.cv.R")
set.seed(1)

# =============================================================================
# helper functions  
plot.score <- function(df.score, itemtype, limits = c(200, 1000)){
   box::use(ggplot[...])
   df.score |> 
      dplyr::filter(itemtype == itemtype) |>
      ggplot(aes(x = score, y = p, color = type)) +
cv.extract <- function(results, itemtype) {
   dfs <- lapply(results[[itemtype]], function(fold) fold$df)
   for (i in 1:length(dfs)) {
      dfs[[i]]$type <- itemtype
      dfs[[i]]$fold <- i
   }
   do.call(rbind, dfs)
}

cv.collect <- function(results) {
   dfs <- lapply(names(results), function(itemtype) cv.extract(results, itemtype))
   dfs <- do.call(rbind, dfs)
   dfs$fold <- as.factor(dfs$fold)
   dfs$set <- factor(dfs$set, levels = c("train", "test"))
   dfs
}

spearmanize <- function(df.score) {
   gprint("Calculating Spearman correlation between theta and score...")
   df.score |>
      dplyr::group_by(type, set) |>
      dplyr::summarize(
            spearman = cor(theta, score, method = "spearman"),
            .groups = 'drop')
}

         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         scale_x_continuous(limits = limits) +
         scale_y_continuous(limits = limits) +
         facet_wrap( ~ fold) +
         scale_color_manual(values = c("train" = "gray", "test" = "orange")) +
         labs(
            title = glue("{BM} Score Reconstruction ({Model})"),
            x = "Score",
            y = "Prediction",
            color = "Type"
            ) +
         mytheme()
}

# =============================================================================
# load cv results
cvpath <- gpath("analysis/models/{BM}-cv.rds")
cvs <- readRDS(cvpath)

