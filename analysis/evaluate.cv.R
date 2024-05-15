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

plot.score <- function(df.score, limits, outpath = NULL){
   box::use(ggplot2[...])
   p <- df.score |> 
      ggplot(aes(x = score, y = p, color = set)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         facet_wrap(~type, scales = "free") +
         scale_x_continuous(limits = limits) +
         scale_y_continuous(limits = limits) +
         scale_color_manual(values = c("train" = "gray", "test" = "orange")) +
         labs(
            title = glue::glue("{BM} Score Reconstruction"),
            x = "true",
            y = "predicted",
            color = "Set"
            ) +
         mytheme()
   # save or print
   if (!is.null(outpath)) {
      ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Score reconstruction saved to {outpath}")
   } else {
      print(p)
   }
}

# =============================================================================
# load cv results
cvpath <- gpath("analysis/models/{BM}-cv.rds")
cvs <- readRDS(cvpath)

