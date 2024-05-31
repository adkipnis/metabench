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
   df <- results[[itemtype]]$df
   df$type <- itemtype
   df
}

cv.collect <- function(results) {
  dfs <- lapply(names(results), function(itemtype) cv.extract(results, itemtype))
  names(dfs) <- names(results)
  dfs <- do.call(rbind, dfs)
  dfs$set <- factor(dfs$set, levels = c("train", "test"))
  dfs$error <- dfs$score - dfs$p
  dfs
}

evaluate.fit <- function(df.score) {
   df.score |> 
      dplyr::group_by(type, set) |>
      dplyr::summarize(
            rmse = sqrt(mean(error^2)),
            mae = mean(abs(error)),
            r = cor(theta, score, method = "spearman"),
            .groups = 'drop')
}

   box::use(ggplot2[...])
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         labs(
            ) +
         mytheme()
}

         labs(
            ) +
         mytheme()
}

# =============================================================================
# load cv results
cvpath <- gpath("analysis/models/{BM}-cv.rds")
cvs <- readRDS(cvpath)
df.score <- cv.collect(cvs)

