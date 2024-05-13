# Evaluate the performance of the IRT models on the given benchmark
#   1. Model comparisons (AIC, Chi-Square)
#   2. Item fits (Infit, Outfit)
#
# usage: Rscript evaluate.fit.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme])
parse.args(
   names = c("BM"),
   defaults = c("winogrande"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/evaluate.fit.R")
set.seed(1)

# =============================================================================
# helper functions  
compare.models <- function(results) {
   gprint("🔍 Model comparisons...")
   model_names <- names(results)
   args <- glue::glue("results[['{model_names}']]$model")
   args <- paste(args, collapse = ", ")
   call <- paste0("mirt::anova(", args, ")")
   comparisons <- eval(parse(text = call))
   rownames(comparisons) <- model_names
   comparisons
}

summarize.comparisons <- function(comparisons) {
   gprint("📊 Summarizing model comparisons")
   best_aic <- which.min(comparisons$AIC)
   aic <- round(min(comparisons$AIC))
   best_bic <- which.min(comparisons$BIC)
   bic <- round(min(comparisons$BIC))
   gprint("Best AIC: {rownames(comparisons)[best_aic]} ({aic})")
   gprint("Best BIC: {rownames(comparisons)[best_bic]} ({bic})")
}

