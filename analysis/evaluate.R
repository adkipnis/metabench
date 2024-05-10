# Evaluate the performance of the IRT models on the given benchmark
# A) Cross-Validated IRT Models
#   1. Score Recovery (Train Set)
#   2. Score Recovery (Test Set)
# B) Full IRT Models
#   1. Model comparisons (AIC, Chi-Square)
#   2. Item fits (Infit, Outfit)
#
# usage: Rscript cv.R {benchmark} {model}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath])
parse.args(
   names = c("BM"),
   defaults = c("hellaswag"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/evaluate.R")
set.seed(1)

# =============================================================================
# helper functions  


# =============================================================================
# load cv results

