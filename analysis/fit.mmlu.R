# Jointly fit MMLU single model on all subtests 
# 1. Perform exploratory FA on scores and discard non-unique subtests
# 2. Fit IRT model on remaining subtests
# usage: Rscript fit.mmlu.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, rowmerge, do.fa, mytheme])
here::i_am("analysis/fit.mmlu.R")
set.seed(1)

