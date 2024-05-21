# Go beyond what single items measure by doing factor analysis on the test level.
# This makes single item thetas more interpretable (and potentially allows for even more reduction).
# usage: Rscript meta.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme])
Saveplots <- T
mkdir("plots")
here::i_am("analysis/meta.R")
set.seed(1)
benchmarks <- list(arc="4PL", gsm8k="3PLu", hellaswag="3PL", truthfulqa="3PL", winogrande="3PL")
# TODO: include MMLU

