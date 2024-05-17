# Get the information per item and construct a reduced test set:
#   1. 
# usage: Rscript reduce.R {benchmark} {model}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, merge.params])
parse.args(
   names = c("BM", "Model"),
   defaults = c("hellaswag", "2PL"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande"),
     Model = c("2PL", "3PL", "3PLu", "4PL")
   )
)
here::i_am("analysis/reduce.R")
set.seed(1)

