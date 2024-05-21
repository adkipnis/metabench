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

# =============================================================================
# helper functions

rowmerge <- function(df1, df2){
   merge(df1, df2, by="row.names") |>
     tibble::column_to_rownames("Row.names")
}

collect.theta <- function(benchmark){
   fitpath <- gpath("analysis/models/{benchmark}-all.rds")
   results <- readRDS(fitpath)
   modeltype <- benchmarks[[benchmark]]
   # TODO: choose EAPsum for some
   theta <- results[[modeltype]]$theta
   datapath <- gpath("data/{benchmark}_preproc.rds")
   names <- rownames(readRDS(datapath)$data)
   theta <- as.data.frame(theta)
   rownames(theta) <- names
   colnames(theta) <- benchmark
   theta
}

merge.theta <- function(theta.full){
   theta.reduced <- theta.full[[1]]
   for (i in 2:length(theta.full)){
      theta.reduced <- rowmerge(theta.reduced, theta.full[[i]])
   }
   theta.reduced
}

collect.scores <- function(benchmark){
   datapath <- gpath("data/{benchmark}_preproc.rds")
   all <- readRDS(datapath)
   scores <- data.frame(all$score)
   colnames(scores) <- benchmark
   rownames(scores) <- rownames(all$data)
   scores
}

