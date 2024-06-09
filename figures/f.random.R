
# =============================================================================
box::use(./utils[mkdir, gprint, gpath])
here::i_am("figures/f.random.R")

# =============================================================================
# helper functions

# =============================================================================
# prepare data
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
sizes <- seq(50, 500, 50)
BM <- "arc"
results <- matrix(NA, nrow = length(sizes), ncol = 2)
results[, 1] <- sizes
for (i in seq_along(sizes)) {
   inpath <- gpath("data/{BM}-sub-{N}.rds")
   resulults[i, 2] <- readRDS(inpath)$test.rmse
}


