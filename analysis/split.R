# Split preprocessed data into train and validation sets according to normalized full score
# usage: Rscript split.R

# =============================================================================
box::use(./utils[gprint, gpath, prop.indices])
here::i_am("analysis/split.R")
set.seed(1)

# =============================================================================
# helper functions
collect.scores <- function(benchmark){
   all <- readRDS(gpath("data/{benchmark}-preproc.rds"))
   scores.norm <- as.data.frame(100 * all$scores.orig / all$max.points.orig)
   colnames(scores.norm) <- benchmark
   scores.norm
}

subset.data <- function(benchmark){
   all <- readRDS(gpath("data/{benchmark}-preproc.rds"))
   data <- all$data
   scores <- all$scores.orig
   indices <- rownames(data) %in% llms.val
   all$data <- data[!indices, ]
   all$data.val <- data[indices, ]
   all$scores.orig <- all$scores.orig[!indices]
   all$scores.orig.val <- all$scores.orig[indices]
   saveRDS(all, gpath("data/{benchmark}-preproc-split.rds"))
}


# =============================================================================
# prepare data
gprint("🚰 Loading scores...")
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
score.list <- lapply(benchmarks, collect.scores)
scores.partial <- Reduce(rowmerge, score.list)
scores.partial$mean <- rowMeans(scores.partial)

# select indices for validation set
gprint("🎲 Selecting validation indices...")
indices <- prop.indices(scores.partial$mean)
llms.val <- rownames(scores.partial)[indices]
llms.rest <- rownames(scores.partial)[-indices]

# subset data
invisible(lapply(benchmarks, subset.data))
gprint("💾 Split off the same {length(llms.val)} models from each preprocessed dataset.")
