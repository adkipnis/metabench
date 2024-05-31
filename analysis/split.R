# Split preprocessed data into train and validation sets according to normalized full score
# usage: Rscript split.R

# =============================================================================
box::use(./utils[gprint, gpath, rowmerge])
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
   indices <- which(rownames(data) %in% llms.val)
   out <- list(data.train = data[-indices, ],
               data.test = data[indices, ],
               scores.train = scores[-indices],
               scores.test = scores[indices],
               max.points.orig = all$max.points.orig)
   na.count <- sapply(out, function(x) sum(is.na(x)))
   if (any(na.count > 0)) gprint("⚠️ Incomplete data produced: {na.count}")
   saveRDS(out, gpath("data/{benchmark}-preproc-split.rds"))
   gprint("💾 Split off validation set from {b}.")
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
indices <- caret::createDataPartition(scores.partial$mean, p = 0.1, list = F)
llms.val <- rownames(scores.partial)[indices]
llms.rest <- rownames(scores.partial)[-indices]

# subset data
for (b in benchmarks) subset.data(b)
