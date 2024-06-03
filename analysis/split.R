# Split preprocessed data into train and test sets according to normalized full score
# usage: Rscript split.R

# =============================================================================
box::use(./utils[gprint, gpath, rowmerge])
here::i_am("analysis/split.R")
set.seed(1)
SHOW <- F

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
   indices <- which(rownames(data) %in% llms.test)
   out <- list(data.train = data[-indices, ],
               data.test = data[indices, ],
               scores.train = scores[-indices],
               scores.test = scores[indices],
               max.points.orig = all$max.points.orig,
               items = all$items)
   na.count <- sapply(out, function(x) sum(is.na(x)))
   if (any(na.count > 0)) gprint("⚠️ Incomplete data produced: {na.count}")
   saveRDS(out, gpath("data/{benchmark}-preproc-split.rds"))
   gprint("💾 Split off test set from {b}: {nrow(out$data.train)} remain.")
}


# =============================================================================
# prepare data
gprint("🚰 Loading scores...")
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
score.list <- lapply(benchmarks, collect.scores)
scores.partial <- Reduce(rowmerge, score.list)
scores.partial$mean <- rowMeans(scores.partial)

# check if score is distributed differently if we only took one model per user
if (SHOW){
   models <- rownames(scores.partial)
   users <- unique(sapply(strsplit(models, "/"), function(x) x[1]))
   last.models <- sapply(users, function(u) tail(grep(u, models), 1))
   par(mfrow = c(2, 1))
   xlim <- c(0, 100)
   plot(density(scores.partial$mean), xlim = xlim, main = "Density of mean scores")
   plot(density(scores.partial[last.models, "mean"]), xlim = xlim, main = "Density of mean scores for last model per user")
   par(mfrow = c(1, 1))
}

# select indices for test set
gprint("🎲 Selecting test indices...")
indices <- caret::createDataPartition(scores.partial$mean, p = 0.1, list = F)
llms.test <- rownames(scores.partial)[indices]
llms.rest <- rownames(scores.partial)[-indices]

# subset data
for (b in benchmarks) subset.data(b)
