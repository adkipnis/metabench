# Jointly fit MMLU single model on all subtests 
# 1. Perform exploratory FA on scores and discard non-unique subtests
# 2. Fit IRT model on remaining subtests
# usage: Rscript fit.mmlu.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, rowmerge, do.fa, mytheme])
here::i_am("analysis/fit.mmlu.R")
set.seed(1)

# =============================================================================
# helper functions

collect.scores <- function(datapath){
   all <- readRDS(datapath)
   scores <- data.frame(all$scores)
   benchmark <- gsub("mmlu_", "", gsub("_preproc.rds", "", basename(datapath)))
   colnames(scores) <- benchmark
   rownames(scores) <- rownames(all$data)
   scores
}

plot.unique <- function(unique){
   box::use(ggplot2[...])
   data.frame(unique = unique, id = 1:length(unique)) |>
      ggplot(aes(x=id, y=unique)) +
         geom_bar(stat="identity", fill="white", color="black") +
         xlab("Test ID (sorted)") +
         ylab("Uniqueness") +
         ggtitle("Unique variance of MMLU subtests") +
         mytheme()
}



# =============================================================================
# prepare data
gprint("🚰 Loading preprocessed MMLU data...")
mmlu.files <- list.files(gpath("data"), pattern="mmlu_.*_preproc.rds", full.names=T)
scores.list <- lapply(mmlu.files, collect.scores)
scores <- Reduce(rowmerge, scores.list)

# exploratory factor analysis to determine unique subtests
cor(scores) |>
   corrplot::corrplot(method="color", type="upper", tl.pos="n", tl.cex=0.5, order="hclust")
fa.mmlu <- do.fa(scores, 1)
unique <- sort(fa.mmlu$uniquenesses, decreasing=T)
plot.unique(unique)
keepers <- names(unique[unique > 0.1])
