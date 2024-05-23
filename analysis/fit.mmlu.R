# Jointly fit MMLU single model on all subtests 
# 1. Perform exploratory FA on scores and discard non-unique subtests
# 2. Fit IRT model on remaining subtests
# usage: Rscript fit.mmlu.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, df2data, rowmerge, do.fa, mytheme])
here::i_am("analysis/fit.mmlu.R")
set.seed(1)

# =============================================================================
# helper functions

collect.data <- function(datapath){
  df <- readr::read_csv(datapath, show_col_types = F)
  data <- df2data(df)
  benchmark <- gsub("mmlu_", "", gsub(".csv", "", basename(datapath)))
  colnames(data) <- paste0(benchmark, ".", colnames(data))
  data
}

collect.scores <- function(dataset){
  scores <- data.frame(rowSums(dataset))
  colnames(scores) <- gsub("\\..*", "", colnames(dataset)[1])
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
gprint("🚰 Loading  MMLU data...")
mmlu.files <- list.files(gpath("data"), pattern="mmlu_.*csv", full.names=T)
mmlu.files <- mmlu.files[!grepl("prompts", mmlu.files)]
mmlu.names <- gsub("mmlu_", "", gsub(".csv", "", basename(mmlu.files)))
data.list <- lapply(mmlu.files, collect.data)
names(data.list) <- mmlu.names
scores <- Reduce(rowmerge, lapply(data.list, collect.scores))

# exploratory factor analysis to determine unique subtests
cor(scores) |>
   corrplot::corrplot(method="color", type="upper", tl.pos="n", tl.cex=0.5, order="hclust")
fa.mmlu <- do.fa(scores, 1)
unique <- sort(fa.mmlu$uniquenesses, decreasing=T)
plot.unique(unique)
keepers <- names(unique[unique > 0.1])
