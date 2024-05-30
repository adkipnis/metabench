# item pre-selection (before IRT analysis)
# 1. remove outlier subjects
# 2. do item analysis
# 3. perform hard exclusion 
# 4. if needed, perform soft exclusion based on rejection sampling
# usage: Rscript preprocess.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, df2data, mytheme])
parse.args(names = c("BM"),
           defaults = c("arc"),
           legal = list(
             BM = c("arc", "gsm8k", "hellaswag", "mmlu_sub", "truthfulqa", "winogrande")
            )
)
here::i_am("analysis/preprocess.R")
mkdir("plots")
set.seed(1)

# =============================================================================
# helper functions

get.item.difficulty <- function(data, n_options = 4) {
  # item difficulty (corrected for guessing)
  # guess_coeff = (n_options - 1) / n_options
  # p = n_correct * guess_coeff / n_total
  n_total <- nrow(data)
  n_correct <- colSums(data)
  guess_coeff <- (n_options - 1) / n_options
  n_correct * guess_coeff / n_total
}

get.item.discrimination <- function(data, scores) {
  # item discrimination (point-biserial correlation)
  # m1 = mean score of the group that answered the item correctly
  # m = mean score of the entire group
  # s = empirical standard deviation of the entire group score
  # n1, n0 = sizes of the groups who answered the item correctly resp. incorr.
  # n = total group size
  # rpbis = (m1 - m)/s * sqrt(n1 * n0 / (n*(n-1)))
  n1 <- colSums(data) # how many subjects answered this item correctly
  m <- mean(scores)
  m1 <- colSums(data * scores) / n1 # mean scores of correct subjects
  s <- sd(scores)
  n <- nrow(data)
  n0 <- n - n1
  (m1 - m) / s * sqrt(n1 * n0 / (n * (n-1)))
}

plot.items <- function(items, den = T) {
   box::use(ggplot2[...], cowplot[plot_grid])
   hist1 <- ggplot(items, aes(diff)) +
      geom_histogram(bins = 100, fill="lightgrey", color="black") +
      labs(x = "difficulty") +
      mytheme()
   hist2 <- ggplot(items, aes(disc)) +
      geom_histogram(bins = 100, fill="lightgrey", color="black") +
      labs(x = "discrimination") +
      mytheme()
   scatter <- ggplot(items, aes(diff, disc)) +
      geom_point() +
      labs(x = "difficulty", y = "discrimination") +
      mytheme()
   if (den) {
      scatter <- scatter +
         geom_density_2d(color = "red", linewidth = 1)
   }
   plot_grid(hist1, hist2, scatter, ncol = 1)
}


# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
if (BM == "mmlu_sub"){
  datapath <- gpath("data/{BM}.rds")
  all <- readRDS(datapath)
  data <- all$data
  items <- all$prompts
  items$item.orig <- items$item
  items$item <- colnames(data) <- 1:ncol(data)
  scores <- rowSums(all$scores)
  max.points.orig <- 7987L
} else {
  df <- readr::read_csv(gpath("data/{BM}.csv"), show_col_types = F)
  data <- df2data(df)
  rm(df)
  items <- readr::read_csv(gpath("data/{BM}_prompts.csv"), show_col_types = F) 
  scores <- rowSums(data)
  max.points.orig <- ncol(data)
}

# check if data and items conform
if (!all(colnames(data) == items$item)){
   stop("❌ Item indices don't match prompts aborting.")
}
gprint("# LLMs: {nrow(data)}, # items: {ncol(data)}.")

# =============================================================================
# outlier removal
threshold <- as.numeric(quantile(scores, probs=c(0.001)))
n <- nrow(data)
data <- data[!(scores <= threshold),]
scores <- scores[!(scores <= threshold)]
gprint("🧹 Removed {n - nrow(data)} tail outliers (lowest 0.1% of score, threshold: {threshold}).")

# =============================================================================
# item analysis
gprint("⚙️  Starting item analysis...")
items$sd <- apply(data, 2, sd)
items$diff <- get.item.difficulty(data)
items$disc <- get.item.discrimination(data, scores)
p.pre <- plot.items(items, den = F)

# =============================================================================
# item pre-selection

# 0. item answers must vary
items$exclude <- F
items$exclude[items$sd <= 0.01] <- T

# 1. items should not be too easy 
guess_coeff <- 3/4
upper_bound <- 0.95 * guess_coeff
items$exclude[items$diff > upper_bound] <- T

# 2. item discrimination shouldn't be negative
items$exclude[items$disc < 0] <- T

# pre-selection summary
n_excluded <- sum(items$exclude)
p_excluded <- round(100 * n_excluded / nrow(items), 2)
n_remaining <- nrow(items) - n_excluded
gprint("1️⃣  Excluding {p_excluded}% items, {n_remaining} remain...")

# plots (after)
items.sub <- items[!items$exclude, ]
p.post <- plot.items(items.sub)
p <- cowplot::plot_grid(p.pre, p.post, ncol = 2, labels = "AUTO")
ggplot2::ggsave(gpath("plots/{BM}-preproc.png"), p, width = 10, height = 5)

# reduce data and save
data.sub <- data[, items.sub$item]
gprint("🏁 Reduced dataset to {nrow(items.sub)} items.")
out <- list(items = items.sub,
            data = data.sub,
            scores.orig = scores,
            max.points.orig = max.points.orig 
)
outpath <- gpath("data/{BM}-preproc.rds")
saveRDS(out, outpath)
gprint("💾 Saved to '{outpath}'.")
