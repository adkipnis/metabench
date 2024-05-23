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
           defaults = c("hellaswag"),
           legal = list(
             BM = c("arc", "gsm8k", "hellaswag", "mmlu_sub", "truthfulqa", "winogrande")
            )
)
here::i_am("analysis/preprocess.R")
mkdir(gpath("plots"))
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

plot.items <- function(items, den = T, outpath = NULL) {
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

   # position three plots in a single column
   p <- plot_grid(hist1, hist2, scatter, ncol = 1)

   # save or print
   if (!is.null(outpath)) {
      ggsave(outpath, p, width = 8, height = 8)
   } else {
      print(p)
   }
}

rejection.prob <- function(items, pmf) {
   x.values <- findInterval(items$diff, pmf$x)
   pmf$y[x.values] * (1 - items$disc) # reduce probability for high discrimination
}

rejection.sampling <- function(items, max_reject = 100) {
  # create pmf for equally spaced bins
  pmf <- density(items$diff, from = 0, to = 1, n = 100)
  pmf$y <- pmf$y / max(pmf$y)
  items$reject <- rejection.prob(items, pmf)
  items$exclude <- items$reject > runif(nrow(items))
  # if too many items are rejected, keep the rest
  if (sum(items$exclude) > max_reject) {
     n_too_many <- sum(items$exclude) - max_reject
     indices <- sample(which(items$exclude), n_too_many)
     items$exclude[indices] <- F
  }
  items <- items[!items$exclude, ]
  return(items)
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
if (BM == "mmlu_sub"){
  datapath <- gpath("data/{BM}.rds")
  all <- readRDS(datapath)
  data <- all$data
  items <- all$prompts
  scores <- rowSums(all$scores)
} else {
  df <- readr::read_csv(gpath("data/{BM}.csv"), show_col_types = F)
  data <- df2data(df)
  rm(df)
  items <- readr::read_csv(gpath("data/{BM}_prompts.csv"), show_col_types = F) 
  scores <- rowSums(data)
}

# check if data and items conform
if (!all(colnames(data) == items$item)){
   stop("❌ Item indices don't match prompts aborting.")
}

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
items$disc <- get.item.discrimination(data, d = items$diff)
plot.items(items, den = F, outpath = gpath("plots/pp_{BM}_0.png"))

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
plot.items(items.sub, outpath = gpath("plots/pp_{BM}_1.png"))

# optionally do rejection sampling for item pre-selection
n_max <- nrow(data)/4 # aspire an item to subject ratio of at max 1:4
if (n_remaining > n_max) gprint("2️⃣  Starting rejection sampling...")
while (nrow(items.sub) > n_max) {
  items.sub <- rejection.sampling(items.sub)
  plot.items(items.sub, outpath = gpath("plots/pp_{BM}_2.png"))
}

# reduce data and save
data.sub <- data[, items.sub$item]
gprint("🏁 Reduced dataset to {nrow(items.sub)} items.")
scores <- rowSums(data)
scores.norm <- scores / ncol(data)
out <- list(items = items.sub, data = data.sub,
            scores = scores, scores.norm = scores.norm)
outpath <- gpath("data/{BM}_preproc.rds")
saveRDS(out, outpath)
gprint("💾 Saved to '{outpath}'.")
