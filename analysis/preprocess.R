# item pre-selection (before IRT analysis)
# 1. remove outlier subjects
# 2. do item analysis
# 3. perform hard exclusion 
# 4. if needed, perform soft exclusion based on rejection sampling
# usage: Rscript preprocess.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, df2data])
box::use(./utils[parse.args, mkdir, gprint, gpath, df2data])
parse.args(names = c("BM"), defaults = c("hellaswag"))
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
  p <- n_correct * guess_coeff / n_total
  return(p)
}

get.item.discrimination <- function(data, d) {
  # item discrimination (point-biserial correlation)
  # m_c = mean score of the group that answered the item correctly
  # m = mean score of the entire group
  # sd = standard deviation of the entire group
  # d = item difficulty
  # rpbis = (m_c - m)/sd * sqrt(d * (1 - d))
  scores <- rowSums(data)
  n_correct <- colSums(data)
  m <- mean(scores)
  m_c <- colSums(data * scores) / n_correct
  s <- sd(scores)
  rpbis <- (m_c - m) / s * sqrt(d * (1 - d))
  return(rpbis)
}

plot.items <- function(items, den = T) {
  par(mfrow = c(3, 1))
  hist(items$diff,
       breaks = 100,
       main = "",
       xlab = "difficulty")
  hist(items$disc,
       breaks = 100,
       main = "",
       xlab = "discrimination")
  plot(
    disc ~ diff,
    data = items,
    xlab = "difficulty",
    ylab = "discrimination"
  )
  if (den) {
    density <- MASS::kde2d(items$diff, items$disc, n = 100)
    contour(density,
            add = T,
            col = "red",
            lwd = 2)
  }
  par(mfrow = c(1, 1))
}

rejection.prob <- function(items, density) {
   x_indices <- findInterval(items$diff, density$x)
   y_indices <- findInterval(items$disc, density$y)
   z <- density$z[cbind(x_indices, y_indices)]
   return(z)
}

rejection.sampling <- function(items, max_reject = 100) {
  density <- MASS::kde2d(items$diff, items$disc, n = 100)
  density$z <- density$z / max(density$z)
  items$reject <- rejection.prob(items, density)
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
df <- readr::read_csv(gpath("data/{BM}.csv"), show_col_types = F)
data <- df2data(df)
rm(df)

items <- readr::read_csv(gpath("data/{BM}_prompts.csv"), show_col_types = F)
if (!all(colnames(data) == items$item)){
   stop("❌ Item indices don't match prompts aborting.")
}

# =============================================================================
# outlier removal
scores <- rowSums(data)
threshold <- as.numeric(quantile(scores, probs=c(0.001)))
n <- nrow(data)
data <- data[!(scores <= threshold),]
gprint("🧹 Removed {n - nrow(data)} tail outliers (lowest 0.1% of score, threshold: {threshold}).")

# =============================================================================
# item analysis
gprint("⚙️  Starting item analysis...")
items$sd <- apply(data, 2, sd)
items$diff <- get.item.difficulty(data)
items$disc <- get.item.discrimination(data, d = items$diff)
plot.items(items, den = F)

# =============================================================================
# item pre-selection

# 0. item answers must vary
items$exclude <- F
items$exclude[items$sd <= 0.01] <- T

# 1. item difficulty shouldn't be extreme
guess_coeff <- 3/4
lower_bound <- 0.05 * guess_coeff
upper_bound <- 0.95 * guess_coeff
items$exclude[items$diff < lower_bound | items$diff > upper_bound] <- T

# 2. item discrimination shouldn't be negative
items$exclude[items$disc < 0] <- T

# pre-selection summary
n_excluded <- sum(items$exclude)
p_excluded <- round(100 * n_excluded / nrow(items), 2)
n_remaining <- nrow(items) - n_excluded
gprint("1️⃣  Excluding {p_excluded}% items, {n_remaining} remain...")

# plots (after)
items.sub <- items[!items$exclude, ]
plot.items(items.sub)

# optionally do rejection sampling for item pre-selection
n_max <- nrow(data)/4 # aspire an item to subject ratio of at max 1:4
if (n_remaining > n_max) gprint("2️⃣  Starting rejection sampling...")
while (nrow(items.sub) > n_max) {
  items.sub <- rejection.sampling(items.sub)
  plot.items(items.sub)
}

# reduce data and save
data.sub <- data[, items.sub$item]
gprint("🏁 Reduced dataset to {nrow(items.sub)} items.")
out <- list(items = items.sub, data = data.sub, scores = rowSums(data))
outpath <- gpath("data/{BM}_preproc.rds")
saveRDS(out, outpath)
gprint("💾 Saved to '{outpath}'.")
