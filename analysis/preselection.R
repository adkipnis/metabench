# item pre-selection (before IRT analysis)
# 1. do item analysis
# 2. perform hard exclusion 
# 3. if needed, perform soft exclusion based on rejection sampling

# =============================================================================
# load packages
packages <- c("tidyr", "dplyr", "tibble", "readr", "here", "glue")
invisible(suppressMessages(sapply(packages, require, character.only = T)))

# =============================================================================
# parse args
args <- commandArgs(trailingOnly = T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "hellaswag"
}

# =============================================================================
# path and seed
here::i_am("analysis/preselection.R")
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

rejection.sampling <- function(items) {
  density <- MASS::kde2d(items$diff, items$disc, n = 100)
  density$z <- density$z / max(density$z)
  items$reject <- rejection.prob(items, density)
  items$exclude <- items$reject > runif(nrow(items))
  items <- items[!items$exclude, ]
  return(items)
}

# =============================================================================
# prepare data
print(glue("Preprocessing for {BM}..."))
df <- read_csv(here::here(glue("data/{BM}.csv")), show_col_types = F)
data <- df %>%
  mutate(correct = as.integer(correct)) %>%
  pivot_wider(names_from = item, values_from = correct) %>%
  column_to_rownames(var = "source")
n_missing <- sum(is.na(data))
if (n_missing > 0) {
  print(glue("Warning: {n_missing} missing values in data, aborting..."))
  stop()
} else {
  print("No missing data. :)")
}
rm(df)

items <- read_csv(here::here(glue("data/{BM}_prompts.csv")), show_col_types = F)
print(glue("Item indices match prompts: {all(colnames(data) == items$item)}"))

# =============================================================================
# item analysis
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

# 2. item discrimination shouldn't be small
items$exclude[items$disc < 0.1] <- T

# pre-selection summary
n_excluded <- sum(items$exclude)
p_excluded <- round(n_excluded / nrow(items), 2)
n_remaining <- nrow(items) - n_excluded
print(glue("Excluding {n_excluded} ({p_excluded}%) items,
           {n_remaining} remain..."))

# plots (after)
items.sub <- items[!items$exclude, ]
plot.items(items.sub)

# optionally do rejection sampling for item pre-selection
while (nrow(items.sub) > 1500) {
  items.sub <- rejection.sampling(items.sub)
  plot.items(items.sub)
}

# reduce data and save
data.sub <- data[, items.sub$item]
out <- list(items = items.sub, data = data.sub)
saveRDS(out, here::here(glue("data/{BM}_selected.rds")))

