# item pre-selection (before IRT analysis)
# 1. do item analysis
# 2. perform hard exclusion 
# 3. if needed, perform soft exclusion based on rejection sampling

# =============================================================================
# load packages
packages <- c("tidyr", "dplyr", "tibble", "readr", "here", "glue")
install.packages(setdiff(packages, rownames(installed.packages())))
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
}

rejection.prob <- function(item, density) {
  x <- item$diff
  y <- item$disc
  z <-
    density$z[which.min(abs(density$x - x)), which.min(abs(density$y - y))]
  return(z)
}

rejection.sampling <- function(items) {
  density <- MASS::kde2d(items$diff, items$disc, n = 100)
  density$z <- density$z / max(density$z)
  items$reject <- 0
  for (i in 1:nrow(items)) {
    item <- items[i, ]
    items$reject[i] <- rejection.prob(item, density)
  }
  items$exclude <- items$reject > runif(nrow(items))
  items <- items[!items$exclude, ]
  return(items)
}

# =============================================================================
# prepare data
print(glue("Preprocessing for {BM}..."))
df <-
  read_csv(here::here(glue("data/{BM}.csv")), show_col_types = F)
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

# =============================================================================
# item analysis
items <- data.frame(item = colnames(data))
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
upper_bound <- 0.95 * (3 / 4)
items$exclude[items$diff < 0.05 | items$diff > upper_bound] <- T

# 2. item discrimination shouldn't be small
items$exclude[items$disc < 0.1] <- T

# pre-selection summary
n_excluded <- sum(items$exclude)
p_excluded <- round(n_excluded / nrow(items), 2)
n_remaining <- nrow(items) - n_excluded
print(glue(
  "Excluding {n_excluded} ({p_excluded}%) items, {n_remaining} remain..."
))

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
write_csv(data.sub, here::here(glue("data/{BM}_sub.csv")))

