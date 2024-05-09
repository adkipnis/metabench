# item pre-selection (before IRT analysis)
# 1. remove outlier subjects
# 2. do item analysis
# 3. perform hard exclusion 
# 4. if needed, perform soft exclusion based on rejection sampling
# usage: Rscript preprocess.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, df2data])
parse.args(names = c("BM"), defaults = c("hellaswag"))
here::i_am("analysis/preprocess.R")
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

