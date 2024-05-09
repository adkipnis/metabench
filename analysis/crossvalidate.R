# =============================================================================
# load packages
packages <-
  c("tidyr",
    "dplyr",
    "tibble",
    "readr",
    "ggplot2",
    "mirt",
    "here",
    "glue",
    "caret")
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(sapply(packages, require, character.only = T))

# =============================================================================
# parse args

# set benchmark
args <- commandArgs(trailingOnly = T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "arc"
} else if (!BM %in% c('arc', 'gsm8k', 'hellaswag', 'truthfulqa', 'winogrande')) {
  stop("Invalid benchmark option.")
}

# set model
args <- commandArgs(trailingOnly = T)
Model <- args[2]
if (is.na(Model)) {
  Model <- "2PL"
} else if (!Model %in% c('2PL', '3PL', '3PLu', '4PL')) {
  stop("Invalid model option.")
}

# set theta estimator
Method <- args[3]
if (is.na(Method)) {
  Method <- "MAP"
} else if (!Method %in% c('EAP', 'MAP', 'ML', 'WLE', 'EAPsum', 'plausible', 'classify')) {
  stop("Invalid theta estimation method.")
}

print(glue("Benchmark: {BM}, IRT Model: {Model}, Theta Estimation Method: {Method}"))

# =============================================================================
# path and seed
here::i_am("analysis/cv.R")
if (!dir.exists(here::here("analysis/models"))) {
  dir.create(here::here("analysis/models"))
}
set.seed(1)

# =============================================================================
# helper functions  

fit.model <- function(train, itemtype) {
  out <- mirt(
    train,
    1,
    itemtype = itemtype,
    method = 'EM',
    density = 'Davidian-4',
    TOL = 1e-4,
    technical = list(NCYCLES = 1000)
  )
  return(out)
}


get.theta <- function(model, resp = NULL) {
  use_dentype_estimate <- Method %in% c('EAPsum', 'EAP')
  theta <- fscores(
    model,
    method = Method,
    use_dentype_estimate = use_dentype_estimate,
    response.pattern = resp
  )
  return(theta)
}


subset.score <- function(df.score, indices, theta) {
  df <- df.score[indices, ]
  df$theta <- theta[, 1]
  df <- df %>% 
    arange(by=theta) %>%
    mutate(rank.theta = rank(theta),
           perc.theta = rank.theta / max(rank.theta))
  return(df)
}

plot.prediction <- function(df.score, set) {
  p <- ggplot(df.score, aes(x = theta, y = score)) +
    geom_point() +
    geom_line(aes(y = p), color = 'red') +
    labs(x = expression(theta), y = 'Score') +
    ggtitle(glue("Score recovery ({set} set)"))
}


get.error <- function(df.score) {
  error <- df.score %>%
    summarize(mae = mean(abs(score - p)),
              sd = sd(abs(score - p)))
  return(error)
}


cv.fold <- function(fold, itemtype) {
  # prepare data
  train <- data[-fold, ]
  test <- data[fold, ]
  std.train <- apply(train, 2, sd)
  std.test <- apply(test, 2, sd)
  item.ids <- which(std.train > 0 & std.test > 0)
  print(glue("Removing {ncol(train) - length(item.ids)} items with zero variance."))
  train <- train[, item.ids]
  test <- test[, item.ids]
  
  # fit model
  model <- fit.model(train, itemtype)
  
  # train performance
  theta.train <- get.theta(model)
  df.train <- subset.score(df.score, -fold, theta.train)
  mod.score <- mgcv::gam(score ~ s(theta), data = df.train)
  df.train$p <- predict(mod.score)
  p.train <- plot.prediction(df.train, 'training')
  r.train <-
    cor(df.train$theta, df.train$score, method = 'spearman')
  eps.train <- get.error(df.train)
  
  # test performance
  theta.test <- get.theta(model, resp = test)
  df.test <- subset.score(df.score, fold, theta.test)
  df.test$p <- predict(mod.score, newdata = df.test)
  p.test <- plot.prediction(df.test, 'test')
  r.test <- cor(df.test$theta, df.test$score, method = 'spearman')
  eps.test <- get.error(df.test)
  
  # summary
  out <- list(
    model = model,
    train = list(
      theta = theta.train,
      df = df.train,
      plot = p.train,
      r = r.train,
      error = eps.train
    ),
    test = list(
      theta = theta.test,
      df = df.test,
      plot = p.test,
      r = r.test,
      error = eps.test
    )
  )
  return(out)
}

cv.wrapper <- function(folds, itemtype, save = F) {
  results <- list()
  i <- 0
  for (f in folds) {
    i <- i + 1
    print(glue("Fold {i}"))
    result <- cv.fold(f, itemtype)
    if (save) {
      modpath <-
        here::here(glue("analysis/models/{BM}-{Model}-cv-{i}.rds"))
      saveRDS(result, file = modpath)
    }
    results[[i]] <- result
  }
  return(results)
}

# =============================================================================
# prepare data
df <- read_csv(here::here(glue("data/{BM}.csv")), show_col_types = F)
data <- df %>%
  mutate(correct = as.integer(correct)) %>%
  pivot_wider(names_from = item, values_from = correct) %>%
  column_to_rownames(var = "source")
n_missing <- sum(is.na(data))
rm(df)

# remove outliers and items without variance
scores <- rowSums(data)
threshold <- as.numeric(quantile(scores, probs=c(0.001)))
n <- nrow(data)
data <- data[!(scores <= threshold),] # remove tail outliers
std <- apply(data, 2, sd)
m <- ncol(data)
data <- data[, std > 0]

# print summary
summary.str <- glue(
  "Prepared preprocessing for {BM}:\n",
  "{n_missing} missing values (check data if > 0)\n",
  "Removed {n - nrow(data)} tail outliers (lowest 0.1% of score, threshold: {threshold})\n",
  "Removed {m - ncol(data)} items without variance\n",
  "Nubmer of subjects: {nrow(data)}\n",
  "Number of items: {ncol(data)}\n"
  )
print(summary.str)

# df scores
scores <- rowSums(data) # after removing outliers
df.score <- data.frame(score = scores) %>%
  mutate(rank.score = rank(score),
         perc.score = rank.score / max(rank.score))

# 10-fold CV split (stratified wrt. scores)
folds <- createFolds(scores, k = 10, list = T)


# =============================================================================
# fit model
mirtCluster()
mirtCluster(remove=T)
modpath <- here::here(glue("analysis/models/{BM}-{Model}-cv.rds"))
results <- cv.wrapper(folds, Model)
saveRDS(results, file = modpath)

