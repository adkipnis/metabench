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
lapply(packages, require, character.only = T)

# set benchmark (first arg when calling this file with Rscript)
args <- commandArgs(trailingOnly = T)
BM <- args[1]
if (is.na(BM)) {
   BM <- "gsm8k"
}
glue("Benchmark: {BM}")

# options
here::i_am("analysis/fit.R")
if (!dir.exists(here::here("analysis/models"))) {
  dir.create(here::here("analysis/models"))
}
set.seed(1)
TOL <- 1e-4


# helper functions =============================================================
fit.model <- function(train, itemtype) {
  out <- mirt(
    train,
    1,
    itemtype = itemtype,
    method = 'EM',
    density = 'Davidian-4',
    TOL = TOL,
    # technical = list(NCYCLES = 1000)
  )
  return(out)
}


get.theta <- function(model, resp = NULL, map = F) {
  if (map) {
    theta <- fscores(
      model,
      method = 'MAP',
      use_dentype_estimate = F,
      response.pattern = resp
    )
  } else {
    theta <- fscores(
      model,
      method = 'EAPsum',
      use_dentype_estimate = T,
      response.pattern = resp
    )
  }
  return(theta)
}


subset.score <- function(df.score, indices, theta) {
  df <- df.score[indices, ]
  df$theta <- theta[, 1]
  df <- df %>% mutate(rank.theta = rank(theta),
                      perc.theta = rank.theta / max(rank.theta))
  return(df)
}


fit.score <- function(df.score) {
  mod.score = mgcv::gam(score ~ s(theta), data = df.score)
  df.score$p <- predict(mod.score)
  df.score <- df.score %>% arrange(by = theta)
  return(df.score)
}


plot.prediction <- function(df.score, set) {
  p <- ggplot(df.score, aes(x = theta, y = score)) +
    geom_point() +
    geom_line(aes(y = p), color = 'red') +
    labs(x = expression(theta), y = 'Score') +
    ggtitle(paste0('Score recovery (', set, ' set)'))
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
  train <- train[, item.ids]
  test <- test[, item.ids]
  
  # fit model
  model <- fit.model(train, itemtype)
  
  # train performance
  theta.train <- get.theta(model)
  df.train <- subset.score(df.score,-fold, theta.train)
  df.train <- fit.score(df.train)
  p.train <- plot.prediction(df.train, 'training')
  r.train <-
    cor(df.train$theta, df.train$score, method = 'spearman')
  eps.train <- get.error(df.train)
  
  # test performance
  theta.test <- get.theta(model, resp = test)
  df.test <- subset.score(df.score, fold, theta.test)
  df.test <- fit.score(df.test)
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



cv.wrapper <- function(folds, itemtype) {
  results <- list()
  i <- 0
  for (f in folds) {
    i <- i + 1
    glue("Fold {i}")
    modpath <- here::here(paste0("analysis/models/", BM, "-2PL-cv-", i, ".rds"))
    result <- cv.fold(f, itemtype)
    saveRDS(result, file = modpath)
    results[[i]] <- result
  }
  return(results)
}


# =============================================================================
# prepare data
df <- read_csv(here::here(paste0("data/", BM, ".csv")))
data <- df %>%
  mutate(correct = as.integer(correct)) %>%
  pivot_wider(names_from = item, values_from = correct) %>%
  column_to_rownames(var = "source")
glue("Number of missing values: {sum(is.na(data))}")

# remove outliers
data <- data[!(rowSums(data) < 30),] # remove tail outliers
# sample 100 items for prototyping
# data <- data[, sample(1:ncol(data), 100)]

# df scores
scores <- rowSums(data)
df.score <- data.frame(score = scores) %>%
  mutate(rank.score = rank(score),
         perc.score = rank.score / max(rank.score))

# 10-fold CV split (stratified wrt. scores)
folds <- createFolds(scores, k = 10, list = T)


# =============================================================================
# 2PL Model
modpath <- here::here(paste0("analysis/models/", BM, "-2PL-cv.rds"))
results <- cv.wrapper(folds, '2PL')
saveRDS(results, file = modpath)

