packages <-
  c("tidyr", "dplyr", "tibble", "readr", "ggplot2", "mirt", "here", "caret")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = T)

# set benchmark (first arg when calling this file with Rscript)
BM <- 'gsm8k'

# options
here::i_am("analysis/fit.R")
set.seed(1)
TOL <- 1e-4

# prepare data
#df <- read_csv(paste0("~/Documents/data/", BM, ".csv"))
df <- read_csv(here::here(paste0("data/", BM, ".csv")))
data <- df %>%
  mutate(correct = as.integer(correct)) %>%
  pivot_wider(names_from = item, values_from = correct) %>%
  column_to_rownames(var = "source")
(sum(is.na(data)))

# remove outliers
data <- data[!(rowSums(data) < 30), ] # remove tail outliers

# sample 100 items for prototyping
data <- data[, sample(1:ncol(data), 100)]

# df scores
scores <- rowSums(data)
df.score <- data.frame(score=scores) %>%
  mutate(rank.score = rank(score), perc.score = rank.score/max(rank.score))

# 10-fold CV split (stratified wrt. scores)
folds <- createFolds(scores, k = 10, list = T)


# helper functions =============================================================
fit.model <- function(train, itemtype) {
  out <- mirt(
    train,
    1,
    itemtype = itemtype,
    method = 'EM',
    density = 'Davidian-4',
    TOL = TOL,
    technical = list(NCYCLES = 2000)
  )
  return(out)
}

get.theta <- function(model, resp = NULL){
   theta <- tryCatch(
     fscores(model,
             method = 'EAPsum',
             use_dentype_estimate = T,
             response.pattern = resp),
     error = function(e){
       fscores(model,
               method = 'MAP',
               use_dentype_estimate = F,
               response.pattern = resp)
     }
   )
   return(theta)
}

subset.score <- function(df.score, indices, theta){
   df <- df.score[indices,]
   df$theta <- theta[,1]
   df <- df %>% mutate(rank.theta = rank(theta),
                       perc.theta = rank.theta/max(rank.theta))
   return(df)
}

eval.model <- function(df.score, set = 'training') {
   mod.score = mgcv::gam(score ~ s(theta), data = df.score)
   df.score$p <- predict(mod.score)
   df.score <- df.score %>% arrange(by=theta)
   p <- ggplot(df.score, aes(x=theta, y=score)) +
     geom_point() +
     geom_line(aes(y=p), color='red') +
     labs(x=expression(theta), y='Score') +
     ggtitle(paste0('Score recovery (', set, ' set)'))
   error <- df.score %>%
     summarize(mae = mean(abs(score-p)),
               sd = sd(abs(score-p))
               )
   return(list(error=error, plot=p))
}

cv.fold <- function(fold, itemtype) {
  # fit model
  train <- data[-fold,]
  test <- data[fold,]
  model <- fit.model(train, itemtype)
  
  # train performance 
  theta <- get.theta(model)
  df.train <- subset.score(df.score, -fold, theta)
  cor(df.train$theta, df.train$score, method = 'spearman')
  train.fit <- eval.model(df.train, 'training')
  
  # test performance
  theta.unseen <- get.theta(model, test)
  df.test <- subset.score(df.score, fold, theta.unseen)
  cor(df.test$theta, df.test$score, method = 'spearman')
  test.fit <- eval.model(df.test, 'test')
  
  # summary
  out = list(model=model, train=df.train, test=df.test,
             train.fit=train.fit, test.fit=test.fit)
  return(out)
}

cv.wrapper <- function(folds, itemtype) {
  results <- list()
  i <- 1
  for (f in folds) {
    result <- cv.fold(f, itemtype)
    results[[i]] <- result
    i <- i + 1
  }
  return(results)
}

# =============================================================================
# 2PL Model
modpath <- here::here(paste0("analysis/models/", BM, "-2PL-cv.rds"))
results <- cv.wrapper(folds, '2PL')
saveRDS(results, file=modpath) 
