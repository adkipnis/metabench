# cross-validated IRT fitting of preprocessed data
# goal: determine the best IRT model for the given benchmark
# usage: Rscript crossvalidate.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(
   names = c("BM", "MOD"),
   defaults = c("hellaswag", "3PL"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     MOD = c("2PL", "3PL")
   )
)
here::i_am("analysis/crossvalidate.R")
mkdir("analysis/models")
set.seed(1)

# =============================================================================
# helper functions  

make.df.score <- function(scores, theta) {
   data.frame(score = scores, theta = theta[,1]) |>
      dplyr::mutate(rank.score = rank(score),
                    perc.score = rank.score / max(rank.score),
                    rank.theta = rank(theta),
                    perc.theta = rank.theta / max(rank.theta))
}

glm.train <- function(normalized.scores, data.column){
  df.log <- data.frame(y = data.column, x = normalized.scores)
  mod <- glm(y ~ x, data = df.log, family = binomial(link = "logit"))
  mod$coefficients
}

train.glms <- function(scores.train.norm, data.train){
   params <- list()
   for (i in colnames(data.train)){
      params[[i]] <- glm.train(scores.train.norm, data.train[,i])
   }
   do.call(rbind, params)
}

get.log.likelihood <- function(thetas, params, response){
  a <- params[2]
  b <- params[1]
  inner <- thetas * a + b
  prob <- 1 / (1 + exp(-inner))
  response %*% t(log(prob)) + (1 - response) %*% t(log(1 - prob))
}


cross.validate <- function(itemtype){
  # fit model
  gprint("⚙️ Fitting {itemtype} model to training fold...")
  model <- run.mirt(data.train, itemtype)
 
  # train performance
  theta.train <- get.theta(model, method = "MAP")
  df.train <- make.df.score(scores.train, theta.train)
  mod.score <- mgcv::gam(score ~ s(theta), data = df.train)
  df.train$p <- predict(mod.score)
  
  # test performance
  theta.test <- get.theta(model, method = "MAP", resp = data.test)
  df.test <- make.df.score(scores.test, theta.test)
  df.test$p <- predict(mod.score, newdata = df.test)

  # collaps both dataframes
  df.train$set <- "train"
  df.test$set <- "test"
  list(df = rbind(df.train, df.test), model = model, mod.score = mod.score)
}


# =============================================================================
# prepare data
gprint("🚰 Loading preprocessed {BM} data...")
if (BM %in% c("hellaswag", "mmlu")){
   datapath <- gpath("data/{BM}-sub.rds")
} else {
   datapath <- gpath("data/{BM}-preproc-split.rds")
}
preproc <- readRDS(datapath)
data.train <- preproc$data.train
data.test <- preproc$data.test
scores.train <- 100 * preproc$scores.train / preproc$max.points.orig
scores.test <- 100 * preproc$scores.test / preproc$max.points.orig

# theta recovery
scores.train.norm <- (scores.train - mean(scores.train))
item.params <- train.glms(scores.train.norm, data.train)
prior <- density(scores.train.norm, n = 512)
loglikelihoods <- matrix(0, nrow(data.train), 512)

for (i in 1:ncol(data.train)){
  ll <- function(theta) get.log.likelihood(theta, item.params[i,], data.train[,i])
  loglikelihoods <- loglikelihoods + ll(prior$x) 
}
logposterior <- loglikelihoods + log(prior$y)
plot(prior$x, logposterior[19,])
argmax <- apply(logposterior, 1, which.max)
theta.rec <- prior$x[argmax]
plot(scores.train.norm, theta.rec)
abline(0, 1)
sqrt(mean((scores.train.norm - theta.rec)^2))

# =============================================================================
# cv models
cv <- cross.validate(MOD)
outpath <- gpath("analysis/models/{BM}-{MOD}-cv.rds")
saveRDS(cv, outpath)
gprint("💾 Saved to '{outpath}'.")
