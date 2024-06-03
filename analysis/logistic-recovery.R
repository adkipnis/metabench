# Score recovery without IRT?

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(
  names = c("BM" ),
  defaults = c("hellaswag"),
  legal = list(
    BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
  )
)
here::i_am("analysis/logistic-recovery.R")
set.seed(1)

# =============================================================================
# helper functions  

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

# estimate logistic regression models for each item
scores.train.norm <- (scores.train - mean(scores.train))
item.params <- train.glms(scores.train.norm, data.train)

# get max aposteriori estimates of scores
prior <- density(scores.train.norm, n = 512)
loglikelihoods <- matrix(0, nrow(data.train), 512)
for (i in 1:ncol(data.train)){
  ll <- function(theta) get.log.likelihood(theta, item.params[i,], data.train[,i])
  loglikelihoods <- loglikelihoods + ll(prior$x) 
}
logposterior <- loglikelihoods + log(prior$y)
argmax <- apply(logposterior, 1, which.max)
theta.rec <- prior$x[argmax]
plot(scores.train.norm + mean(scores.train), theta.rec+ mean(scores.train))
abline(0, 1, col="red")
(sqrt(mean((scores.train.norm - theta.rec)^2)))
