# Score recovery without IRT?

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, cbPalette])
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
parse.args(
  names = c("BM" ),
  defaults = c("arc"),
  legal = list(
    BM = benchmarks
  )
)
here::i_am("analysis/logistic.R")
set.seed(1)

# =============================================================================
# helper functions  
cbp <- cbPalette()

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
  f <- 1 / (1 + exp(-inner))
  
  # Handle cases where f is close to 0 or 1
  log_f <- ifelse(f < 1e-16, -37.6, log(f))
  log_1_minus_f <- ifelse(f > 1 - 1e-16, -37.6, log(1 - f))
  ll_terms <- response %*% t(log_f) + (1 - response) %*% t(log_1_minus_f)

  # Apply log-sum-exp trick to reduce overflow
  # max.term <- apply(cbind(log_f, log_1_minus_f), 1, max)
  # ll_terms <- max.term + log(exp(ll_terms - max.term))
  ll_terms
}

plot.score <- function(df.plot, text = ""){
   color <- cbp[which(benchmarks == BM)]
   box::use(ggplot2[...])
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         geom_point(alpha = 0.4, color = color) +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         annotate("text", x = 75, y = 25, label = text, size = 5) +
         labs(
            title = BM,
            x = "Score",
            y = "Predicted",
            ) +
         mytheme()
}

# =============================================================================
# prepare data
gprint("🚰 Loading preprocessed {BM} data...")
datapath <- gpath("data/{BM}-preproc-split.rds")
# datapath <- gpath("data/{BM}-sub-350.rds")

preproc <- readRDS(datapath)
data.train <- preproc$data.train
data.test <- preproc$data.test
scores.train <- 100 * preproc$scores.train / preproc$max.points.orig
scores.test <- 100 * preproc$scores.test / preproc$max.points.orig

# estimate logistic regression models for each item
scores.train.norm <- scores.train
item.params <- train.glms(scores.train.norm, data.train)

# get max aposteriori estimates of scores
prior <- density(scores.train.norm, n = 1024, from = 0, to = 100)
loglikelihoods <- matrix(0, nrow(data.train), 1024)
for (i in 1:ncol(data.train)){
  ll <- function(theta) get.log.likelihood(theta, item.params[i,], data.train[,i])
  loglikelihoods <- loglikelihoods + ll(prior$x) 
}
logposterior <- loglikelihoods + log(prior$y)
argmax <- apply(logposterior, 1, which.max)
score.rec <- prior$x[argmax]
if (any(is.na(logposterior))){
  gprint("NAs produced, caution!")
}

# plot result
rmse <- sqrt(mean((scores.train.norm - score.rec)^2))
gprint("Reached an RMSE of {round(rmse, 3)}.")
df.plot <- data.frame(score = scores.train, p = score.rec)
p <- plot.score(df.plot, glue::glue("RMSE = {round(rmse, 3)}"))
p

# save
outpath <- gpath("plots/{BM}-logistic.rds")
saveRDS(p, outpath)
gprint("Saved the result to {outpath}")
