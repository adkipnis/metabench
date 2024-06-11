# Go beyond what single items measure by doing factor analysis on the test level.
# This makes single item thetas more interpretable (and potentially allows for even more reduction).
# usage: Rscript meta.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme, get.theta, do.fa, do.fa.cov])
Saveplots <- T
here::i_am("analysis/meta.R")
set.seed(1)


# =============================================================================
# helper functions

rowmerge <- function(df1, df2){
   merge(df1, df2, by="row.names") |>
     tibble::column_to_rownames("Row.names")
}

collect.data <- function(benchmark, train=T){
  datapath <- gpath("data/{benchmark}-preproc-split.rds")
  all <- readRDS(datapath)
  if (train) {
    data <- all$data.train
  } else {
    data <- all$data.test
  }
  data
}

subsample.data.score <- function(benchmark, train=T){
   n <- numitems.sub[[benchmark]]
   if (train){
     data <- data.full.train[[benchmark]]
   } else {
     data <- data.full.test[[benchmark]]
   }
   indices <- sample(1:ncol(data), n, replace = F)
   score.s <- data.frame(rowMeans(data[, indices]) * 100)
   colnames(score.s) <- benchmark
   score.s
}

collect.theta <- function(benchmark, train=T){
  model.type <- benchmarks[[benchmark]]$mod
  theta.type <- benchmarks[[benchmark]]$est
  suffix <- benchmarks[[benchmark]]$suffix
  fitpath <- gpath("analysis/models/{benchmark}-{model.type}-{suffix}-cv.rds")
  results <- readRDS(fitpath)
  model <- results$model
  if (train) {
    theta <- results$df |> dplyr::filter(set == "train") |>
      dplyr::select(matches("^F")) 
  } else {
    theta <- results$df |> dplyr::filter(set == "test") |>
      dplyr::select(matches("^F")) 
  }
  
  if (theta.type != "MAP") {
    datapath <- gpath("data/{benchmark}-sub.rds")
    all <- readRDS(datapath)
    if (train) {
      data <- all$data.train
    } else {
      data <- all$data.test
    }
    theta.new <- get.theta(model, theta.type, resp = data)
    theta.new <- as.data.frame(theta.new)
    rownames(theta.new) <- rownames(theta)
  }
  if (ncol(theta) == 2){
     colnames(theta) <- paste0(benchmark, c(".1", ".2"))
  } else {
     colnames(theta) <- benchmark
  }
  as.data.frame(theta)
}

collect.theta.reduced <- function(benchmark, train = T){
  model.type <- benchmarks[[benchmark]]$mod
  theta.type <- benchmarks[[benchmark]]$est
  lam <- benchmarks[[benchmark]]$lam
  fitpath <- gpath("analysis/reduced/{benchmark}-{model.type}-{theta.type}-{lam}.rds")
  results <- readRDS(fitpath)
  model <- results$model
  if (train){
    theta.train <- results$theta.train[, 1, drop=F]
    theta.test <- results$theta.test[, 1, drop=F]
    theta <- rbind(theta.train, theta.test)
  } else {
    theta <- results$theta.val[, 1, drop=F]
  }
  colnames(theta) <- benchmark
  theta
}

merge.skill <- function(skill.full){
   skill.reduced <- skill.full[[1]]
   for (i in 2:length(skill.full)){
      skill.reduced <- rowmerge(skill.reduced, skill.full[[i]])
   }
   skill.reduced
}

collect.scores <- function(benchmark, train = T){
   datapath <- gpath("data/{benchmark}-sub.rds")
   all <- readRDS(datapath)
   if (train){
     scores <- all$scores.train
     names <- rownames(all$data.train)
   } else {
     scores <- all$scores.test
     names <- rownames(all$data.test)
   }
   scores.norm <- scores/all$max.points.orig * 100
   scores <- data.frame(scores.norm)
   colnames(scores) <- benchmark
   rownames(scores) <- names
   scores
}

collect.numitems <- function(benchmark, type) {
   if (type == "original"){
      datapath <- gpath("data/{benchmark}-preproc-split.rds")
      all <- readRDS(datapath)
      numitems <- all$max.points.orig
   } else if (type == "preprocessed"){
     datapath <- gpath("data/{benchmark}-sub.rds")
     all <- readRDS(datapath)
     numitems <- ncol(all$data.train)
   } else if (type == "reduced") {
      model.type <- benchmarks[[benchmark]]$mod
      theta.type <- benchmarks[[benchmark]]$est
      lam <- benchmarks[[benchmark]]$lam
      fitpath <- gpath("analysis/reduced/{benchmark}-{model.type}-{theta.type}-{lam}.rds")
      results <- readRDS(fitpath)
      numitems <- nrow(results$items)
   }
   numitems
}

get.numitems <- function(benchmarks, type){
   fun <- function(b) collect.numitems(b, type)
   numitems <- lapply(names(benchmarks), fun)
   names(numitems) <- names(benchmarks)
   numitems <- as.data.frame(numitems)
   numitems$sum <- sum(numitems)
   numitems
}

construct.covmat <- function(thetas){
  # construct covariance matrix from list of thetas
  n <- length(thetas)
  covmat <- matrix(0, n, n)
  rownames(covmat) <- colnames(covmat) <- names(benchmarks)
  for (i in 1:n){
     for (j in i:n){
        df.cov <- rowmerge(thetas[[i]], thetas[[j]])
        covmat[i, j] <- cov(df.cov[,1], df.cov[,2])
     }
  }
  # make symmetric
  covmat <- covmat + t(covmat) - diag(diag(covmat))
  covmat
}

fit.score <- function(scores.partial, res.fa){
   pred.names <- colnames(scores.partial)[grepl("\\d$", colnames(scores.partial))]
   formula <- paste0("grand ~ ", paste0("s(", pred.names, ")", collapse=" + "))
   mgcv::gam(as.formula(formula), data = scores.partial)
}

evaluate.score.pred <- function(scores.partial){
   r.p <- cor(scores.partial$grand, scores.partial$p, method = "spearman")
   s <- scores.partial |>
      dplyr::mutate(error = grand - p) |>
      dplyr::summarize(mae = mean(abs(error)),
                       rmse = sqrt(mean(error^2)))
   plot.score.pred(scores.partial,
     text = glue::glue("RMSE = {format(round(s$rmse, digits=3), nsmall = 3)}\nr = {format(round(r.p, digits=3), nsmall = 3)}"))
}

plot.score.pred <- function(scores.partial, text = ""){
   box::use(ggplot2[...])
   ggplot(scores.partial, aes(x=grand, y=p, color=color)) +
      geom_abline(intercept=0, slope=1, linetype="dashed") +
      geom_point(alpha=0.5) +
      xlim(0,100) + ylim(0, 100) +
      annotate("text", x = 75, y = 25, label = text, size = 5) +
      labs(x="Score", y="Predicted") +
      mytheme()+
     papertheme()+
     theme(legend.position = "None")
}



# =============================================================================
# get ceiling for score prediction

# load scores
benchmarks <- list(arc=list(mod="3PL", est="EAPsum"),
                   gsm8k=list(mod="2PL", est="MAP"),
                   hellaswag=list(mod="3PL", est="MAP"),
                   mmlu=list(mod="4PL", est="EAPsum"),
                   truthfulqa=list(mod="2PL", est="EAPsum"),
                   winogrande=list(mod="3PL", est="EAPsum"))

scores.full.train <- lapply(names(benchmarks), collect.scores)
scores.full.test <- lapply(names(benchmarks), function(n) collect.scores(n, train = F))
scores.partial.train <- merge.skill(scores.full.train)
scores.partial.test <- merge.skill(scores.full.test)
numitems.orig <- get.numitems(benchmarks, "original")
numitems.sum <- numitems.orig$arc * (nrow(scores.full.train[[1]]) + nrow(scores.full.test[[1]])) +
   numitems.orig$gsm8k * (nrow(scores.full.train[[2]]) + nrow(scores.full.test[[2]])) +
   numitems.orig$hellaswag * (nrow(scores.full.train[[3]]) + nrow(scores.full.test[[3]])) +
   numitems.orig$mmlu * (nrow(scores.full.train[[4]]) + nrow(scores.full.test[[4]])) +
   numitems.orig$truthfulqa * (nrow(scores.full.train[[5]]) + nrow(scores.full.test[[5]])) +
   numitems.orig$winogrande * (nrow(scores.full.train[[6]]) + nrow(scores.full.test[[6]]))
  

# plot correlation matrix
cor(scores.partial.train) |>
  corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.score.1 <- do.fa(scores.partial.train, 1)
fa.score.2 <- do.fa(scores.partial.train, 2)
fa.score.3 <- do.fa(scores.partial.train, 3, verbose = F)
fa.score <- fa.score.2
psych::fa.diagram(fa.score)
fs.score.train <- psych::factor.scores(scores.partial.train, fa.score)
fs.score.test <- psych::factor.scores(scores.partial.test, fa.score)
sort(fa.score$uniquenesses, decreasing = T)

# check relation to grand sum
pred.score.train <- cbind(scores.partial.train, fs.score.train$scores)
pred.score.test <- cbind(scores.partial.test, fs.score.test$scores)
pred.score.train$grand <- rowMeans(scores.partial.train)
pred.score.test$grand <- rowMeans(scores.partial.test)
mod.score <- mgcv::gam(grand ~ s(gsm8k) + s(hellaswag),
                       data = pred.score.train)
pred.score.train$p <- predict(mod.score, pred.score.train)
pred.score.test$p <- predict(mod.score, pred.score.test)

n = numitems.orig$gsm8k + numitems.orig$hellaswag

pred.score.test$color = 1
p.base <- evaluate.score.pred(pred.score.test) +
  ggplot2::ggtitle(glue::glue(
    "(GSM8K + HellaSwag, n = {n})"))
p.base

r.score <- cor(pred.score.test$MR1, pred.score.test$grand)
gprint("r(Factor1, Score) = {round(r.score,3)}")

# # =============================================================================
# collect theta estimates and construct covariance matrix
benchmarks <- list(arc=list(mod="3PL", est="EAPsum", suffix = "1"),
                   gsm8k=list(mod="2PL", est="MAP", suffix = "1"),
                   hellaswag=list(mod="3PL", est="MAP", suffix = "1"),
                   mmlu=list(mod="4PL", est="EAPsum", suffix = "1"),
                   truthfulqa=list(mod="2PL", est="EAPsum", suffix = "1"),
                   winogrande=list(mod="3PL", est="EAPsum", suffix = "1"))

thetas.full.train <- lapply(names(benchmarks), collect.theta)
thetas.full.test <- lapply(names(benchmarks), function(n) collect.theta(n, train = F))
thetas.partial.train <- Reduce(rowmerge, thetas.full.train)
thetas.partial.test <- Reduce(rowmerge, thetas.full.test)
numitems.theta <- get.numitems(benchmarks, "preprocessed")

# plot correlation matrix
cor(thetas.partial.train) |>
   corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.theta.1 <- do.fa(thetas.partial.train, 1)
fa.theta.2 <- do.fa(thetas.partial.train, 2)
fa.theta <- fa.theta.1
psych::fa.diagram(fa.theta)
fa.theta$loadings
fs.theta.train <- psych::factor.scores(thetas.partial.train, fa.theta)
fs.theta.test <- psych::factor.scores(thetas.partial.test, fa.theta)
sort(fa.theta$uniquenesses, decreasing = T)

# check relation to grand sum
pred.theta.train <- cbind(thetas.partial.train, fs.theta.train$scores)
pred.theta.test <- cbind(thetas.partial.test, fs.theta.test$scores)
pred.theta.train$grand <- pred.score.train$grand
pred.theta.test$grand <- pred.score.test$grand
mod.theta <- mgcv::gam(grand ~ s(arc, bs="ad") + s(gsm8k, bs="ad") + s(gsm8k, bs="ad") + s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") + s(truthfulqa, bs="ad") + s(winogrande, bs="ad"),
                       data = pred.theta.train)
pred.theta.train$p <- predict(mod.theta)
pred.theta.test$p <- predict(mod.theta, pred.theta.test)

# evaluate grand sum prediction from factor scores
pred.theta.test$color <- runif(nrow(pred.theta.test))
p.full <- evaluate.score.pred(pred.theta.test) +
  ggplot2::scale_colour_gradientn(colours = cbPalette) +
  ggplot2::ggtitle(glue::glue("metabench (n = {numitems.theta$sum})"))
p.full

r.theta <- cor(pred.theta.test$MR1, pred.theta.test$grand)
gprint("r(Factor1, Score) = {round(r.theta,3)}")

# =============================================================================
benchmarks <- list(arc=list(mod="4PL", est="EAPsum", lam=0.01),
                        gsm8k=list(mod="4PL", est="EAPsum", lam=0.01),
                        hellaswag=list(mod="4PL", est="MAP", lam=0),
                        mmlu=list(mod="4PL", est="EAPsum", lam=0),
                        truthfulqa=list(mod="2PL", est="EAPsum", lam=0.015),
                        winogrande=list(mod="2PL", est="MAP", lam=0))
# collect theta estimates from reduced benchmarks
thetas.sub.full.train <- lapply(names(benchmarks), collect.theta.reduced)
thetas.sub.full.test <- lapply(names(benchmarks), function(n) collect.theta.reduced(n, train=F))
thetas.sub.partial.train <- merge.skill(thetas.sub.full.train)
thetas.sub.partial.test <- merge.skill(thetas.sub.full.test)
numitems.sub <- get.numitems(benchmarks, "reduced")
numitems.sub

# plot correlation matrix
cor(thetas.sub.partial.train)|>
   corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.sub <- do.fa(thetas.sub.partial.train, 1)
do.fa(thetas.sub.partial.train, 1)
fs.sub.train <- psych::factor.scores(thetas.sub.partial.train, fa.sub)
fs.sub.test <- psych::factor.scores(thetas.sub.partial.test, fa.sub)

# check relation to grand sum or other benchmarks
pred.sub.train <- cbind(thetas.sub.partial.train, fs.sub.train$scores)
pred.sub.test <- cbind(thetas.sub.partial.test, fs.sub.test$scores)

# pred.sub.train$grand <- pred.score.train$mmlu
# pred.sub.test$grand <- pred.score.test$mmlu
pred.sub.train$grand <- pred.score.train$grand
pred.sub.test$grand <- pred.score.test$grand
mod.sub <- mgcv::gam(grand ~ s(arc, bs="ad") + s(gsm8k, bs="ad") + s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") + s(truthfulqa, bs="ad") + s(winogrande, bs="ad"),
                       data = pred.sub.train)
# mod.theta <- fit.score(pred.theta.train, fa.theta)
pred.sub.train$p <- predict(mod.sub, pred.sub.train)
pred.sub.test$p <- predict(mod.sub, pred.sub.test)

# plot
pred.sub.test$color <- runif(nrow(pred.sub.test))
p.sub <- evaluate.score.pred(pred.sub.test) +
  ggplot2::scale_colour_gradientn(colours = cbPalette) +
  ggplot2::ggtitle(glue::glue("metabench (d = {numitems.sub$sum})"))
p.sub

r.sub <- cor(pred.sub.test$MR1, pred.sub.test$grand)
gprint("r(Factor1, Score) = {round(r.sub,3)}")

# cor(cbind(pred.sub.test$MR1, pred.theta.test$MR1, pred.score.test$MR1))
# saveRDS(p.sub, gpath("plots/meta-prediction.rds"))

# =============================================================================
# summary
p <- cowplot::plot_grid(p.full, p.sub,
                        ncol = 1, labels = c("B", "C"), align = "v")
ggplot2::ggsave(gpath("paper/figures/meta-prediction.pdf"), p, width = 4, height = 7)
saveRDS(list(p.full, p.sub), gpath("plots/meta-prediction.rds"))
# =============================================================================
# comparison to random subsampling
data.full.train <- lapply(names(benchmarks), collect.data)
data.full.test <- lapply(names(benchmarks), function(n) collect.data(n, train = F))
names(data.full.train) <- names(data.full.test) <- names(benchmarks)

rmses <- matrix(NA, nrow = 1000)
for (i in 1:1000){
  scores.train.r <- lapply(names(benchmarks), subsample.data.score)
  scores.test.r <- lapply(names(benchmarks), function(n) subsample.data.score(n, train = F))
  scores.train.r <- merge.skill(scores.train.r)
  scores.test.r <- merge.skill(scores.test.r)
  
  # check relation to grand sum
  scores.train.r$grand.r <- rowMeans(scores.train.r)
  scores.test.r$grand.r <- rowMeans(scores.test.r)
  scores.train.r$grand <- pred.score.train$grand
  scores.test.r$grand <- pred.score.test$grand
  mod.score.r <- mgcv::gam(grand ~ s(grand.r, bs="ad"),
                           data = scores.train.r)
  scores.train.r$p <- predict(mod.score.r, scores.train.r)
  scores.test.r$p <- predict(mod.score.r, scores.test.r)
  rmses[i] <- scores.test.r |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
}
saveRDS(list(rmses.test = rmses), gpath("data/meta-random-rmses.rds"))
