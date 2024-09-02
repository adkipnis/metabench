# Go beyond what single items measure by doing factor analysis on the test level.
# This makes single item thetas more interpretable (and potentially allows for even more reduction).
# usage: Rscript meta.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme, cbPalette, get.theta, do.fa, do.fa.cov])
Saveplots <- T
here::i_am("analysis/meta.R")
set.seed(1)
mkdir("analysis/gams")
benchmark.names <- c("ARC", "GSM8K", "HellaSwag", "MMLU", "TruthfulQA", "WinoGrande")

# =============================================================================
# helper functions
napply <- function(some.names, some.func){
  out <- lapply(some.names, some.func)
  setNames(out, some.names)
}

cormat2vec <- function(y) {
  x <- cor(y)
  unique(x[col(x) != row(x)])
}


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

subsample.data.score <- function(benchmark, seed, source = "full"){
   if (source == "full"){
     n <- numitems.theta[[benchmark]]
   } else if (source == "sub"){
     n <- numitems.sub[[benchmark]]
   } else if (source == "100"){
     n <- numitems.100[[benchmark]]
   } else {
     print("Unkown source provided, must be 'full' or 'sub'")
     return()
   }
   data <- data.full.train[[benchmark]]
   set.seed(seed)
   sample(1:ncol(data), n, replace = F)
}

partition.counts <- function(n = 100){
  counts <- sample(names(benchmarks), n, replace = T)
  numitems.n <- table(counts)
  numitems.n <- as.data.frame(rbind(numitems.n))
  numitems.n$sum <- n
  rownames(numitems.n) <- NULL
  numitems.n
}

subscores <- function(name, index.list){
  data.train <- data.full.train[[name]]
  data.test <- data.full.test[[name]]
  indices <- index.list[[name]]
  data.train.r <- data.train[,indices]
  data.test.r <- data.test[,indices]
  scores.train.r <- data.frame(rowMeans(data.train.r) * 100)
  scores.test.r <- data.frame(rowMeans(data.test.r) * 100)
  colnames(scores.test.r) <- colnames(scores.train.r) <- name
  list(train = scores.train.r, test = scores.test.r)
}

subsample.wrapper <- function(seed, source){
  subsampler <- function(x) subsample.data.score(x, seed, source)
  index.list <- napply(names(benchmarks), subsampler)
  scores.r <- napply(names(benchmarks), function(n) subscores(n, index.list))
  scores.train.r <- napply(names(benchmarks), function(n) scores.r[[n]]$train)
  scores.train.r <- Reduce(rowmerge, scores.train.r)
  scores.test.r <- napply(names(benchmarks), function(n) scores.r[[n]]$test)
  scores.test.r <- Reduce(rowmerge, scores.test.r)
  
  # check relation to grand sum
  scores.train.r$grand.r <- rowMeans(scores.train.r)
  scores.test.r$grand.r <- rowMeans(scores.test.r)
  scores.train.r$grand <- pred.score.train$grand
  scores.test.r$grand <- pred.score.test$grand
  mod.score.r <- mgcv::gam(grand ~ s(grand.r, bs="ps"),
                           data = scores.train.r)
  scores.train.r$p <- predict(mod.score.r, scores.train.r)
  scores.test.r$p <- predict(mod.score.r, scores.test.r)
  scores.test.r |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
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
    datapath <- gpath("data/{benchmark}-sub-350.rds")
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
    theta.val <- results$theta.val[, 1, drop=F]
    theta <- rbind(theta.train, theta.val)
  } else {
    theta <- results$theta.test[, 1, drop=F]
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
   datapath <- gpath("data/{benchmark}-sub-350.rds")
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
     datapath <- gpath("data/{benchmark}-sub-350.rds")
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

fit.score <- function(scores.partial, res.fa){
   pred.names <- colnames(scores.partial)[grepl("\\d$", colnames(scores.partial))]
   formula <- paste0("grand ~ ", paste0("s(", pred.names, ", bs = 'ad')", collapse=" + "))
   mgcv::gam(as.formula(formula), data = scores.partial)
}

evaluate.score.pred <- function(scores.partial){
   r.p <- cor(scores.partial$grand, scores.partial$p, method = "spearman")
   s <- scores.partial |>
      dplyr::mutate(error = grand - p) |>
      dplyr::summarize(mae = mean(abs(error)),
                       rmse = sqrt(mean(error^2)))
   plot.score.pred(scores.partial,
     text = glue::glue("RMSE = {format(round(s$rmse, digits=3), nsmall = 3)}
                       r = {format(round(r.p, digits=3), nsmall = 3)}"))
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
     theme(legend.position = "None")
}

plot.corrmat <- function(scores.partial){
  scores.partial |>
    setNames(benchmark.names) |>
    cor() |>
    corrplot::corrplot(method="color",
                       cl.pos = 'b', col.lim = c(0, 1),
                       addgrid.col = 'white', addCoef.col = 'grey50',
                       tl.cex=0.8, tl.col="black", tl.pos='d')
}

load.reduced <- function(bm){
   mod <- benchmarks[[bm]]$mod
   est <- benchmarks[[bm]]$est
   lam <- benchmarks[[bm]]$lam
   path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}.rds")
   readRDS(path)$items$item
}

# =============================================================================
# 0. Leaderboard
leaderboard <- read.csv(gpath("scraping/open-llm-leaderboard.csv"))
categories <- c("unknown", "LlamaForCausalLM", "MistralForCausalLM",
                "MixtralForCausalLM")
leaderboard$arch[leaderboard$arch == "error"] <- "unknown"
leaderboard$arch[!leaderboard$arch %in% categories] <- "other"
rownames(leaderboard) <- leaderboard$name
leaderboard <- leaderboard |> dplyr::select(nparams, arch)

# =============================================================================
# 1. Point scores
# load scores
benchmarks <- list(arc=list(mod="3PL", est="EAPsum", suffix = "1"),
                   gsm8k=list(mod="2PL", est="EAPsum", suffix = "1"),
                   hellaswag=list(mod="3PL", est="MAP", suffix = "1"),
                   mmlu=list(mod="4PL", est="EAPsum", suffix = "1"),
                   truthfulqa=list(mod="2PL", est="EAPsum", suffix = "1"),
                   winogrande=list(mod="3PL", est="EAPsum", suffix = "1"))

scores.full.train <- lapply(names(benchmarks), collect.scores)
scores.full.test <- lapply(names(benchmarks), function(n) collect.scores(n, train = F))
scores.partial.train <- merge.skill(scores.full.train)
scores.partial.test <- merge.skill(scores.full.test)
numitems.orig <- get.numitems(benchmarks, "original")

# plot correlation matrix
pdf(file = gpath("plots/corrmat.scores.pdf"))
plot.corrmat(scores.partial.train)
dev.off()

# exploratory factor analysis
fa.score.1 <- do.fa(scores.partial.train, 1)
fa.score.2 <- do.fa(scores.partial.train, 2)
fa.score.3 <- do.fa(scores.partial.train, 3, verbose = F) # this throws an exception
fa.score <- fa.score.1
psych::fa.diagram(fa.score)
sort(fa.score$uniquenesses, decreasing = T)

# check recovery using fewer full benchmarks
fs.score.train <- psych::factor.scores(scores.partial.train, fa.score)
fs.score.test <- psych::factor.scores(scores.partial.test, fa.score)
pred.score.train <- cbind(scores.partial.train, fs.score.train$scores)
pred.score.test <- cbind(scores.partial.test, fs.score.test$scores)
pred.score.train$grand <- rowMeans(scores.partial.train)
pred.score.test$grand <- rowMeans(scores.partial.test)
mod.score <- mgcv::gam(grand ~ s(gsm8k, bs = "ad") + s(hellaswag, bs = "ad"),
                       data = pred.score.train)
pred.score.train$p <- predict(mod.score, pred.score.train)
pred.score.test$p <- predict(mod.score, pred.score.test)
n = numitems.orig$gsm8k + numitems.orig$hellaswag
pred.score.test$color = 1
p.base <- evaluate.score.pred(pred.score.test) +
  ggplot2::ggtitle(glue::glue(
    "(GSM8K + HellaSwag, n = {n})"))
p.base

# correlation between first factor and grand score
r.score <- cor(pred.score.test$MR1, pred.score.test$grand, method = "spearman")
gprint("r(Factor1, Score) = {round(r.score,3)}")


# =============================================================================
# 2. Latent Abilities (350 items)
# collect theta estimates and construct covariance matrix
thetas.full.train <- lapply(names(benchmarks), collect.theta)
thetas.full.test <- lapply(names(benchmarks), function(n) collect.theta(n, train = F))
thetas.partial.train <- Reduce(rowmerge, thetas.full.train)
thetas.partial.test <- Reduce(rowmerge, thetas.full.test)
numitems.theta <- get.numitems(benchmarks, "preprocessed")

cor(cormat2vec(scores.partial.train), cormat2vec(thetas.partial.train))


# plot correlation matrix
pdf(file = gpath("plots/corrmat.thetas-f.pdf"))
plot.corrmat(thetas.partial.train)
dev.off()

# exploratory factor analysis
fa.theta.1 <- do.fa(thetas.partial.train, 1)
fa.theta.2 <- do.fa(thetas.partial.train, 2)
fa.theta <- fa.theta.1 # additional explained variance of second factor is slim
psych::fa.diagram(fa.theta)
fa.theta$loadings
sort(fa.theta$uniquenesses, decreasing = T)

# check relation to grand sum
fs.theta.train <- psych::factor.scores(thetas.partial.train, fa.theta)
fs.theta.test <- psych::factor.scores(thetas.partial.test, fa.theta)
pred.theta.train <- cbind(thetas.partial.train, fs.theta.train$scores)
pred.theta.test <- cbind(thetas.partial.test, fs.theta.test$scores)
pred.theta.train$grand <- pred.score.train$grand
pred.theta.test$grand <- pred.score.test$grand
mod.theta <- mgcv::gam(grand ~
                         s(arc, bs="ad") +
                         s(gsm8k, bs="ad") +
                         s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") +
                         s(truthfulqa, bs="ad") +
                         s(winogrande, bs="ad"),
                       data = pred.theta.train)
pred.theta.train$p <- predict(mod.theta)
pred.theta.test$p <- predict(mod.theta, pred.theta.test)

# evaluate grand sum prediction from factor scores
pred.theta.test$color <- runif(nrow(pred.theta.test))
p.full <- evaluate.score.pred(pred.theta.test) +
  ggplot2::scale_colour_gradientn(colours = cbPalette()) +
  ggplot2::ggtitle(glue::glue("metabench (n = {numitems.theta$sum})"))
p.full
saveRDS(p.full, gpath("plots/metabench-full.rds"))

# correlation between first factor and grand score
r.theta <- cor(pred.theta.test$MR1, pred.theta.test$grand, method = "spearman")
gprint("r(Factor1, Score) = {round(r.theta,3)}")

# =============================================================================
# 3. Latent Abilities (subsets)
# benchmarks <- list(
#   arc = list(mod = "2PL", est = "MAP", lam = 0.001),
#   gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
#   hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
#   mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
#   truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
#   winogrande = list(mod = "4PL", est = "MAP", lam = 0.001)
# )
benchmarks <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
  winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
)

# collect theta estimates from reduced benchmarks
thetas.sub.full.train <- lapply(names(benchmarks), collect.theta.reduced)
thetas.sub.full.test <- lapply(names(benchmarks), function(n) collect.theta.reduced(n, train=F))
thetas.sub.partial.train <- Reduce(rowmerge, thetas.sub.full.train)
thetas.sub.partial.test <- Reduce(rowmerge, thetas.sub.full.test)
numitems.sub <- get.numitems(benchmarks, "reduced")
numitems.sub

cor(cormat2vec(scores.partial.train), cormat2vec(thetas.sub.partial.train))

# plot correlation matrix
pdf(file = gpath("plots/corrmat.thetas-s.pdf"))
plot.corrmat(thetas.sub.partial.train)
dev.off()

# exploratory factor analysis
fa.sub.1 <- do.fa(thetas.sub.partial.train, 1)
fa.sub.2 <- do.fa(thetas.sub.partial.train, 2)
fa.sub.3 <- do.fa(thetas.sub.partial.train, 3, verbose = F)
fa.sub <- fa.sub.1
fa.sub$loadings

psych::fa.diagram(fa.sub.2)

# check relation to grand sum or other benchmarks
fs.sub.train <- psych::factor.scores(thetas.sub.partial.train, fa.sub)
fs.sub.test <- psych::factor.scores(thetas.sub.partial.test, fa.sub)
pred.sub.train <- cbind(thetas.sub.partial.train, fs.sub.train$scores)
pred.sub.test <- cbind(thetas.sub.partial.test, fs.sub.test$scores)

pred.sub.train$grand <- pred.score.train$grand
pred.sub.test$grand <- pred.score.test$grand
mod.sub <- mgcv::gam(grand ~
                       s(arc, bs="ad") +
                       s(gsm8k, bs="ad") +
                       s(hellaswag, bs="ad") +
                       s(mmlu, bs="ad") +
                       s(truthfulqa, bs="ad") +
                       s(winogrande, bs="ad"),
                       data = pred.sub.train)
# mod.theta <- fit.score(pred.theta.train, fa.theta)
pred.sub.train$p <- predict(mod.sub, pred.sub.train)
pred.sub.test$p <- predict(mod.sub, pred.sub.test)

# save model
saveRDS(mod.sub, gpath("analysis/gams/gam-grand.rds"))

# plot
pred.sub.test$color <- runif(nrow(pred.sub.test))
p.sub <- evaluate.score.pred(pred.sub.test) +
  ggplot2::scale_colour_gradientn(colours = cbPalette()) +
  ggplot2::ggtitle(glue::glue("metabench (d = {numitems.sub$sum})"))
p.sub
saveRDS(p.sub, gpath("plots/metabench-sub.rds"))
r.sub <- cor(pred.sub.test$p, pred.sub.test$grand, method = "spearman")
gprint("r(Predicted, Score) = {round(r.sub,3)}")

# correlation between first factor and grand score 
r.sub <- cor(pred.sub.test$MR1, pred.sub.test$grand, method = "spearman")
gprint("r(Factor1, Score) = {round(r.sub,3)}")

# mod.fa <- mgcv::gam(grand ~ s(MR1, bs="ad"), data = pred.sub.train)
# pred.sub.train$pfa <- predict(mod.fa, pred.sub.train)
# pred.sub.test$pfa <- predict(mod.fa, pred.sub.test)

# sqrt(mean( (pred.sub.test$pfa - pred.sub.test$grand)^2 ))

# =============================================================================
# 3. Check relationship to model architecture
plot.error <- function(pred.sub){
  box::use(ggplot2[...])
  pred.sub$error <- pred.sub$grand - pred.sub$p
  ggplot(pred.sub, aes(x=p, y=error)) +
    geom_point(alpha=0.5) +
    # add horizontal line
    geom_hline(yintercept = 0, linetype="dashed") +
    labs(x="Predicted", y="Error") +
    xlim(0, 100) +
    facet_wrap(~arch) +
    mytheme()+
    theme(legend.position = "None")
}

pred.sub.test.l <- rowmerge(pred.sub.test, leaderboard)
table(pred.sub.test.l$arch)
p.arch <- plot.error(pred.sub.test.l)
outpath <- gpath("plots/error-architecture.png")
ggplot2::ggsave(outpath, p.arch, width = 8, height = 6)

# =============================================================================
# 4. Predict specific scores using all latent abilities
plot.specific <- function(bm){
  pred.sub.train$grand <- pred.score.train[[bm]]
  pred.sub.test$grand <- pred.score.test[[bm]]
  
  mod.sub <- mgcv::gam(grand ~
                         s(arc, bs="ad") +
                         s(gsm8k, bs="ad") +
                         s(hellaswag, bs="ad") +
                         s(mmlu, bs="ad") +
                         s(truthfulqa, bs="ad") +
                         s(winogrande, bs="ad"),
                       data = pred.sub.train)
  pred.sub.train$p <- predict(mod.sub, pred.sub.train)
  pred.sub.test$p <- predict(mod.sub, pred.sub.test)
  
  # save model
  saveRDS(mod.sub, gpath("analysis/gams/gam-{bm}.rds"))
  
  # plot
  pred.sub.test$color <- runif(nrow(pred.sub.test))
  p.sub <- evaluate.score.pred(pred.sub.test) +
    ggplot2::scale_colour_gradientn(colours = cbPalette()) +
    ggplot2::ggtitle(glue::glue("{bm}"))
  saveRDS(p.sub, gpath("plots/mb-{bm}.rds"))
  p.sub
}

# specific reconstruction plots
p.arc <- plot.specific("arc")
p.gsm8k <- plot.specific("gsm8k")
p.hs <- plot.specific("hellaswag")
p.mmlu <- plot.specific("mmlu")
p.tfqa <- plot.specific("truthfulqa")
p.wg <- plot.specific("winogrande")
saveRDS(list(arc=p.arc, gsm8k=p.gsm8k, hs=p.hs, mmlu=p.mmlu, tfqa=p.tfqa, wg=p.wg),
        gpath("plots/mb-specific.rds"))

# specific abilities plots
plot.ability <- function(bm){
  box::use(ggplot2[...])
  df.plot <- data.frame(theta = pred.sub.test[[bm]],
                        score = scores.partial.test[[bm]])
  color.index <- which(names(benchmarks) == bm)
  color <- cbPalette()[color.index]
  p.ab <- ggplot(df.plot, aes(x=theta, y=score)) +
    geom_point(alpha=0.5, color = color) +
    ylim(0, 100) +
    labs(x="Latent Ability", y="Score", title=glue::glue("{bm}")) +
    mytheme()+
    theme(legend.position = "None")
  
  saveRDS(p.ab, gpath("plots/mb-{bm}-ability.rds"))
  p.ab
}
p.arc.ab <- plot.ability("arc")
p.gsm8k.ab <- plot.ability("gsm8k")
p.hs.ab <- plot.ability("hellaswag")
p.mmlu.ab <- plot.ability("mmlu")
p.tfqa.ab <- plot.ability("truthfulqa")
p.wg.ab <- plot.ability("winogrande")
saveRDS(list(arc=p.arc.ab, gsm8k=p.gsm8k.ab, hs=p.hs.ab, mmlu=p.mmlu.ab, tfqa=p.tfqa.ab, wg=p.wg.ab),
        gpath("plots/mb-ability.rds"))


# =============================================================================
# comparison to random subsampling
# setup paral
box::use(doParallel[...], foreach[...])
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "FORK")
doParallel::registerDoParallel(mu.cluster)

# prepare data
data.full.train <- lapply(names(benchmarks), collect.data)
data.full.test <- lapply(names(benchmarks), function(n) collect.data(n, train = F))
names(data.full.train) <- names(data.full.test) <- names(benchmarks)

# run subsampling for 350 item set
gprint("🔁 Running 10000 subsampling iterations with for 350 metabench...")
rmses.full <- foreach(i = 1:10000, .combine=c) %dopar% {
  numitems.100 <- partition.counts()
  subsample.wrapper(i, "full")
}
saveRDS(list(rmses.test = rmses.full), gpath("plots/metabench-full-rmses.rds"))

# run subsampling for reduced item set
gprint("🔁 Running 10000 subsampling iterations with for reduced metabench...")
rmses.sub <- foreach(i = 1:10000, .combine=c) %dopar% {
  subsample.wrapper(i, "sub")
}
saveRDS(list(rmses.test = rmses.sub), gpath("plots/metabench-sub-rmses.rds"))

# run subsampling for 100 item set
gprint("🔁 Running 10000 subsampling iterations with for 100 item metabench...")
rmses.100 <- foreach(i = 1:10000, .combine=c) %dopar% {
  numitems.100 <- partition.counts(n = 100)
  subsample.wrapper(i, "100")
}
saveRDS(list(rmses.test = rmses.100), gpath("plots/metabench-100-rmses.rds"))

# =============================================================================
# most informative items
load.items <- function(b){
  bm <- benchmarks[[b]]
  fitpath <- gpath("analysis/reduced/{b}-{bm$mod}-{bm$est}-{bm$lam}.rds")
  fit <- readRDS(fitpath)
  items <- fit$items
  items$item <- paste0(b, ".", items$item)
  argmax.quantiles <- quantile(items$info.argmax, p=seq(0, 1, 0.2))
  n <- length(argmax.quantiles) - 1
  item.list <- list()
  for (i in 1:n){
    q0 <- as.numeric(argmax.quantiles[i])
    q1 <- as.numeric(argmax.quantiles[i+1])
    # select rows in info.items that are in the current quantile
    q.set <- items |>
      dplyr::filter(info.argmax >= q0 & info.argmax < q1)
    index <- which.max(q.set$info.max)
    item.list[[i]] <- q.set[index, ]
  }
  out <- do.call(rbind, item.list)
  out$quantiles <- names(argmax.quantiles)[2:6]
  out
}
example.items <- lapply(names(benchmarks), load.items)
example.items <- do.call(rbind, example.items)
write.csv(example.items, gpath("analysis/reduced/example.items.csv"), row.names=F)
