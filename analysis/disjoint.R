# Reproduce some analyses in meta.R for the disjoint version of metabench

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme, cbPalette, get.theta, do.fa, do.fa.cov])
Saveplots <- T
here::i_am("analysis/disjoint.R")
benchmark.names <- c("ARC", "GSM8K", "HellaSwag", "MMLU", "TruthfulQA", "WinoGrande")

# =============================================================================
# helper functions
napply <- function(some.names, some.func){
  out <- lapply(some.names, some.func)
  setNames(out, some.names)
}

rowmerge <- function(df1, df2){
   merge(df1, df2, by="row.names") |>
     tibble::column_to_rownames("Row.names")
}

load.reduced <- function(bm){
   mod <- benchmarks.v1[[bm]]$mod
   est <- benchmarks.v1[[bm]]$est
   lam <- benchmarks.v1[[bm]]$lam
   path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}-seed=1.rds")
   readRDS(path)$items$item
}

load.reduced.2 <- function(bm){
   mod <- benchmarks[[bm]]$mod
   est <- benchmarks[[bm]]$est
   lam <- benchmarks[[bm]]$lam
   path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}-seed=1-v2.rds")
   readRDS(path)$items$item
}

# Version A
benchmarks <- list(
  arc = list(mod = "4PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.001),
  truthfulqa = list(mod = "3PL", est = "MAP", lam = 0.001),
  winogrande = list(mod = "3PL", est = "MAP", lam = 0.001)
)

# Version B
benchmarks.v1 <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.001),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
  winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
)

collect.data <- function(benchmark, train=T){
  # load full data and remove items from v1 of metabench
  datapath <- gpath("data/{benchmark}-preproc-split-seed=1.rds")
  all <- readRDS(datapath)
  reduced <- load.reduced(benchmark)
  if (train) {
    data <- all$data.train
  } else {
    data <- all$data.test
  }
  data[,!colnames(data) %in% reduced]
}

subsample.data.score <- function(benchmark, seed, source = "full"){
   if (source == "full"){
     n <- numitems.theta[[benchmark]]
   } else if (source == "sub"){
     n <- numitems.sub[[benchmark]]
  } else {
     print("Unkown source provided, must be 'full' or 'sub'")
     return()
   }
   data <- data.full.train[[benchmark]]
   set.seed(seed)
   sample(1:ncol(data), n, replace = F)
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
  scores.train.r$grand <- scores.partial.train$grand
  scores.test.r$grand <- scores.partial.test$grand
  mod.score.r <- mgcv::gam(grand ~ s(grand.r, bs="ad"),
                           data = scores.train.r)
  scores.train.r$p <- predict(mod.score.r, scores.train.r)
  scores.test.r$p <- predict(mod.score.r, scores.test.r)
  scores.test.r |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
}

collect.theta.reduced <- function(benchmark, train = T){
  model.type <- benchmarks[[benchmark]]$mod
  theta.type <- benchmarks[[benchmark]]$est
  lam <- benchmarks[[benchmark]]$lam
  fitpath <- gpath("analysis/reduced/{benchmark}-{model.type}-{theta.type}-{lam}-seed=1-v2.rds")
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
   datapath <- gpath("data/{benchmark}-sub-350-seed=1.rds")
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
      datapath <- gpath("data/{benchmark}-preproc-split-seed=1.rds")
      all <- readRDS(datapath)
      numitems <- all$max.points.orig
   } else if (type == "reduced") {
      model.type <- benchmarks[[benchmark]]$mod
      theta.type <- benchmarks[[benchmark]]$est
      lam <- benchmarks[[benchmark]]$lam
      fitpath <- gpath("analysis/reduced/{benchmark}-{model.type}-{theta.type}-{lam}-seed=1-v2.rds")
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

prepare.lm.data <- function(bm, type){
   if (type == "train"){
      data <- data.full.train[[bm]]
      scores <- data.frame(grand = scores.partial.train[[bm]])
      rownames(scores) <- rownames(scores.partial.train)
   } else {
      data <- data.full.test[[bm]]
      scores <- data.frame(grand = scores.partial.test[[bm]])
      rownames(scores) <- rownames(scores.partial.test)
   }
   items <- load.reduced.2(bm)
   data <- data[,colnames(data) %in% items]
   rowmerge(scores, data)
}

train.lm <- function(bm){
  data.train <- prepare.lm.data(bm, "train")
  data.test <- prepare.lm.data(bm, "test")
  nc <- ncol(data.train)
  sub.train <- rowSums(data.train[,-1]) / nc * 100
  sub.test <- rowSums(data.test[,-1]) / nc * 100
  mod.lin <- lm(grand ~ ., data = data.train)
  data.train$p <- predict(mod.lin, data.train)
  data.test$p <- predict(mod.lin, data.test)
  rmse.train <- data.train |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
  rmse.test <- data.test |> dplyr::mutate(error = grand - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2))) |> as.numeric()
  out <- list(rmse.train = rmse.train,
         rmse.test = rmse.test,
         pred.train = data.train$p,
         pred.test = data.test$p,
         sub.train = sub.train,
         sub.test = sub.test,
         model = mod.lin)
  outpath <- gpath("analysis/lms/lm-{bm}-seed=1-v2.rds")
  saveRDS(out, outpath)
  out
}



# =============================================================================
# Leaderboard
leaderboard <- read.csv(gpath("scraping/open-llm-leaderboard.csv"))
categories <- c("unknown", "LlamaForCausalLM", "MistralForCausalLM",
                "MixtralForCausalLM")
leaderboard$arch[leaderboard$arch == "error"] <- "unknown"
leaderboard$arch[!leaderboard$arch %in% categories] <- "other"
rownames(leaderboard) <- leaderboard$name
leaderboard <- leaderboard |> dplyr::select(nparams, arch)

# =============================================================================
# Prepare data and point scores

data.full.train <- lapply(names(benchmarks), collect.data)
data.full.test <- lapply(names(benchmarks),
                         function(n) collect.data(n, train = F))
names(data.full.train) <- names(data.full.test) <- names(benchmarks)

scores.full.train <- lapply(names(benchmarks), collect.scores)
scores.full.test <- lapply(names(benchmarks),
                           function(n) collect.scores(n, train = F))
scores.partial.train <- merge.skill(scores.full.train)
scores.partial.test <- merge.skill(scores.full.test)
scores.partial.train$grand <- rowMeans(scores.partial.train)
scores.partial.test$grand <- rowMeans(scores.partial.test)
numitems.orig <- get.numitems(benchmarks, "original")

# =============================================================================
# Joint score reconstruction
# collect theta estimates from reduced benchmarks
numitems.sub <- get.numitems(benchmarks, "reduced")
numitems.sub
thetas.sub.full.train <- lapply(names(benchmarks), collect.theta.reduced)
thetas.sub.full.test <- lapply(names(benchmarks),
                               function(n) collect.theta.reduced(n, train=F))
pred.sub.train <- Reduce(rowmerge, thetas.sub.full.train)
pred.sub.test <- Reduce(rowmerge, thetas.sub.full.test)
pred.sub.train$grand <- rowMeans(scores.partial.train)
pred.sub.test$grand <- rowMeans(scores.partial.test)

# add linear models
lm.arc <- train.lm("arc")
lm.gsm8k <- train.lm("gsm8k")
lm.hs <- train.lm("hellaswag")
lm.mmlu <- train.lm("mmlu")
lm.tfqa <- train.lm("truthfulqa")
lm.wg <- train.lm("winogrande")

pred.sub.train$arc.l <- lm.arc$pred.train
pred.sub.train$arc.s <- lm.arc$sub.train
pred.sub.train$gsm8k.l <- lm.gsm8k$pred.train
pred.sub.train$gsm8k.s <- lm.gsm8k$sub.train
pred.sub.train$hs.l <- lm.hs$pred.train
pred.sub.train$hs.s <- lm.hs$sub.train
pred.sub.train$mmlu.l <- lm.mmlu$pred.train
pred.sub.train$mmlu.s <- lm.mmlu$sub.train
pred.sub.train$tfqa.l <- lm.tfqa$pred.train
pred.sub.train$tfqa.s <- lm.tfqa$sub.train
pred.sub.train$wg.l <- lm.wg$pred.train
pred.sub.train$wg.s <- lm.wg$sub.train
pred.sub.train <- pred.sub.train |>
  dplyr::mutate(grand.l = 1/6 * (arc.l + gsm8k.l + hs.l + mmlu.l + tfqa.l + wg.l),
                grand.s = 1/6 * (arc.s + gsm8k.s + hs.s + mmlu.s + tfqa.s + wg.s))

pred.sub.test$arc.l <- lm.arc$pred.test
pred.sub.test$arc.s <- lm.arc$sub.test
pred.sub.test$gsm8k.l <- lm.gsm8k$pred.test
pred.sub.test$gsm8k.s <- lm.gsm8k$sub.test
pred.sub.test$hs.l <- lm.hs$pred.test
pred.sub.test$hs.s <- lm.hs$sub.test
pred.sub.test$mmlu.l <- lm.mmlu$pred.test
pred.sub.test$mmlu.s <- lm.mmlu$sub.test
pred.sub.test$tfqa.l <- lm.tfqa$pred.test
pred.sub.test$tfqa.s <- lm.tfqa$sub.test
pred.sub.test$wg.l <- lm.wg$pred.test
pred.sub.test$wg.s <- lm.wg$sub.test
pred.sub.test <- pred.sub.test |>
  dplyr::mutate(grand.l = 1/6 * (arc.l + gsm8k.l + hs.l + mmlu.l + tfqa.l + wg.l),
                grand.s = 1/6 * (arc.s + gsm8k.s + hs.s + mmlu.s + tfqa.s + wg.s))

bs <- "ad"
mod.sub <- mgcv::gam(grand ~
                       s(grand.l, bs=bs) +
                       s(grand.s, bs=bs) +
                       s(arc, bs=bs) +
                       s(gsm8k, bs=bs) +
                       s(hellaswag, bs=bs) +
                       s(mmlu, bs=bs) +
                       s(truthfulqa, bs=bs) +
                       s(winogrande, bs=bs),
                       data = pred.sub.train)


pred.sub.train$p <- predict(mod.sub, pred.sub.train)
pred.sub.test$p <- predict(mod.sub, pred.sub.test)

# save model
saveRDS(mod.sub, gpath("analysis/gams/gam-grand-v2.rds"))

# plot
pred.sub.test$color <- runif(nrow(pred.sub.test))
p.sub <- evaluate.score.pred(pred.sub.test) +
  ggplot2::scale_colour_gradientn(colours = cbPalette()) +
  ggplot2::ggtitle(glue::glue("metabench-A (d = {numitems.sub$sum})"))
p.sub
saveRDS(p.sub, gpath("plots/metabench-sub-v2.rds"))

# =============================================================================
# Check relationship to model architecture
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
outpath <- gpath("figures/f.architecture-v2.pdf")
ggplot2::ggsave(outpath, p.arch, width = 8, height = 6)

# =============================================================================
# FA
thetas.train <- pred.sub.train |> dplyr::select(arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande)
thetas.test <- pred.sub.test |> dplyr::select(arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande)
fa.score <- do.fa(thetas.train, 1)
sort(fa.score$uniquenesses, decreasing = T)
scores.partial.test$MR1 <- psych::factor.scores(thetas.test, fa.score)$scores
r.score <- cor(scores.partial.test$MR1, scores.partial.test$grand, method = "spearman")
gprint("r(Factor1, Score) = {round(r.score,3)}")

# =============================================================================
# Predict specific scores using all latent abilities
plot.specific <- function(bm){
  pred.sub.train$grand <- scores.partial.train[[bm]]
  pred.sub.test$grand <- scores.partial.test[[bm]] 
  if (bm == "hellaswag"){
    this.str <- "hs"
  } else if (bm == "truthfulqa") {
    this.str <- "tfqa"
  } else if (bm == "winogrande") {
    this.str <- "wg"
  } else {
    this.str <- bm
  }
  this.l.str <- paste0(this.str, ".l")
  this.s.str <- paste0(this.str, ".s")
  pred.sub.train$this.l <- pred.sub.train[[this.l.str]]
  pred.sub.train$this.s <- pred.sub.train[[this.s.str]]
  pred.sub.test$this.l <- pred.sub.test[[this.l.str]]
  pred.sub.test$this.s <- pred.sub.test[[this.s.str]]
  
  # rmse.lin <- sqrt(mean((pred.sub.test$grand - pred.sub.test$this.l)^2))
  # gprint("Test RMSE of the linear model: {round(rmse.lin, 3)}")
  
  mod.sub <- mgcv::gam(grand ~
                         s(grand.l, bs="ad") +
                         # s(grand.s, bs="ad") +
                         s(this.l, bs="ad") +
                         # s(this.s, bs="ad") +
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
  saveRDS(mod.sub, gpath("analysis/gams/gam-{bm}-v2.rds"))

  # plot
  pred.sub.test$color <- runif(nrow(pred.sub.test))
  p.sub <- evaluate.score.pred(pred.sub.test) +
    ggplot2::scale_colour_gradientn(colours = cbPalette()) +
    ggplot2::ggtitle(glue::glue("{bm}"))
  saveRDS(p.sub, gpath("plots/mb-{bm}-v2.rds"))
  p.sub
}

# specific reconstruction plots
(p.arc <- plot.specific("arc"))
(p.gsm8k <- plot.specific("gsm8k"))
(p.hs <- plot.specific("hellaswag")) # don't use subscores for this one
(p.mmlu <- plot.specific("mmlu"))
(p.tfqa <- plot.specific("truthfulqa"))
(p.wg <- plot.specific("winogrande"))
saveRDS(list(arc=p.arc, gsm8k=p.gsm8k, hs=p.hs, mmlu=p.mmlu, tfqa=p.tfqa, wg=p.wg),
        gpath("plots/mb-specific-v2.rds"))

# =============================================================================
# comparison to random subsampling
# setup paral
box::use(doParallel[...], foreach[...])
n.cores <- parallel::detectCores() - 1
mu.cluster <- parallel::makeCluster(n.cores, type = "FORK")
doParallel::registerDoParallel(mu.cluster)

# run subsampling for reduced item set
gprint("🔁 Running 10000 subsampling iterations with for reduced metabench...")
rmses.sub <- foreach(i = 1:10000, .combine=c) %dopar% {
  subsample.wrapper(i, "sub")
}
saveRDS(list(rmses.test = rmses.sub), gpath("plots/metabench-sub-rmses-v2.rds"))

# =============================================================================
# export items to csv
load.items <- function(b){
  bm <- benchmarks[[b]]
  fitpath <- gpath("analysis/reduced/{b}-{bm$mod}-{bm$est}-{bm$lam}-v2.rds")
  fit <- readRDS(fitpath)
  items <- fit$items
  items$item <- paste0(b, ".", items$item)
  items
}

export.items <- function(b){
  items <- load.items(b) |> dplyr::select(item, prompt)
  outpath <- gpath("items/items-{b}-A.csv")
  write.csv(items, outpath, row.names=F)
  gprint("Exported {b} to {outpath}")
  items
}

out <- lapply(names(benchmarks), export.items)
