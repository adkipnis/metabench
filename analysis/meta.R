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
add.mmlu <- function(){
   # list all mmlu_*.rds files in data folder
   mmlu.files <- list.files(gpath("data"), pattern="mmlu_.*_preproc.rds", full.names=T)
   mmlu.benchmarks <- gsub("_preproc.rds", "", basename(mmlu.files))
   # write list where each key is from mmlu.benchmarks and the value is "3PL"
   out <- list()
   for (benchmark in mmlu.benchmarks){
      out[[benchmark]] <- "3PL"
   }
   out
}

rowmerge <- function(df1, df2){
   merge(df1, df2, by="row.names") |>
     tibble::column_to_rownames("Row.names")
}

collect.theta <- function(benchmark, train=T){
  model.type <- benchmarks[[benchmark]]$mod
  theta.type <- benchmarks[[benchmark]]$est
  fitpath <- gpath("analysis/models/{benchmark}-{model.type}-cv.rds")
  results <- readRDS(fitpath)
  model <- results$model
  if (train) {
    theta <- results$df |> dplyr::filter(set == "train") |> dplyr::select(theta)
  } else {
    theta <- results$df |> dplyr::filter(set == "test") |> dplyr::select(theta)
  }
  
  if (theta.type != "MAP") {
    if (benchmark %in% c("hellaswag", "mmlu")) {
      datapath <- gpath("data/{benchmark}-sub.rds")
    } else {
      datapath <- gpath("data/{benchmark}-preproc-split.rds")
    }
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
  colnames(theta) <- benchmark
  as.data.frame(theta)
}

merge.skill <- function(skill.full){
   skill.reduced <- skill.full[[1]]
   for (i in 2:length(skill.full)){
      skill.reduced <- rowmerge(skill.reduced, skill.full[[i]])
   }
   skill.reduced
}

collect.scores <- function(benchmark){
   datapath <- gpath("data/{benchmark}_preproc.rds")
   all <- readRDS(datapath)
   scores.norm <- all$scores.orig/all$max.points.orig
   scores <- data.frame(100 * scores.norm)
   colnames(scores) <- benchmark
   rownames(scores) <- rownames(all$data)
   scores
}

collect.numitems <- function(benchmark, type) {
   if (type == "original"){
      datapath <- gpath("data/{benchmark}_preproc.rds")
      all <- readRDS(datapath)
      numitems <- all$max.points.orig
   } else if (type == "preprocessed"){
     datapath <- gpath("data/{benchmark}_preproc.rds")
     all <- readRDS(datapath)
     numitems <- ncol(all$data)
   } else if (type == "reduced") {
      model.type <- benchmarks[[benchmark]]$mod
      fitpath <- gpath("analysis/reduced/{benchmark}-{model.type}-0.1.rds")
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
   mod.score <- mgcv::gam(as.formula(formula), data = scores.partial)
   scores.partial$p <- predict(mod.score)
   scores.partial
}

evaluate.score.pred <- function(scores.partial){
   r.s <- cor(scores.partial$grand, scores.partial$MR1, method = "spearman")
   r.p <- cor(scores.partial$grand, scores.partial$p, method = "spearman")
   s <- scores.partial |>
      dplyr::mutate(error = grand - p) |>
      dplyr::summarize(mae = mean(abs(error)),
                       rmse = sqrt(mean(error^2)))
   gprint("Spearman corr. Score (norm.) x First Factor: {round(r.s, 3)}")
   gprint("Spearman corr. Score (norm.) x Predicted: {round(r.p, 3)}")
   gprint("Mean absolute error: {round(s$mae, 3)}, RMSE: {round(s$rmse, 3)}")
   plot.score.pred(scores.partial,
     text = glue::glue("r = {round(r.p, 3)}\nMAE = {round(s$mae, 3)}\nRMSE = {round(s$rmse, 3)}"))
}

plot.score.pred <- function(scores.partial, text = ""){
   box::use(ggplot2[...])
   x.label <- 0.9 * diff(range(scores.partial$grand)) + min(scores.partial$grand)
   y.label <- 0.1 * diff(range(scores.partial$p)) + min(scores.partial$p)
   ggplot(scores.partial, aes(x=grand, y=p)) +
      geom_point(alpha=0.3) +
      geom_abline(intercept=0, slope=1, color="red") +
      xlim(20,80) + ylim(20, 80) +
      annotate("text", x = x.label, y = y.label, label = text, size = 3) +
      labs(x="Total Score (norm.)", y="Predicted") +
      mytheme()
}



# =============================================================================
# get ceiling for score prediction

# load scores
benchmarks <- list(arc=list(mod="4PL", est="EAPsum"),
                   gsm8k=list(mod="3PLu", est="EAPsum"),
                   hellaswag=list(mod="3PL", est="MAP"),
                   mmlu_sub=list(mod="3PLu", est="EAPsum"),
                   truthfulqa=list(mod="3PL", est="EAPsum"),
                   winogrande=list(mod="3PL", est="EAPsum"))
scores.full <- lapply(names(benchmarks), collect.scores)
scores.partial <- merge.skill(scores.full)
numitems.orig <- get.numitems(benchmarks, "original")

# plot correlation matrix
covmat.score <- construct.covmat(scores.full)
  corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.score.1 <- do.fa(scores.partial, 1)
fa.score.2 <- do.fa(scores.partial, 2)
fa.score.3 <- do.fa(scores.partial, 3, verbose = F)
fa.score <- fa.score.2
psych::fa.diagram(fa.score)
fa.score$loadings
fs.score <- psych::factor.scores(scores.partial, fa.score)

# check prediction of grand sum from factor scores
scores.partial$grand <- rowSums(scores.partial)/ncol(scores.partial)
pred.base <- rowmerge(scores.partial, fs.score$scores)
pred.base <- fit.score(pred.base, fa.score)
p.base <- evaluate.score.pred(pred.base) +
  ggplot2::ggtitle(glue::glue(
    "Score prediction from factor scores (raw points, n = {numitems.orig$sum})"))

# =============================================================================
# collect theta estimates and construct covariance matrix
thetas.full <- lapply(names(benchmarks), collect.theta)
thetas.partial <- merge.skill(thetas.full)
numitems.theta <- get.numitems(benchmarks, "preprocessed")
covmat.theta <- construct.covmat(thetas.full)

# plot correlation matrix
cov2cor(covmat.theta)|>
   corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.theta.1 <- do.fa(thetas.partial, 1)
fa.theta.2 <- do.fa(thetas.partial, 2)
fa.theta <- fa.theta.1
psych::fa.diagram(fa.theta)
fa.theta$loadings
fs.theta <- psych::factor.scores(thetas.partial, fa.theta)
sort(fa.theta$uniquenesses, decreasing = T) 

# check relation to grand sum
pred.full <- rowmerge(scores.partial, fs.theta$scores)
pred.full <- fit.score(pred.full, fa.theta)

# evaluate grand sum prediction from factor scores
p.full <- evaluate.score.pred(pred.full) +
  ggplot2::ggtitle(glue::glue(
    "Score prediction from factor scores (IRT, n = {numitems.theta$sum})"))

# =============================================================================
# collect theta estimates from reduced benchmarks
thetas.sub.full <- lapply(names(benchmarks), function(b) collect.theta(b, full = F))
thetas.sub.partial <- merge.skill(thetas.sub.full)
covmat.sub.theta <- construct.covmat(thetas.sub.full)
numitems.sub <- get.numitems(benchmarks, "reduced")

# plot correlation matrix
cov2cor(covmat.sub.theta)|>
   corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")

# exploratory factor analysis
fa.theta.s <- do.fa(thetas.sub.partial, 1)
psych::fa.diagram(fa.theta.s)
fa.theta.s$loadings
fs.theta.s <- psych::factor.scores(thetas.sub.partial, fa.theta.s)

# check relation to grand sum
pred.sub <- rowmerge(scores.partial, fs.theta.s$scores)
pred.sub <- fit.score(pred.sub, fa.theta.s)

# evaluate grand sum prediction from factor scores
p.sub <- evaluate.score.pred(pred.sub) +
  ggplot2::ggtitle(glue::glue(
    "Score prediction from factor scores (reduced IRT, n = {numitems.sub$sum})"))

# =============================================================================
# summary
p <- cowplot::plot_grid(p.base, p.full, p.sub,
                        ncol = 1, labels = "AUTO", align = "v")
ggplot2::ggsave(gpath("plots/meta-prediction.png"), p, width = 8, height = 12)
