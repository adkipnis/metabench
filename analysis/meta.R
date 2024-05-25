# Go beyond what single items measure by doing factor analysis on the test level.
# This makes single item thetas more interpretable (and potentially allows for even more reduction).
# usage: Rscript meta.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme, get.theta, do.fa, do.fa.cov])
Saveplots <- T
mkdir("plots")
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

collect.theta <- function(benchmark){
   fitpath <- gpath("analysis/models/{benchmark}-all.rds")
   results <- readRDS(fitpath)
   modeltype <- benchmarks[[benchmark]]$mod
   theta.type <- benchmarks[[benchmark]]$est
   if (theta.type == "MAP"){
      theta <- results[[modeltype]]$theta
   } else {
      theta <- get.theta(results[[modeltype]]$model, theta.type)
   }
   datapath <- gpath("data/{benchmark}_preproc.rds")
   names <- rownames(readRDS(datapath)$data)
   theta <- as.data.frame(theta)
   rownames(theta) <- names
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

collect.scores <- function(benchmark){
   datapath <- gpath("data/{benchmark}_preproc.rds")
   all <- readRDS(datapath)
   scores <- data.frame(100 * all$scores.norm)
   colnames(scores) <- benchmark
   rownames(scores) <- rownames(all$data)
   scores
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
   pred.names <- colnames(scores.partial)[(length(benchmarks)+2):ncol(scores.partial)]
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
   plot.score.pred(scores.partial)
}

plot.score.pred <- function(scores.partial){
   box::use(ggplot2[...])
   ggplot(scores.partial, aes(x=grand, y=p)) +
      geom_point(alpha=0.3) +
      geom_abline(intercept=0, slope=1, color="red") +
      labs(title="Score prediction from latent factors",
           x="Total Score (norm.)", y="Predicted") +
      mytheme()
}

# =============================================================================
# get ceiling
benchmarks <- list(arc=list(mod="4PL", est="EAPsum"),
                   gsm8k=list(mod="3PLu", est="MAP"),
                   hellaswag=list(mod="3PL", est="EAPsum"),
                   mmlu_sub=list(mod="3PL", est="EAPsum"),
                   truthfulqa=list(mod="3PL", est="EAPsum"),
                   winogrande=list(mod="3PL", est="EAPsum"))
scores.full <- lapply(names(benchmarks), collect.scores)
scores.partial <- merge.skill(scores.full)

covmat.score <- construct.covmat(scores.full)
cov2cor(covmat.score)|>
  corrplot::corrplot(method="color", type="upper", tl.cex=0.5, order = "hclust")
fa.score.1 <- do.fa(scores.partial, 1)
fa.score.2 <- do.fa(scores.partial, 2)
fa.score.3 <- do.fa(scores.partial, 3)
#fa.score.1 <- do.fa.cov(covmat.score, 1, n.obs = n.obs)
#fa.score.2 <- do.fa.cov(covmat.score, 2, n.obs = n.obs)
fa.score <- fa.score.2
psych::fa.diagram(fa.score)
fa.score$loadings

# =============================================================================
# collect theta estimates and construct covariance matrix
thetas.full <- lapply(names(benchmarks), collect.theta)
thetas.partial <- merge.theta(thetas.full)
thetas.partial <- merge.skill(thetas.full)
covmat.theta <- construct.covmat(thetas.full)
n.obs.min <- min(sapply(thetas.full, function(t) nrow(t)))

# plot correlation matrix
cov2cor(covmat.theta)|>
   corrplot::corrplot(method="color", type="upper", tl.cex=0.5)

# exploratory factor analysis
fa.theta.1 <- do.fa(thetas.partial, 1)
fa.theta.2 <- do.fa(thetas.partial, 2)
fa.theta <- fa.theta.1
psych::fa.diagram(fa.theta)
fa.theta$loadings
fs <- psych::factor.scores(thetas.partial, fa.theta)
plot(sort(fa.theta$uniquenesses, decreasing = T))

# =============================================================================
# check relation to grand sum
scores.partial$grand <- rowSums(scores.partial)/ncol(scores.partial)
scores.partial <- rowmerge(scores.partial, fs$scores)
scores.partial <- fit.score(scores.partial, fa.theta)

# evaluate grand sum prediction from factor scores
evaluate.score.pred(scores.partial)

# =============================================================================
# TODO: check stability wrt subtests
