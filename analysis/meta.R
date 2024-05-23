# Go beyond what single items measure by doing factor analysis on the test level.
# This makes single item thetas more interpretable (and potentially allows for even more reduction).
# usage: Rscript meta.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, parse.args, gprint, gpath, mytheme])
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
   modeltype <- benchmarks[[benchmark]]
   # TODO: choose EAPsum for some
   theta <- results[[modeltype]]$theta
   datapath <- gpath("data/{benchmark}_preproc.rds")
   names <- rownames(readRDS(datapath)$data)
   theta <- as.data.frame(theta)
   rownames(theta) <- names
   colnames(theta) <- benchmark
   theta
}

merge.theta <- function(theta.full){
   theta.reduced <- theta.full[[1]]
   for (i in 2:length(theta.full)){
      theta.reduced <- rowmerge(theta.reduced, theta.full[[i]])
   }
   theta.reduced
}

collect.scores <- function(benchmark){
   datapath <- gpath("data/{benchmark}_preproc.rds")
   all <- readRDS(datapath)
   scores <- data.frame(all$scores.norm)
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

do.fa <- function(covmat, nfactors){
   res <- psych::fa(covmat,
                    nfactors = nfactors,
                    rotate="oblimin",
                    fm = "minres",
                    covar = T,
                    n.obs = n.obs.min)
   evaluate.fa.fit(res)
   res
}

evaluate.fa.fit <- function(res.fa){
   gprint("RMSEA: {round(res.fa$RMSEA[1], 2)} (< 0.05: good, 0.05 - 0.08: reasonable, > 0.10: poor)")
   gprint("Corrected RMSR: {round(res.fa$crms, 2)} (< 0.08: good)")
   gprint("CFI: {round(res.fa$CFI, 2)} (> 0.95: good)")
   gprint("TLI: {round(res.fa$TLI, 2)} (> 0.95: good)")
}

fit.score <- function(scores.partial, res.fa){
   pred.names <- colnames(scores.partial)[(length(benchmarks)+2):ncol(scores.partial)]
   formula <- paste0("grand ~ ", paste0("s(", pred.names, ")", collapse=" + "))
   mod.score <- mgcv::gam(as.formula(formula), data = scores.partial)
   scores.partial$p <- predict(mod.score)
   scores.partial
}

evaluate.score.pred <- function(scores.partial){
   r <- cor(scores.partial$grand, scores.partial$p)
   gprint("Correlation between normalized grand sum and predicted grand sum: {round(r, 3)}")
   s <- scores.partial |>
      dplyr::mutate(error = grand - p) |>
      dplyr::summarize(mae = mean(abs(error)),
                       rmse = sqrt(mean(error^2)))
   gprint("Mean absolute error: {round(s$mae, 3)}, RMSE: {round(s$rmse, 3)}")
   plot.score.pred(scores.partial)
}

plot.score.pred <- function(scores.partial){
   box::use(ggplot2[...])
   ggplot(scores.partial, aes(x=grand, y=p)) +
      geom_point(alpha=0.3) +
      geom_abline(intercept=0, slope=1, color="red") +
      xlim(0.1, 0.9) + ylim(0.1, 0.9) +
      labs(title="Score prediction from latent factors",
           x="Total Score (norm.)", y="Predicted") +
      mytheme()
}

# =============================================================================
# start with mmlu
benchmarks <- add.mmlu()
thetas.mmlu <- lapply(names(benchmarks), collect.theta)
thetas.mmlu <- merge.theta(thetas.mmlu)
covmat.mmlu <- cov(thetas.mmlu)
n.obs.min <- nrow(thetas.mmlu)

# exploratory factor analysis
res.fa.1.mmlu <- do.fa(covmat.mmlu, 1)
res.fa.2.mmlu <- do.fa(covmat.mmlu, 2)
res.fa.3.mmlu <- do.fa(covmat.mmlu, 3)
res.fa <- res.fa.2.mmlu
res.fa$loadings
psych::fa.diagram(res.fa)
res.fs <- psych::factor.scores(thetas.mmlu, res.fa)

# =============================================================================
# collect theta estimates and construct covariance matrix
benchmarks <- list(arc="4PL",
                   gsm8k="3PLu",
                   hellaswag="3PL",
                   truthfulqa="3PL",
                   winogrande="3PL")

thetas.full <- lapply(names(benchmarks), collect.theta)
thetas.partial <- merge.theta(thetas.full)
covmat.theta <- construct.covmat(thetas.full)
n.obs.min <- min(sapply(thetas.full, function(t) nrow(t)))

# plot correlation matrix
cov2cor(covmat.theta)|>
   corrplot::corrplot(method="color", type="upper", tl.pos="n", tl.cex=0.5)

# exploratory factor analysis
res.fa.1 <- do.fa(covmat.theta, 1)
res.fa.2 <- do.fa(covmat.theta, 2)
res.fa.3 <- do.fa(covmat.theta, 3)
res.fa <- res.fa.2
res.fa$loadings
psych::fa.diagram(res.fa)

# estimate factor scores
thetas.partial <- merge.theta(thetas.full)
res.fs <- psych::factor.scores(thetas.partial, res.fa)


# =============================================================================
# check relation to grand sum
scores.full <- lapply(names(benchmarks), collect.scores)
scores.partial <- merge.theta(scores.full)
scores.partial$grand <- rowSums(scores.partial)/ncol(scores.partial)
scores.partial <- rowmerge(scores.partial, res.fs$scores)
scores.partial <- fit.score(scores.partial, res.fa)

# evaluate grand sum prediction from factor scores
evaluate.score.pred(scores.partial)

# efa on scores
res.fa.score <- psych::fa(scores.partial,
          nfactors = 1,
          fm = "minres")
evaluate.fa.fit(res.fa.score)
res.fa.score$loadings
psych::fa.diagram(res.fa.score)
unique <- sort(res.fa.score$uniquenesses, decreasing = T)
plot(unique)
# =============================================================================
# TODO: check stability wrt subtests
