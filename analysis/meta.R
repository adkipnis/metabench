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
benchmarks <- list(arc="4PL",
                   gsm8k="3PLu",
                   hellaswag="3PL",
                   truthfulqa="3PL",
                   winogrande="3PL")
# TODO: include MMLU

# =============================================================================
# helper functions

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
     for (j in 1:n){
        df.cov <- rowmerge(thetas[[i]], thetas[[j]])
        covmat[i, j] <- cov(df.cov[,1], df.cov[,2])
     }
  }
  covmat
}

do.fa <- function(covmat, nfactors){
  psych::fa(covmat, nfactors = nfactors, rotate="promax", fm = "ml",
            covar = T, n.obs = n.obs.min)
}

evaluate.fa.fit <- function(res.fa){
   gprint("RMSEA: {round(res.fa$RMSEA[1], 2)} (< 0.05: good, 0.05 - 0.08: reasonable, > 0.10: poor)")
   gprint("Corrected RMSR: {round(res.fa$crms, 2)} (< 0.08: good)")
   gprint("CFI: {round(res.fa$CFI, 2)} (> 0.95: good)")
   gprint("TLI: {round(res.fa$TLI, 2)} (> 0.95: good)")
}

# =============================================================================
# collect theta estimates and construct covariance matrix
thetas.full <- lapply(names(benchmarks), collect.theta)
covmat.theta <- construct.covmat(thetas.full)
n.obs.min <- min(sapply(thetas.full, function(t) nrow(t)))

# plot correlation matrix
cov2cor(covmat.theta) |> 
  corrplot::corrplot(method="color", type="upper", order="hclust",
                     tl.col="black", tl.srt=45)

# exploratory factor analysis
res.fa.1 <- do.fa(covmat.theta, 1)
res.fa.2 <- do.fa(covmat.theta, 2)
eval.fa.fit(res.fa.1)
eval.fa.fit(res.fa.2)
res.fa <- res.fa.2
res.fa
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
