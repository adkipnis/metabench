# Reconstruct the full score after evaluating an LLM
# with metabench on the eleuther-harness
# usage: Rscript reconstruct.R modelName

# =============================================================================
# custom utils, path
box::use(../analysis/utils[parse.args, gprint, gpath, napply, get.theta])
parse.args(names = c("ID", "VERSION", "PERMUTED"),
           defaults = c("EleutherAI__pythia-160m", "A", "F"))
here::i_am("testing/reconstruct.R")

# specs for reduced benchmarks
irt.specs.tmp <- list(
  "A" = list(
    arc = list(mod = "4PL", est = "MAP", lam = 0.005),
    gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
    hellaswag = list(mod = "3PL", est = "MAP", lam = 0.005),
    mmlu = list(mod = "3PL", est = "MAP", lam = 0.001),
    truthfulqa = list(mod = "3PL", est = "MAP", lam = 0.001),
    winogrande = list(mod = "3PL", est = "MAP", lam = 0.001)
  ),
  "B" = list(
    arc = list(mod = "2PL", est = "MAP", lam = 0.005),
    gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.001),
    hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
    mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
    truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
    winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
  )
)

# setup globals
PERMUTED <- as.logical(PERMUTED)
irt.specs <- irt.specs.tmp[[VERSION]]
benchmarks <- names(irt.specs)
sfx.ver <- ifelse(VERSION=="A", "", "_secondary") # for .jsonl
sfx.per <- ifelse(PERMUTED, "_permute", "")
sfx.red <- ifelse(VERSION=="A", "-v2", "") # for .rds

# =============================================================================
# helper variables and functions
load.json <- function(bm){
   if (bm == "gsm8k") sfx.per <- ""
   pattern <- glue::glue("^samples_metabench_{bm}{sfx.ver}{sfx.per}.*\\.jsonl$")
   files <- list.files(path=data.path, pattern=pattern, full.names=T, recursive=T)
   out <- files[1]
   if (length(files) > 1){
     gprint("Warning - Found multiple matching files, taking the first:\n{out}")
   }
   if (length(files) == 0){
     gprint("Warning - No files found for {bm}{sfx.ver}{sfx.per}")
   }
   out
}

json2long <- function(fpath, bm){
   data <- jsonlite::stream_in(file(fpath), verbose=F)
   # get item indices
   idx <- data$doc$metabench_idx
   if (bm == "mmlu"){
     idx <- paste0(data$doc$subject, ".", idx)
   }
   # get response accuracies
   if (bm == "gsm8k"){
     acc <- data$exact_match
   } else {
     acc <- data$acc
   }
   data.frame(idx = idx, acc = acc)
}

load.data <- function(bm){
  fpath <- load.json(bm)
  testit::assert(file.exists(fpath))
  resp.long <- json2long(fpath, bm)
  items <- irt.list[[bm]]$items$item
  idx.sort <- match(items, resp.long$idx)
  resp.long <- resp.long[idx.sort,]
  rownames(resp.long) <- NULL
  resp.long |> tidyr::pivot_wider(names_from = idx, values_from = acc)
}

load.irt <- function(bm){
   mod <- irt.specs[[bm]]$mod
   est <- irt.specs[[bm]]$est
   lam <- irt.specs[[bm]]$lam
   mod.path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}-seed=1{sfx.red}.rds")
   readRDS(mod.path)
}

load.lm <- function(bm){
  lm.path <- gpath("analysis/lms/lm-{bm}-seed=1{sfx.red}.rds")
  readRDS(lm.path)
}

load.gam <- function(bm){
  gam.path <- gpath("analysis/gams/gam-{bm}-seed=1{sfx.red}.rds")
  readRDS(gam.path)$model
}

load.errors <- function(bm){
  gam.path <- gpath("analysis/gams/gam-{bm}-seed=1{sfx.red}.rds")
  df.test <- readRDS(gam.path)$test |>
    dplyr::mutate(error = p - grand) |>
    dplyr::select(p, error) |>
    dplyr::arrange(p)
  rownames(df.test) <- NULL
  df.test
}

get.subscore <- function(bm){
  resp <- data.list[[bm]]
  d <- ncol(resp)
  rowSums(resp) / d * 100
}

estimate.linpred <- function(bm){
  model <- lm.list[[bm]]$model
  resp <- data.list[[bm]]
  predict(model, resp)[[1]]
}

estimate.theta.i <- function(bm, i){
  model <- irt.list[[bm]]$model
  method <- irt.specs[[bm]]$est
  resp <- data.list[[bm]]
  get.theta(model, method, resp)[[i]]
}

estimate.theta <- function(bm) estimate.theta.i(bm, 1)

estimate.theta.se <- function(bm) estimate.theta.i(bm, 2)

gather.results <- function(){
  theta <- sapply(benchmarks, estimate.theta)
  theta.se <- sapply(benchmarks, estimate.theta.se)
  sub <- sapply(benchmarks, get.subscore)
  lin <- sapply(benchmarks, estimate.linpred)
  df.theta <- data.frame(as.list(theta))
  df.theta.se <- data.frame(as.list(theta.se))
  df.sub <- data.frame(as.list(sub))
  df.lin <- data.frame(as.list(lin))
  colnames(df.theta.se) <- paste0(colnames(df.theta.se),  ".se")
  colnames(df.sub) <- paste0(colnames(df.sub),  ".s")
  colnames(df.lin) <- paste0(colnames(df.lin),  ".l")
  cbind(df.theta, df.theta.se, df.sub, df.lin) |>
    dplyr::mutate(grand.s = mean(sub), grand.l = mean(lin))
}

add.se <- function(bm, df.gam, theta.fac){
   if (theta.fac == 0) return(df.gam)
   se.str <- paste0(bm, ".se")
   theta <- df.gam[[bm]]
   se <- df.gam[[se.str]]
   theta.new <- theta + theta.fac * se
   df.gam[[bm]] <- theta.new
   df.gam
}

get.local.rmse <- function(bm, prediction, radius){
  df.error <- err.list[[bm]]
  lower <- prediction - radius
  upper <- prediction + radius
  i <- which.min(abs(df.error$p - lower))
  j <- which.min(abs(df.error$p - upper))
  df.error.tmp <- df.error[i:j,]
  sqrt(mean(df.error.tmp$error^2))
}

estimate.gam <- function(bm, theta.fac=0){
  # subroutine
  gam <- gam.list[[bm]]
  
  # optionally add error bound to theta
  for (b in benchmarks){
     df.gam <- add.se(b, df.gam, theta.fac)
  }

  # prepare predictors for specific score
  if (bm != "grand"){
    this.l <- paste0(bm, ".l")
    this.s <- paste0(bm, ".s")
    df.gam$this.l <- df.gam[[this.l]]
    df.gam$this.s <- df.gam[[this.s]]
  }
  predict(gam, df.gam)[[1]]
}

estimate <- function(bm, df.gam, rmse.rad=2, theta.fac=0){
  # wrapper for automatic assignment
  colname <- paste0(bm, ".p")
  est <- estimate.gam(bm, 0)
  est <- min(max(est, 0), 100)
  
  # RMSE CI
  if (rmse.rad > 0){
    local.rmse <- get.local.rmse(bm, est, rmse.rad)
    emp.lb <- max(est - local.rmse, 0)
    emp.ub <- min(est + local.rmse, 100)
    gprint("{bm}: {round(est,3)}, [{round(emp.lb,3)}, {round(emp.ub,3)}]")
  } else {
    gprint("{bm}: {round(est,3)}")
  }
  
  # theta CI
  if (theta.fac != 0){
    est.lb <- estimate.gam(bm, -theta.fac)
    est.ub <- estimate.gam(bm, theta.fac)
    est.lb <- max(est.lb, 0)
    est.ub <- min(est.ub, 100)
    gprint("theta-based CI (±{theta.fac} SE): [{round(est.lb,3)}, {round(est.ub,3)}]")
  }
  
  df.gam[[colname]] <- est
  df.gam
}


# =============================================================================
# load data for your model
data.path <- gpath("testing/harness-results/{ID}")
testit::assert(file.exists(data.path))

# load irt, linear, and gam fits
irt.list <- napply(benchmarks, load.irt)
lm.list <- napply(benchmarks, load.lm)
gam.list <- napply(c(benchmarks, "grand"), load.gam)
err.list <- napply(c(benchmarks, "grand"), load.errors)

# load your data
data.list <- napply(benchmarks, load.data)

# estimate subscores, linear predictors, latent ability
gprint("🏁 Reconstruction results of {ID} on metabench{sfx.ver}{sfx.per}...")
df.gam <- gather.results()
for (bm in c("grand", benchmarks)){
  df.gam <- estimate(bm, df.gam)
}
