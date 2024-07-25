# Demonstration of how metabench model evaluation
# usage: Rscript demo.R

# =============================================================================
# custom utils, args, path, seed
box::use(../analysis/utils[parse.args, gprint, gpath, napply, df2data, get.theta])
parse.args(names = c("ID"),
           defaults = c("0")
)
here::i_am("testing/demo.R")
irt.specs <- list(
  arc = list(mod = "2PL", est = "MAP", lam = 0.005),
  gsm8k = list(mod = "2PL", est = "EAPsum", lam = 0.005),
  hellaswag = list(mod = "3PL", est = "MAP", lam = 0.01),
  mmlu = list(mod = "3PL", est = "MAP", lam = 0.01),
  truthfulqa = list(mod = "2PL", est = "EAPsum", lam = 0.01),
  winogrande = list(mod = "4PL", est = "MAP", lam = 0.005)
)

# =============================================================================
# helper variables and functions
llm <- "SF-Foundation/Ein-72B-v0.12"
true.thetas <- c(0.4523034, 1.652988, 1.007097, 1.465396, 1.733728, 1.455679)
true.scores <- c(76.19454, 79.22669, 89.46425, 77.48841, 65.97307, 84.45146)

benchmarks <- c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")

load.irt <- function(bm){
   mod <- irt.specs[[bm]]$mod
   est <- irt.specs[[bm]]$est
   lam <- irt.specs[[bm]]$lam
   path <- gpath("analysis/reduced/{bm}-{mod}-{est}-{lam}.rds")
   readRDS(path)
}

load.gam <- function(bm){
  path <- gpath("analysis/gams/gam-{bm}.rds")
  readRDS(path)
}

load.data <- function(bm){
  items <- irt.list[[bm]]$items$item
  df <- readr::read_csv(gpath("data/{bm}.csv"), show_col_types = F) |>
    dplyr::filter(source == llm) |>
    dplyr::mutate(bm = bm)
  data <- df2data(df)
  data[,items]
}

estimate.theta <- function(bm){
  est <- irt.specs[[bm]]$est
  irt.model <- irt.list[[bm]]$model
  resp <- data.list[[bm]]
  get.theta(irt.model, method = est, resp = resp)[1]
}

estimate.score <- function(bm){
  gam.model <- gam.list[[bm]]
  predict(gam.model, newdata = theta.df)
}

# =============================================================================
# preparations
irt.list <- napply(benchmarks, load.irt)
gam.list <- napply(names(irt.specs), load.gam)
gam.list[["grand"]] <- load.gam("grand")
data.list <- napply(benchmarks, load.data)
# TODO: check if data indices match

# =============================================================================
# estimate latent variables
theta.list <- napply(benchmarks, estimate.theta)
theta.list[["mmlu"]] <- true.thetas[4]
theta.df <- as.data.frame(do.call(cbind, theta.list))

# estimate scores
scores.list <- napply(names(gam.list), estimate.score)

# TODO: get interval by either (1) using SE for theta or
# (2) querying a table of empirical error bounds from meta.R
