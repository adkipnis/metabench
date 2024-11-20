# =============================================================================
box::use(.. / analysis / utils[gprint, gpath, napply, mytheme, cbPalette, cbPalette2])
box::use(.. / analysis / reduce.utils[score.stats])
box::use(.. / analysis / reduced / best[all.benchmarks])
here::i_am("figures/f.seeds.R")
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande", "metabench")
names <- c("ARC", "GSM8K", "Hellaswag", "MMLU", "TruthfulQA", "WinoGrande", "metabench")
point.size <- 5
mean.size <- 5
mean.shape <- 4

# =============================================================================
# helper functions
get.stats <- function(data){
   data <- data |>
      dplyr::mutate(error = p - grand)
   rmse <- sqrt(mean(data$error^2))
   mae <- mean(abs(data$error))
   r <- cor(data$p, data$grand, method="spearman")
   list(rmse=rmse, mae=mae, r=r)
}

load.stats <- function(bm, seed){
   d.path <- gpath("analysis/reduced/numitems-sub-seed={seed}.rds")
   d <- readRDS(d.path)[[bm]]
   if (seed == 1){
      plot.path <- gpath("plots/mb-{bm}-v2.rds")
   } else {
      plot.path <- gpath("plots/mb-{bm}-seed={seed}.rds")
   }
   data <- readRDS(plot.path)$data
   stats <- get.stats(data)
   data.frame(bm=bm, seed=seed, d=d, rmse=stats$rmse, mae=stats$mae, r=stats$r)
}

load.total <- function(seed){
   d.path <- gpath("analysis/reduced/numitems-sub-seed={seed}.rds")
   d <- readRDS(d.path)$sum
   if (seed == 1){
      plot.path <- gpath("plots/metabench-sub-v2.rds")
   } else {
      plot.path <- gpath("plots/metabench-sub-seed={seed}.rds")
   }
   data <- readRDS(plot.path)$data
   stats <- get.stats(data)
   data.frame(bm="metabench", seed=seed, d=d, rmse=stats$rmse, mae=stats$mae, r=stats$r)
}


load.bm <- function(bm){
   seeds <- c(1, 2, 3, 4, 5)
   if (bm == "metabench"){
      aggregates <- napply(seeds, load.total)
   } else {
      aggregates <- napply(seeds, function(s) load.stats(bm, s))
   }
   do.call(rbind, aggregates)
}

load.df <- function(){
   stats <- napply(benchmarks, load.bm)
   stats <- do.call(rbind, stats)
   rownames(stats) <- NULL
   stats |>
      dplyr::mutate(bm=factor(bm, levels=benchmarks, labels=names))
}

