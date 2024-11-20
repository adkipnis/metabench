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

