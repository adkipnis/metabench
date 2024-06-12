# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
here::i_am("figures/f.logistic.R")

# =============================================================================
# helper functions
cbp <- cbPalette()
rename <- function(benchmark){
   out <- benchmark
   if (benchmark == "arc") {
      out <- "ARC"
   } else if (benchmark == "gsm8k") {
      out <- "GSM8K"
   } else if (benchmark == "hellaswag") {
      out <- "HellaSwag"
   } else if (benchmark == "mmlu") {
      out <- "MMLU"
   } else if (benchmark == "truthfulqa") {
      out <- "TruthfulQA"
   } else if (benchmark == "winogrande") {
      out <- "Winogrande"
   }
   out
}

apply.theme <- function(plot){
   box::use(ggplot2[...])
   bm <- plot$labels$title
   plot$labels$title <- rename(bm)
   plot + 
     theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "cm"))
}

# =============================================================================
# load plots
benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
p.arc <- readRDS(gpath("plots/arc-logistic.rds")) |> apply.theme() + ggplot2::labs(x = "")
p.gsm8k <- readRDS(gpath("plots/gsm8k-logistic.rds")) |> apply.theme() + ggplot2::labs(x = "", y = "")
p.hellaswag <- readRDS(gpath("plots/hellaswag-logistic.rds")) |> apply.theme() + ggplot2::labs(x = "", y = "")
p.mmlu <- readRDS(gpath("plots/mmlu-logistic.rds")) |> apply.theme()
p.truthfulqa <- readRDS(gpath("plots/truthfulqa-logistic.rds")) |> apply.theme() + ggplot2::labs(y = "")
p.winogrande <- readRDS(gpath("plots/winogrande-logistic.rds")) |> apply.theme() + ggplot2::labs(y = "")
p <- cowplot::plot_grid(p.arc, p.gsm8k, p.hellaswag, p.mmlu, p.truthfulqa, p.winogrande, nrow = 2)

# save
outpath <- gpath("figures/f.logistic.pdf")
ggplot2::ggsave(outpath, p, width = 16, height = 8)
gprint("Saved plot to {outpath}.")
