# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
here::i_am("figures/f.scores.R")


this.theme <- function(){
   box::use(ggplot2[...])
   theme(legend.position = "none", plot.margin = margin(1.5, 1.5, 0, 0), "cm")
}

# =============================================================================
p.arc <- readRDS(gpath("plots/arc-score-dist.rds")) + 
   ggplot2::labs(title = "ARC", x="") +
  ggplot2::theme(plot.margin = ggplot2::margin(1.0, 1.0, 0.1, 0.1), "cm")
p.gsm8k <- readRDS(gpath("plots/gsm8k-score-dist.rds")) +
   ggplot2::labs(title = "GSM8K", x="", y="") + this.theme()
p.hellaswag <- readRDS(gpath("plots/hellaswag-score-dist.rds")) + 
   ggplot2::labs(title = "HellaSwag", x="", y="") + this.theme()
p.mmlu <- readRDS(gpath("plots/mmlu-score-dist.rds")) +
   ggplot2::labs(title = "MMLU") + this.theme()
p.truthfulqa <- readRDS(gpath("plots/truthfulqa-score-dist.rds")) +
   ggplot2::labs(title = "TruthfulQA", y="") + this.theme()
p.winogrande <- readRDS(gpath("plots/winogrande-score-dist.rds")) +
   ggplot2::labs(title = "Winogrande", y="") + this.theme()

p <- cowplot::plot_grid(p.arc, p.gsm8k, p.hellaswag, p.mmlu, p.truthfulqa, p.winogrande, ncol = 3, align = "v")

outpath <- gpath("figures/f.scores.pdf")
ggplot2::ggsave(outpath, p, width = 13, height = 6)
gprint("Saved plot to {outpath}.")
