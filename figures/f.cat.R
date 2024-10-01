# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
here::i_am("figures/f.cat.R")


this.theme <- function(){
  box::use(ggplot2[...])
  theme(legend.position = "none", plot.margin = margin(1.5, 1.5, 0, 0), "cm")
}

best.benchmarks <- list(
  arc = list(bm = "arc", mod = "4PL", meth = "BM"),
  gsm8k = list(bm = "gsm8k", mod = "2PL", meth = "BM"),
  hellaswag = list(bm = "hellaswag", mod = "4PL", meth = "BM"),
  mmlu = list(bm = "mmlu", mod = "4PL", meth = "BM"),
  truthfulqa = list(bm = "truthfulqa", mod = "3PL", meth = "BM"),
  winogrande = list(bm = "winogrande", mod = "4PL", meth = "EAP")
)
# =============================================================================
# Plot Score Recovery against Number of Items
p.arc <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-arc-4PL-BM.rds")) + 
  ggplot2::labs(title = "ARC", x="") +
  ggplot2::theme(plot.margin = ggplot2::margin(1.0, 1.0, 1, 1), "cm") + ggplot2::xlim(0,1300)
p.gsm8k <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-gsm8k-2PL-BM.rds")) +
  ggplot2::labs(title = "GSM8K", x="", y="") + this.theme() + ggplot2::xlim(0,1300)
p.hellaswag <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-hellaswag-4PL-BM.rds")) + 
  ggplot2::labs(title = "HellaSwag", x="", y="") + ggplot2::xlim(0,1300)
p.mmlu <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-mmlu-4PL-BM.rds")) +
  ggplot2::labs(title = "MMLU") + this.theme() + ggplot2::xlim(0,1300)
p.truthfulqa <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-truthfulqa-3PL-BM.rds")) +
  ggplot2::labs(title = "TruthfulQA", y="") + this.theme() + ggplot2::xlim(0,1300)
p.winogrande <- readRDS(gpath("analysis/cat/cat-score-recovery-plot-winogrande-4PL-EAP.rds")) +
  ggplot2::labs(title = "Winogrande", y="") + this.theme() + ggplot2::xlim(0,1300)

p <- cowplot::plot_grid(p.arc, p.gsm8k, p.hellaswag, p.mmlu, p.truthfulqa, p.winogrande, ncol = 3, align = "v")

outpath <- gpath("figures/f.cat-recovery.pdf")
ggplot2::ggsave(outpath, p, width = 13, height = 6)
gprint("Saved plot to {outpath}.")
