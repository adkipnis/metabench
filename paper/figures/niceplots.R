#niceify(hs.evo) + labs(x = "Score", y = "Predicted", title = "HellaSwag")) Gather some saved figures and make them paper ready
# usage: Rscript niceplots.R

# =============================================================================
# custom utils, args, path, seed
here::i_am("paper/figures/niceplots.R")
box::use(../../analysis/utils[mkdir, gprint, gpath], ggplot2[...])

# =============================================================================
niceify <- function(p){
   p <- p + 
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size = 20, hjust = 0.5)
            )
   p[["layers"]][[3]][["aes_params"]][["size"]] <- 5
   p[["layers"]][[3]][["data"]][["x"]] <- 75
   p[["layers"]][[3]][["data"]][["y"]] <- 25
   p
}
# =============================================================================
# evlolutionary algorithm
mmlu.evo <- readRDS(gpath("plots/mmlu-reduced.rds"))[[3]]
mmlu.rand <- readRDS(gpath("plots/mmlu-reduced.rds"))[[4]]
(mmlu.evo <- niceify(mmlu.evo) + labs(x = "Score", y = "Predicted", title = "MMLU (Evo)"))
(mmlu.rand <- niceify(mmlu.rand) + labs(x = "Score", y = "Predicted", title = "MMLU (Random)"))

hs.evo <- readRDS(gpath("plots/hellaswag-reduced.rds"))[[3]]
hs.rand <- readRDS(gpath("plots/hellaswag-reduced.rds"))[[4]]
(hs.evo <- niceify(hs.evo) + labs(x = "Score", y = "Predicted", title = "HellaSwag (Evo)"))
(hs.rand <- niceify(hs.rand) + labs(x = "Score", y = "Predicted", title = "HellaSwag (Random)"))

(p.evo <- cowplot::plot_grid(hs.evo, hs.rand, mmlu.evo, mmlu.rand, ncol = 2, labels = "AUTO"))

outpath <- gpath("paper/figures/evo.pdf")
ggplot2::ggsave(outpath, p.evo, width = 12, height = 8)
