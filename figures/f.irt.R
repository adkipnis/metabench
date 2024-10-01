# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
box::use(./violin.utils[plot.violin])
here::i_am("figures/f.irt.R")

# =============================================================================
# helper functions
benchmarks <- c("ARC", "GSM8K", "HellaSwag", "MMLU", "TruthfulQA", "WinoGrande")
cbp <- cbPalette()

niceify <- function(p, benchmark){
  box::use(ggplot2[...])
  index <- which(benchmarks == benchmark)
   p <- p +
     labs(title = glue::glue("{benchmark} (d = 350)")) +
     aes(color = set) +
     scale_color_manual(values = cbp[index]) +
     theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
           legend.position = "None")
     
   p[["layers"]][[3]][["aes_params"]][["size"]] <- 5
   p
}

rmse.from.plot <- function(p){
  text <- p[["layers"]][[3]][["aes_params"]]$label
  text <- gsub("\n.*", "", text)
  as.numeric(gsub("[^0-9.]", "", text))
}

randlist2df <- function(rand.list){
  rand.list <- lapply(rand.list, function(x) data.frame(rmse = x))
  rand.list <- lapply(names(rand.list), function(x) cbind(rand.list[[x]], bm = x))
  rand <- do.call(rbind, rand.list)
  rand$bm <- factor(rand$bm, levels = c("ARC", "GSM8K", "HellaSwag", "MMLU", "TruthfulQA", "WinoGrande", "metabench"))
  rand
}

add.asterisk <- function(x, y){
  box::use(ggplot2[...])
  ggplot2::geom_text(aes(x = x, y = y, label = "*"), color = "#444444", size = 4.5, vjust = 0.8)
}
# =============================================================================
# load score plots
arc.irt <- readRDS(gpath("plots/arc-EAPsum-1-cv.rds"))[[2]] |>
  niceify(benchmark = "ARC") + ggplot2::labs(x = "")
gsm8k.irt <- readRDS(gpath("plots/gsm8k-EAPsum-1-cv.rds"))[[1]] |>
  niceify(benchmark = "GSM8K") + ggplot2::labs(x = "", y = "") 
hs.irt <- readRDS(gpath("plots/hellaswag-MAP-1-cv.rds"))[[2]] |>
  niceify(benchmark = "HellaSwag") + ggplot2::labs(x = "", y = "")
mmlu.irt <- readRDS(gpath("plots/mmlu-EAPsum-1-cv.rds"))[[3]] |>
  niceify(benchmark = "MMLU")
tfqa.irt <- readRDS(gpath("plots/truthfulqa-EAPsum-1-cv.rds"))[[1]] |>
  niceify(benchmark = "TruthfulQA") + ggplot2::labs(y = "")
wg.irt <- readRDS(gpath("plots/winogrande-EAPsum-1-cv-v2.rds"))[[3]] |>
  niceify(benchmark = "WinoGrande") + ggplot2::labs(y = "")
mb <- readRDS(gpath("plots/metabench-full.rds")) +
  ggplot2::labs(y ="", title = "metabench (d = 2100)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))

p.irt <- cowplot::plot_grid(
  arc.irt, gsm8k.irt, hs.irt, mmlu.irt, tfqa.irt, wg.irt, ncol = 3)

# load random results
rand.list = list(
   ARC = readRDS(gpath("data/arc-sub-350.rds"))$rmses.test,
   GSM8K = readRDS(gpath("data/gsm8k-sub-350.rds"))$rmses.test,
   HellaSwag = readRDS(gpath("data/hellaswag-sub-350.rds"))$rmses.test,
   MMLU = readRDS(gpath("data/mmlu-sub-350.rds"))$rmses.test,
   TruthfulQA = readRDS(gpath("data/truthfulqa-sub-350.rds"))$rmses.test,
   WinoGrande = readRDS(gpath("data/winogrande-sub-350.rds"))$rmses.test,
   metabench = readRDS(gpath("plots/metabench-full-rmses.rds"))$rmses.test
)
p.rand <- plot.violin(rand.list, distance = 10.0, cbp = cbp) +
  add.asterisk(rmse.from.plot(arc.irt), 0) +
  add.asterisk(rmse.from.plot(gsm8k.irt), 10) +
  add.asterisk(rmse.from.plot(hs.irt), 20) +
  add.asterisk(rmse.from.plot(mmlu.irt), 30) +
  add.asterisk(rmse.from.plot(tfqa.irt), 40) +
  add.asterisk(rmse.from.plot(wg.irt), 50) +
  add.asterisk(rmse.from.plot(mb), 60) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

# final plot
p.col <- cowplot::plot_grid(p.rand, mb, ncol = 1, align= "v", labels = c("B", "C"), label_x = 0.03)
p <- cowplot::plot_grid(p.irt, p.col, labels = c("A", NA), rel_widths = c(3, 1), label_x=0)
outpath <- gpath("figures/f.irt.pdf")
ggplot2::ggsave(outpath, p, width = 16, height = 8)
gprint("Saved plot to {outpath}.")


mean(c(rmse.from.plot(arc.irt),
       rmse.from.plot(gsm8k.irt),
       rmse.from.plot(hs.irt),
       rmse.from.plot(mmlu.irt),
       rmse.from.plot(tfqa.irt),
       rmse.from.plot(wg.irt)))
