# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
box::use(./violin.utils[plot.violin])
here::i_am("figures/f.random.R")

# =============================================================================
# helper functions
cbp <- cbPalette()

get.rmse <- function(result){
  df.plot <- result$df.score.sub |>
    dplyr::filter(set == "test") |>
    dplyr::mutate(error = p - score)
  sqrt(mean(df.plot$error^2))
}

get.mae <- function(result){
  df.plot <- result$df.score.sub |>
    dplyr::filter(set == "test") |>
    dplyr::mutate(error = p - score)
  mean(abs(df.plot$error))
}

rmse.from.plot <- function(p){
  text <- p[["layers"]][[3]][["aes_params"]]$label
  text <- gsub("\n.*", "", text)
  as.numeric(gsub("[^0-9.]", "", text))
}

stats.from.plot <- function(p){
  rmse <- rmse.from.plot(p)
  mae <- mean(abs(p$data$grand - p$data$p))
  r<- cor(p$data$grand, p$data$p, method = "spearman")
  gprint("RMSE = {round(rmse, 3)},\nMAE = {round(mae, 3)},\nr = {round(r, 3)}")
}

plot.score <- function(result, bm, color){
  box::use(ggplot2[...])
  df.plot <- result$df.score.sub |>
    dplyr::filter(set == "test")
  rmse <- get.rmse(result)
  mae <- get.mae(result)
  r <- cor(df.plot$theta, df.plot$score, method = "spearman")
  gprint("RMSE = {round(rmse, 3)},\nMAE = {round(mae, 3)},\nr = {round(r, 3)}")
  n.items <- nrow(result$items)
  text <- glue::glue("RMSE = {round(rmse, 3)}")
  ggplot(df.plot, aes(x = score, y = p)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_point(alpha = 0.5, color = color) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    annotate("text", x = 75, y = 25, label = text, size = 5) +
    labs(
      title = glue::glue("{bm} (d = {n.items})"),
      x = "Score",
      y = "Predicted",
    ) +
    mytheme() 
}

add.asterisk <- function(x, y){
  box::use(ggplot2[...])
  ggplot2::geom_text(aes(x = x, y = y, label = "*"),
                     color = "#444444", size = 4.5, vjust = 0.8)
}

rmse.percentile <- function(our, random){
  sum(our <= random)/length(random) 
}

compare.rmses <- function(){
  btr.arc <- rmse.percentile(rmse.from.plot(p.arc), rand.list[["ARC"]])
  btr.gsm8k <- rmse.percentile(rmse.from.plot(p.gsm8k), rand.list[["GSM8K"]])
  btr.hs <- rmse.percentile(rmse.from.plot(p.hs), rand.list[["HellaSwag"]])
  btr.mmlu <- rmse.percentile(rmse.from.plot(p.mmlu), rand.list[["MMLU"]])
  btr.tfqa <- rmse.percentile(rmse.from.plot(p.tfqa), rand.list[["TruthfulQA"]])
  btr.wg <- rmse.percentile(rmse.from.plot(p.wg), rand.list[["WinoGrande"]])
  list(arc = btr.arc, gsm8k = btr.gsm8k, hs = btr.hs, mmlu = btr.mmlu,
       tfqa = btr.tfqa, wg = btr.wg)
}


# =============================================================================
# prepare data
arc.sub <- readRDS(gpath("analysis/reduced/arc-2PL-MAP-0.005.rds"))
gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-2PL-EAPsum-0.005.rds"))
hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-3PL-MAP-0.01.rds"))
mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-3PL-MAP-0.01.rds"))
tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-2PL-EAPsum-0.01.rds"))
wg.sub <- readRDS(gpath("analysis/reduced/WinoGrande-4PL-MAP-0.005.rds"))

p.mb <- readRDS(gpath("plots/metabench-sub.rds")) +
  ggplot2::labs(y ="") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
stats.from.plot(p.mb)

# =============================================================================
# violin plots for RMSE
rand.list = list(
  ARC = readRDS(gpath("data/arc-sub-100.rds"))$rmses.test,
  GSM8K = readRDS(gpath("data/gsm8k-sub-237.rds"))$rmses.test,
  HellaSwag = readRDS(gpath("data/hellaswag-sub-58.rds"))$rmses.test,
  MMLU = readRDS(gpath("data/mmlu-sub-102.rds"))$rmses.test,
  TruthfulQA = readRDS(gpath("data/truthfulqa-sub-136.rds"))$rmses.test,
  WinoGrande = readRDS(gpath("data/WinoGrande-sub-106.rds"))$rmses.test,
  metabench = readRDS(gpath("plots/metabench-sub-rmses.rds"))$rmses.test
)

# =============================================================================
# 1-predictor plots (score from specific skill)
p.arc <- plot.score(arc.sub, "ARC", cbp[1]) + ggplot2::labs(x = "") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.gsm8k <- plot.score(gsm8k.sub, "GSM8K", cbp[2]) + ggplot2::labs(x = "", y = "") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.hs <- plot.score(hs.sub, "HellaSwag", cbp[3]) + ggplot2::labs(x = "", y = "") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.mmlu <- plot.score(mmlu.sub, "MMLU", cbp[4]) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.tfqa <- plot.score(tfqa.sub, "TruthfulQA", cbp[5]) + ggplot2::labs(y = "") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.wg <- plot.score(wg.sub, "WinoGrande", cbp[6]) + ggplot2::labs(y = "") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.reduced <- cowplot::plot_grid(
  p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg, ncol = 3)

compare.rmses()

p.rand <- plot.violin(rand.list, distance = 10.0) +
  add.asterisk(rmse.from.plot(p.arc), 0) +
  add.asterisk(rmse.from.plot(p.gsm8k), 10) +
  add.asterisk(rmse.from.plot(p.hs), 20) +
  add.asterisk(rmse.from.plot(p.mmlu), 30) +
  add.asterisk(rmse.from.plot(p.tfqa), 40) +
  add.asterisk(rmse.from.plot(p.wg), 50) +
  add.asterisk(rmse.from.plot(p.mb), 60) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

p.col <- cowplot::plot_grid(p.rand, p.mb, ncol = 1, align= "hv", labels = c("B", "C"), label_x = 0.03)
p.spec <- cowplot::plot_grid(p.reduced, p.col, labels = c("A", NA), rel_widths = c(3, 1), label_x=0)
outpath <- gpath("figures/f.specific.pdf")
ggplot2::ggsave(outpath, p.spec, width = 16, height = 8)

# =============================================================================
# 6-predictor plots
six <- readRDS(gpath("plots/mb-specific.rds"))
p.arc <- six$arc + ggplot2::scale_color_gradientn(colors = cbp[[1]]) +
  ggplot2::labs(x = "", title = "ARC* (d = 100)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.gsm8k <- six$gsm8k + ggplot2::scale_color_gradientn(colors = cbp[[2]]) +
  ggplot2::labs(x = "", y = "", title = "GSM8K* (d = 237)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.hs <- six$hs + ggplot2::scale_color_gradientn(colors = cbp[[3]]) + 
  ggplot2::labs(x = "", y = "", title = "HellaSwag* (d = 58)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.mmlu <- six$mmlu + ggplot2::scale_color_gradientn(colors = cbp[[4]]) + 
  ggplot2::labs(title = "MMLU* (d = 102)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.tfqa <- six$tfqa + ggplot2::scale_color_gradientn(colors = cbp[[5]]) + 
  ggplot2::labs(y = "", title = "TruthfulQA* (d = 136)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.wg <- six$wg + ggplot2::scale_color_gradientn(colors = cbp[[6]]) + 
  ggplot2::labs(y = "", title = "WinoGrande* (d = 106)") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.reduced.6 <- cowplot::plot_grid(
  p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg, ncol = 3)

stats.from.plot(p.arc)
stats.from.plot(p.gsm8k)
stats.from.plot(p.hs)
stats.from.plot(p.mmlu)
stats.from.plot(p.tfqa)
stats.from.plot(p.wg)
compare.rmses()

p.rand.6 <- plot.violin(rand.list, distance = 10.0) +
  add.asterisk(rmse.from.plot(p.arc), 0) +
  add.asterisk(rmse.from.plot(p.gsm8k), 10) +
  add.asterisk(rmse.from.plot(p.hs), 20) +
  add.asterisk(rmse.from.plot(p.mmlu), 30) +
  add.asterisk(rmse.from.plot(p.tfqa), 40) +
  add.asterisk(rmse.from.plot(p.wg), 50) +
  add.asterisk(rmse.from.plot(p.mb), 60) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

p.col.6 <- cowplot::plot_grid(p.rand.6, p.mb, ncol = 1, align= "hv", labels = c("B", "C"), label_x = 0.03)
p.meta <- cowplot::plot_grid(p.reduced.6, p.col.6, labels = c("A", NA), rel_widths = c(3, 1), label_x=0)
outpath <- gpath("figures/f.meta.pdf")
ggplot2::ggsave(outpath, p.meta, width = 16, height = 8)

# =============================================================================
# reviewer-requested violin plot of 100 item subsamples from open llm leaderboard
rand.100 = list(
  metabench = readRDS(gpath("plots/metabench-100-rmses.rds"))$rmses.test
)
p.100 <- plot.violin(rand.100) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) + 
  ggplot2::labs(title = "100-item subsamples from entire pool") +
  ggplot2::scale_fill_manual(values = rep("#FFFFFF", 7))
outpath <- gpath("figures/f.100.png")
ggplot2::ggsave(outpath, p.100, width = 6, height = 4)
