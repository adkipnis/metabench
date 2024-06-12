# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
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

rmse.from.plot <- function(p){
  text <- p[["layers"]][[3]][["aes_params"]]$label
  text <- gsub("\n.*", "", text)
  as.numeric(gsub("[^0-9.]", "", text))
}

plot.score <- function(result, bm, color){
  box::use(ggplot2[...])
  df.plot <- result$df.score.sub |>
    dplyr::filter(set == "test")
  rmse <- get.rmse(result)
  # r <- cor(df.plot$theta, df.plot$score, method = "spearman")
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

plot.violin <- function(df){
  box::use(ggplot2[...])
  ggplot(df, aes(y = bm, x = rmse, fill = bm)) +
    geom_violin(draw_quantiles = c(0.5)) +
    labs(y="", x = "RMSE", title = "Random") +
    scale_fill_manual(values = cbp) +
    scale_color_gradientn(colors = cbp) +
    scale_y_discrete(limits=rev) +
    mytheme() +
    theme( plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
           legend.position = "None")
}

add.asterisk <- function(x, y){
  box::use(ggplot2[...])
  ggplot2::geom_text(aes(x = x, y = y, label = "*"),
                     color = "#444444", size = 4.5, vjust = 0.8)
}

# =============================================================================
# prepare data
arc.sub <- readRDS(gpath("analysis/reduced/arc-2PL-MAP-0.005.rds"))
gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-2PL-EAPsum-0.005.rds"))
hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-3PL-MAP-0.01.rds"))
mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-3PL-MAP-0.01.rds"))
tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-2PL-EAPsum-0.01.rds"))
wg.sub <- readRDS(gpath("analysis/reduced/winogrande-4PL-MAP-0.005.rds"))

p.mb <- readRDS(gpath("plots/metabench-sub.rds")) +
  ggplot2::labs(y ="") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))

# =============================================================================
# 1-predictor plots (score from specific skill)
p.arc <- plot.score(arc.sub, "ARC", cbp[1]) + ggplot2::labs(x = "")
p.gsm8k <- plot.score(gsm8k.sub, "GSM8K", cbp[2]) + ggplot2::labs(x = "", y = "")
p.hs <- plot.score(hs.sub, "HellaSwag", cbp[3]) + ggplot2::labs(x = "", y = "")
p.mmlu <- plot.score(mmlu.sub, "MMLU", cbp[4])
p.tfqa <- plot.score(tfqa.sub, "TruthfulQA", cbp[5]) + ggplot2::labs(y = "")
p.wg <- plot.score(wg.sub, "Winogrande", cbp[6]) + ggplot2::labs(y = "")
p.reduced <- cowplot::plot_grid(
  p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg, ncol = 3)

# =============================================================================
# 6-predictor plots
six <- readRDS(gpath("plots/mb-specific.rds"))
p.arc.6 <- six$arc + ggplot2::scale_color_gradientn(colors = cbp[[1]]) +
   ggplot2::labs(x = "", title = "ARC")
p.gsm8k.6 <- six$gsm8k + ggplot2::scale_color_gradientn(colors = cbp[[2]]) +
   ggplot2::labs(x = "", y = "", title = "GSM8K")
p.hs.6 <- six$hs + ggplot2::scale_color_gradientn(colors = cbp[[3]]) + 
   ggplot2::labs(x = "", y = "", title = "HellaSwag")
p.mmlu.6 <- six$mmlu + ggplot2::scale_color_gradientn(colors = cbp[[4]]) + 
   ggplot2::labs(title = "MMLU")
p.tfqa.6 <- six$tfqa + ggplot2::scale_color_gradientn(colors = cbp[[5]]) + 
   ggplot2::labs(y = "", title = "TruthfulQA")
p.wg.6 <- six$wg + ggplot2::scale_color_gradientn(colors = cbp[[6]]) + 
   ggplot2::labs(y = "", title = "Winogrande")
p.reduced.6 <- cowplot::plot_grid(
  p.arc.6, p.gsm8k.6, p.hs.6, p.mmlu.6, p.tfqa.6, p.wg.6, ncol = 3)

# =============================================================================
# violin plots for RMSE
rand.list = list(
  ARC = readRDS(gpath("data/arc-sub-100.rds"))$rmses.test,
  GSM8K = readRDS(gpath("data/gsm8k-sub-237.rds"))$rmses.test,
  HellaSwag = readRDS(gpath("data/hellaswag-sub-58.rds"))$rmses.test,
  MMLU = readRDS(gpath("data/mmlu-sub-102.rds"))$rmses.test,
  TruthfulQA = readRDS(gpath("data/truthfulqa-sub-136.rds"))$rmses.test,
  Winogrande = readRDS(gpath("data/winogrande-sub-106.rds"))$rmses.test,
  metabench = readRDS(gpath("plots/metabench-sub-rmses.rds"))$rmses.test
)
rand.list <- lapply(rand.list, function(x) data.frame(rmse = x))
rand.list <- lapply(names(rand.list), function(x) cbind(rand.list[[x]], bm = x))
rand <- do.call(rbind, rand.list)
rand$bm <- factor(rand$bm, levels = c("ARC", "GSM8K", "HellaSwag", "MMLU",
                                      "TruthfulQA", "Winogrande", "metabench"))

p.rand <- plot.violin(rand) +
  ggplot2::scale_x_continuous(limits=c(0.5,7), breaks = seq(1,7)) +
  add.asterisk(rmse.from.plot(p.arc), 7) +
  add.asterisk(rmse.from.plot(p.gsm8k), 6) +
  add.asterisk(rmse.from.plot(p.hs), 5) +
  add.asterisk(rmse.from.plot(p.mmlu), 4) +
  add.asterisk(rmse.from.plot(p.tfqa), 3) +
  add.asterisk(rmse.from.plot(p.wg), 2) +
  add.asterisk(rmse.from.plot(p.mb), 1) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())




