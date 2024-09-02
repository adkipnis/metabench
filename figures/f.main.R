# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette, cbPalette2])
box::use(./violin.utils[plot.violin])
here::i_am("figures/f.random.R")
skip.reduced <- T # load v2
suffix <- ifelse(skip.reduced, "-v2", "")

# =============================================================================
# helper functions
if (skip.reduced){
   cbp <- cbPalette()
} else {
   cbp <- cbPalette2()
}

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
  suffix <- ifelse(skip.reduced, "-test", "")
  df.plot <- result$df.score.sub |>
    dplyr::filter(set == "test")
  rmse <- get.rmse(result)
  mae <- get.mae(result)
  r <- cor(df.plot$theta, df.plot$score, method = "spearman")
  gprint("RMSE = {round(rmse, 3)},\nMAE = {round(mae, 3)},\nr = {round(r, 3)}")
  n.items <- nrow(result$items)
  text <- glue::glue("RMSE = {round(rmse, 3)}\nr = {round(r, 3)}")
  ggplot(df.plot, aes(x = score, y = p)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_point(alpha = 0.5, color = color) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    annotate("text", x = 75, y = 25, label = text, size = 5) +
    labs(
      title = glue::glue("{bm}{suffix} (d = {n.items})"),
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

get.coords <- function(p){
  xlim <- ggplot_build(p)$layout$panel_params[[1]]$x.range
  ylim <- ggplot_build(p)$layout$panel_params[[1]]$y.range
  x.coord <- xlim[1] + 0.75 * (xlim[2] - xlim[1])
  y.coord <- ylim[1] + 0.25 * (ylim[2] - ylim[1])
  list(x = x.coord, y = y.coord)
}

bm.map <- list(arc = "arc", gsm8k = "gsm8k", hs = "hellaswag", mmlu = "mmlu",
               tfqa = "truthfulqa", wg = "winogrande")
color.map <- list(arc = cbp[[1]], gsm8k = cbp[[2]], hs = cbp[[3]], mmlu = cbp[[4]],
                  tfqa = cbp[[5]], wg = cbp[[6]])

compare.versions <- function(bm){
  box::use(ggplot2[...])
  data.A <- readRDS(gpath("plots/mb-specific-v2.rds"))[[bm]]$data
  data.B <- readRDS(gpath("plots/mb-specific.rds"))[[bm]]$data
  color <- color.map[[bm]]
  bm <- bm.map[[bm]]
  
  # rank of latent ability
  r.latent <- cor(data.A[[bm]], data.B[[bm]], method = "spearman")
  text.latent <- glue::glue("r = {round(r.latent, 3)}")
  p.latent <- data.frame(A = data.A[[bm]], B = data.B[[bm]]) |>
    ggplot(aes(x = A, y = B)) +
    geom_point(alpha = 0.5, color = color) +
    labs(x = "Latent (A)", y = "Latent (B)") +
    mytheme()
  coords <- get.coords(p.latent)
  p.latent <- p.latent + annotate("text", x = coords$x, y = coords$y,
                                  label = text.latent, size = 5)
  
  # rank of gam-predicted score
  r.pred <- cor(data.A$p, data.B$p, method = "spearman")
  text.pred <- glue::glue("r = {round(r.pred, 3)}")
  p.pred <- data.frame(A = data.A$p, B = data.B$p) |>
    ggplot(aes(x = A, y = B)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_point(alpha = 0.5, color = color) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    labs(x = "Predicted (A)", y = "Predicted (B)") +
    mytheme()
  coords <- get.coords(p.pred)
  p.pred <- p.pred + annotate("text", x = coords$x, y = coords$y,
                              label = text.pred, size = 5) 
  
  # bias
  bias <- mean(data.A$p - data.B$p)
  text.bias <- glue::glue("b = {round(bias, 3)}%")
  p.bias <- data.frame(grand = data.A$grand, diff = data.A$p - data.B$p) |>
    ggplot(aes(x = grand, y = diff)) +
    geom_abline(intercept = 0, slope = 0,
                linetype = "dashed") +
    geom_point(alpha = 0.5, color = color) +
    coord_cartesian(xlim = c(0, 100)) +
    labs(x = "Score", y = "A - B") +
    mytheme()
  coords <- get.coords(p.bias)
  p.bias <- p.bias + annotate("text", x = coords$x, y = coords$y,
                                  label = text.bias, size = 5) 
  # out
  cowplot::plot_grid(p.latent, p.pred, p.bias, nrow = 1, scale = 0.95)
}

compare.versions.mb <- function(){
  box::use(ggplot2[...])
  data.A <- readRDS(gpath("plots/metabench-sub-v2.rds"))$data
  data.B <- readRDS(gpath("plots/metabench-sub.rds"))$data
  
  # rank of average latent ability
  mean.A <- rowMeans(
    data.A |> dplyr::select(arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande))
  mean.B <- rowMeans(
    data.B |> dplyr::select(arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande))
  r.latent <- cor(mean.A, mean.B, method = "spearman")
  text.latent <- glue::glue("r = {round(r.latent, 3)}")
  p.latent <- data.frame(A = mean.A, B = mean.B, color = data.A$color) |>
    ggplot(aes(x = A, y = B, color = color)) +
    geom_point(alpha = 0.5) +
    labs(x = "Latent (A)", y = "Latent (B)") +
    scale_colour_gradientn(colours = cbp) +
    mytheme() + 
    theme(legend.position = "None")
  coords <- get.coords(p.latent)
  p.latent <- p.latent + annotate("text", x = coords$x, y = coords$y,
                                  label = text.latent, size = 5)
  
  # rank of gam-predicted score
  r.pred <- cor(data.A$p, data.B$p, method = "spearman")
  text.pred <- glue::glue("r = {round(r.pred, 3)}")
  p.pred <- data.frame(A = data.A$p, B = data.B$p, color = data.A$color) |>
    ggplot(aes(x = A, y = B, color = color)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_point(alpha = 0.5) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    labs(x = "Predicted (A)", y = "Predicted (B)") +
    scale_colour_gradientn(colours = cbp) +
    mytheme() +
    theme(legend.position = "None")
  coords <- get.coords(p.pred)
  p.pred <- p.pred + annotate("text", x = coords$x, y = coords$y,
                              label = text.pred, size = 5) 
  
  # rank of lm-predicted score
  r.lin <- cor(data.1$grand.l, data.2$grand.l, method = "spearman")
  rmse.lin.1 <- sqrt(mean((data.A$grand.l - data.A$grand)^2))
  rmse.lin.2 <- sqrt(mean((data.B$grand.l - data.B$grand)^2))
  gprint("The linear model RMSEs are {round(rmse.lin.1, 3)} for version A and {round(rmse.lin.2, 3)} for version B.
         Their Spearman correlation is {round(r.lin, 3)}.")
  
  # bias
  bias <- mean(data.A$p - data.B$p)
  text.bias <- glue::glue("b = {round(bias, 3)}%")
  p.bias <- data.frame(grand = data.A$grand, diff = data.A$p - data.B$p, color = data.A$color) |>
    ggplot(aes(x = grand, y = diff, color = color)) +
    geom_abline(intercept = 0, slope = 0,
                linetype = "dashed") +
    geom_point(alpha = 0.5) +
    coord_cartesian(xlim = c(0, 100)) +
    labs(x = "Score", y = "A - B") +
    scale_colour_gradientn(colours = cbp) +
    mytheme() +
    theme(legend.position = "None")
  coords <- get.coords(p.bias)
  p.bias <- p.bias + annotate("text", x = coords$x, y = coords$y,
                              label = text.bias, size = 5) 
  # out
  cowplot::plot_grid(p.latent, p.pred, p.bias, nrow = 1, scale = 0.95)
}

  

# =============================================================================
# prepare data
if (!skip.reduced){
  arc.sub <- readRDS(gpath("analysis/reduced/arc-2PL-MAP-0.005.rds"))
  gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-2PL-EAPsum-0.001.rds"))
  hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-3PL-MAP-0.01.rds"))
  mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-3PL-MAP-0.01.rds"))
  tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-2PL-EAPsum-0.01.rds"))
  wg.sub <- readRDS(gpath("analysis/reduced/WinoGrande-4PL-MAP-0.005.rds"))
} else {
  arc.sub <- readRDS(gpath("analysis/reduced/arc-4PL-MAP-0.005-v2.rds"))
  gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-2PL-EAPsum-0.005-v2.rds"))
  hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-3PL-MAP-0.005-v2.rds"))
  mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-3PL-MAP-0.001-v2.rds"))
  tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-3PL-MAP-0.001-v2.rds"))
  wg.sub <- readRDS(gpath("analysis/reduced/WinoGrande-3PL-MAP-0.001-v2.rds"))
  

}
p.mb <- readRDS(gpath("plots/metabench-sub{suffix}.rds")) +
    ggplot2::labs(y ="") +
    ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
stats.from.plot(p.mb)

# =============================================================================
# violin plots for RMSE
if (skip.reduced){
  rand.list = list(
    ARC = readRDS(gpath("data/arc-sub-145-2024-v2.rds"))$rmses.test,
    GSM8K = readRDS(gpath("data/gsm8k-sub-237.rds"))$rmses.test,
    HellaSwag = readRDS(gpath("data/hellaswag-sub-93-2024-v2.rds"))$rmses.test,
    MMLU = readRDS(gpath("data/mmlu-sub-96-2024-v2.rds"))$rmses.test,
    TruthfulQA = readRDS(gpath("data/truthfulqa-sub-154-2024-v2.rds"))$rmses.test,
    WinoGrande = readRDS(gpath("data/WinoGrande-sub-133-2024-v2.rds"))$rmses.test,
    metabench = readRDS(gpath("plots/metabench-sub-rmses-v2.rds"))$rmses.test
  )
} else {
  rand.list = list(
    ARC = readRDS(gpath("data/arc-sub-100.rds"))$rmses.test,
    GSM8K = readRDS(gpath("data/gsm8k-sub-249-2024-v2.rds"))$rmses.test,
    HellaSwag = readRDS(gpath("data/hellaswag-sub-58.rds"))$rmses.test,
    MMLU = readRDS(gpath("data/mmlu-sub-102.rds"))$rmses.test,
    TruthfulQA = readRDS(gpath("data/truthfulqa-sub-136.rds"))$rmses.test,
    WinoGrande = readRDS(gpath("data/WinoGrande-sub-106.rds"))$rmses.test,
    metabench = readRDS(gpath("plots/metabench-sub-rmses.rds"))$rmses.test
  )
}

# =============================================================================
# 6-predictor plots
d.arc <- ifelse(skip.reduced, 145, 100)
d.gsm8k <- ifelse(skip.reduced, 237, 249)
d.hs <- ifelse(skip.reduced, 93, 58)
d.mmlu <- ifelse(skip.reduced, 96, 102)
d.tfqa <- ifelse(skip.reduced, 154, 136)
d.wg <- ifelse(skip.reduced, 133, 106)

six <- readRDS(gpath("plots/mb-specific{suffix}.rds"))

p.arc <- six$arc + ggplot2::scale_color_gradientn(colors = cbp[[1]]) +
  ggplot2::labs(x = "", title = glue::glue("ARC* (d = {d.arc})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.gsm8k <- six$gsm8k + ggplot2::scale_color_gradientn(colors = cbp[[2]]) +
  ggplot2::labs(x = "", y = "", title =  glue::glue("GSM8K* (d = {d.gsm8k})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.hs <- six$hs + ggplot2::scale_color_gradientn(colors = cbp[[3]]) + 
  ggplot2::labs(x = "", y = "", title =  glue::glue("HellaSwag* (d = {d.hs})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.mmlu <- six$mmlu + ggplot2::scale_color_gradientn(colors = cbp[[4]]) + 
  ggplot2::labs(title =  glue::glue("MMLU* (d = {d.mmlu})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.tfqa <- six$tfqa + ggplot2::scale_color_gradientn(colors = cbp[[5]]) + 
  ggplot2::labs(y = "", title =  glue::glue("TruthfulQA* (d = {d.tfqa})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.wg <- six$wg + ggplot2::scale_color_gradientn(colors = cbp[[6]]) + 
  ggplot2::labs(y = "", title =  glue::glue("WinoGrande* (d = {d.wg})")) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
p.reduced.6 <- cowplot::plot_grid(
  p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg, ncol = 3)

stats.from.plot(p.arc)
stats.from.plot(p.gsm8k)
stats.from.plot(p.hs)
stats.from.plot(p.mmlu)
stats.from.plot(p.tfqa)
stats.from.plot(p.wg)
stats.from.plot(p.mb)
compare.rmses()

p.rand.6 <- plot.violin(rand.list, distance = 10.0, cbp = cbp) +
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
outpath <- gpath("figures/f.meta{suffix}.pdf")
ggplot2::ggsave(outpath, p.meta, width = 16, height = 8)

# # =============================================================================
# # reviewer-requested violin plot of 100 item subsamples from open llm leaderboard
# rand.100 = list(
#   metabench = readRDS(gpath("plots/metabench-100-rmses.rds"))$rmses.test
# )
# p.100 <- plot.violin(rand.100) +
#   ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) + 
#   ggplot2::labs(title = "100-item subsamples from entire pool") +
#   ggplot2::scale_fill_manual(values = rep("#FFFFFF", 7))
# outpath <- gpath("figures/f.100.png")
# ggplot2::ggsave(outpath, p.100, width = 6, height = 4)
# 
# # =============================================================================
# # reviewer-requested plot of nonlinear relationship between abilities and scores
# abil <- readRDS(gpath("plots/mb-ability.rds"))
# p.arc <- abil$arc +
#   ggplot2::labs(x = "", title = "ARC* (d = 100)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.gsm8k <- abil$gsm8k +
#   ggplot2::labs(x = "", y = "", title = "GSM8K* (d = 237)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.hs <- abil$hs +
#   ggplot2::labs(x = "", y = "", title = "HellaSwag* (d = 58)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.mmlu <- abil$mmlu + 
#   ggplot2::labs(title = "MMLU* (d = 102)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.tfqa <- abil$tfqa + 
#   ggplot2::labs(y = "", title = "TruthfulQA* (d = 136)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.wg <- abil$wg +
#   ggplot2::labs(y = "", title = "WinoGrande* (d = 106)") +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "cm"))
# p.abil <- cowplot::plot_grid(
#   p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg, ncol = 3)
# outpath <- gpath("figures/f.abil.pdf")
# ggplot2::ggsave(outpath, p.abil, width = 12, height = 8)

# =============================================================================
# compare versions A and B for each scenario
c.arc <- compare.versions("arc")
c.gsm8k <- compare.versions("gsm8k")
c.hs <- compare.versions("hs")
c.mmlu <- compare.versions("mmlu")
c.tfqa <- compare.versions("tfqa")
c.wg <- compare.versions("wg")
c.mb <- compare.versions.mb()
comparisons <- cowplot::plot_grid(c.arc, c.gsm8k, c.hs, c.mmlu, c.tfqa, c.wg, c.mb, ncol = 1)
outpath <- gpath("figures/f.comparison.pdf")
ggplot2::ggsave(outpath, comparisons, width = 11, height = 16)
