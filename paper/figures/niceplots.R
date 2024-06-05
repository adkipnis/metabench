#niceify(hs.evo) + labs(x = "Score", y = "Predicted", title = "HellaSwag")) Gather some saved figures and make them paper ready
# usage: Rscript niceplots.R

# =============================================================================
# custom utils, args, path, seed
here::i_am("paper/figures/niceplots.R")
box::use(../../analysis/utils[mkdir, gprint, gpath, mytheme], ggplot2[...])
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF")

# =============================================================================
papertheme <- function(){
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.border = element_rect(size = 2))
         
}
niceify <- function(p){
   p <- p + papertheme()
   p[["layers"]][[3]][["aes_params"]][["size"]] <- 5
   p[["layers"]][[3]][["data"]][["x"]] <- 75
   p[["layers"]][[3]][["data"]][["y"]] <- 25
   p
}

get.rmse <- function(result){
  df.plot <- result$df.score.val |>
    dplyr::filter(set == "test") |>
    dplyr::mutate(error = p - score)
  rmse <- sqrt(mean(df.plot$error^2))
  rmse
}

plot.evo <- function(df.scores){
  box::use(ggplot2[...], latex2exp[TeX])
  
  ggplot(df.scores, aes(x = means, y = p, color = bm)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9")) +
    labs(
      x = "Score",
      y = "Predicted",
      color = "",
      title = glue::glue("Evolutionary")
    ) +
    # positon of the legend in left upper corner
    mytheme() +
    papertheme() +
    theme(legend.position = "None")
}

plot.score <- function(result, bm, color){
  box::use(ggplot2[...])
  df.plot <- result$df.score.val |>
    dplyr::filter(set == "test")
  rmse <- get.rmse(result)
  r <- cor(df.plot$theta, df.plot$score, method = "spearman")
  n.items <- nrow(result$items)
  text <- glue::glue("RMSE = {round(rmse, 3)},\nr = {round(r, 3)}")
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
    mytheme() +
    papertheme()
}

plot.violin <- function(df){
  ggplot(df, aes(y = bm, x = rmse, fill = bm)) +
    geom_violin(draw_quantiles = c(0.5)) +
    labs(y="", x = "RMSE", title = "Random") +
    scale_fill_manual(values = cbPalette) +
    # add colorgradient to the last violin plot
    scale_color_gradientn(colors = cbPalette) +
    scale_y_discrete(limits=rev) +
    mytheme() +
    papertheme() +
    theme(legend.position = "none")
}

# =============================================================================
# evlolutionary algorithm
mmlu.evo <- readRDS(gpath("plots/mmlu-reduced.rds"))[[3]]
mmlu.evo.data <- mmlu.evo$data |> dplyr::select(means, p) |> dplyr::mutate(bm = "MMLU")
hs.evo <- readRDS(gpath("plots/hellaswag-reduced.rds"))[[3]]
hs.evo.data <- hs.evo$data |> dplyr::select(means, p) |> dplyr::mutate(bm = "HellaSwag")
evo <- rbind(mmlu.evo.data, hs.evo.data)
sfs <- evo |>
    dplyr::mutate(error = p - means) |>
    dplyr::group_by(bm)  |>
    dplyr::summarise(rmse = sqrt(mean(error^2)))

rmse.mmlu <- sfs[sfs$bm=="MMLU",]$rmse
rmse.hs <- sfs[sfs$bm!="MMLU",]$rmse


p.samp <- plot.evo(evo) 

# baseline comparison
hs.rand <- readRDS(gpath("paper/figures/hellaswag-random-rmses.rds"))
mmlu.rand <-  readRDS(gpath("paper/figures/mmlu-random-rmses.rds"))
rand <- data.frame(rmse = hs.rand, bm = "HellaSwag")
rand <- rbind(rand, data.frame(rmse = mmlu.rand, bm = "MMLU"))


p.rand <- plot.rand(rand) +
   geom_text(aes(x = 1, y = rmse.hs, label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 2, y = rmse.mmlu, label = "*"), color = "black", size = 8) +
   ylim(0.5, 0.9) +

(p.evo <- cowplot::plot_grid(p.samp, p.rand))
outpath <- gpath("paper/figures/evo.pdf")
ggplot2::ggsave(outpath, p.evo, width = 7, height = 5)

# =============================================================================
# IRT score reconstruction
arc.irt <- readRDS(gpath("plots/arc-EAPsum-1-cv.rds"))[[2]] |> niceify() + labs(title = "ARC")
gsm8k.irt <- readRDS(gpath("plots/gsm8k-MAP-2-cv.rds"))[[1]] |> niceify() + labs(title = "GSM8K")
hs.irt <- readRDS(gpath("plots/hellaswag-MAP-1-cv.rds"))[[2]] |> niceify() + labs(title = "HellaSwag")
mmlu.irt <- readRDS(gpath("plots/mmlu-EAPsum-1-cv.rds"))[[3]] |> niceify() + labs(title = "MMLU")
tfqa.irt <- readRDS(gpath("plots/truthfulqa-EAPsum-1-cv.rds"))[[1]] |> niceify() + labs(title = "TruthfulQA")
wg.irt <- readRDS(gpath("plots/winogrande-EAPsum-1-cv.rds"))[[2]] |> niceify() + labs(title = "Winogrande")
(p.irt <- cowplot::plot_grid(arc.irt, gsm8k.irt, hs.irt, mmlu.irt, tfqa.irt, wg.irt))
outpath <- gpath("paper/figures/score.full.pdf")
ggplot2::ggsave(outpath, p.irt, width = 12, height = 8)

arc.rmse <- readRDS(gpath("data/arc-sub.rds"))$rmse.test
gsm8k.rmse <- readRDS(gpath("data/gsm8k-sub.rds"))$rmse.test
hs.rmse <- readRDS(gpath("data/hellaswag-sub.rds"))$rmse.test
mmlu.rmse <- readRDS(gpath("data/mmlu-sub.rds"))$rmse.test
tfqa.rmse <- readRDS(gpath("data/truthfulqa-sub.rds"))$rmse.test
wg.rmse <- readRDS(gpath("data/winogrande-sub.rds"))$rmse.test


# =============================================================================
# IRT score reconstruction - reduced

arc.sub <- readRDS(gpath("analysis/reduced/arc-4PL-EAPsum-0.01.rds"))
gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-4PL-EAPsum-0.01.rds"))
hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-4PL-MAP-0.rds"))
mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-4PL-EAPsum-0.rds"))
tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-2PL-EAPsum-0.015.rds"))
wg.sub <- readRDS(gpath("analysis/reduced/winogrande-2PL-MAP-0.rds"))


p.arc <- plot.score(arc.sub, "ARC", cbPalette[1]) + labs(x = "")
p.gsm8k <- plot.score(gsm8k.sub, "GSM8K", cbPalette[2]) + labs(x = "", y = "")
p.hs <- plot.score(hs.sub, "HellaSwag", cbPalette[3]) + labs(x = "", y = "")
p.mmlu <- plot.score(mmlu.sub, "MMLU", cbPalette[4])
p.tfqa <- plot.score(tfqa.sub, "TruthfulQA", cbPalette[5]) + labs(y = "")
p.wg <- plot.score(wg.sub, "Winogrande", cbPalette[6]) + labs(y = "")
p.mb <- readRDS(gpath("plots/meta-prediction.rds"))[[2]] + papertheme() + labs(y ="", title = "metabench (d = 845)")
rmse.mb <- as.character(p.mb[["layers"]][[3]][["computed_geom_params"]][["label"]]) 
rmse.mb <- gsub("\n.*", "", rmse.mb)
rmse.mb <- as.numeric(gsub("[^0-9.]", "", rmse.mb))

(p.sub <- cowplot::plot_grid(p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg))

# violin plots for RMSE
rand.list = list(
   ARC = readRDS(gpath("data/arc-sub-150.rds"))$rmses.test,
   GSM8K = readRDS(gpath("data/gsm8k-sub-189.rds"))$rmses.test,
   HellaSwag = readRDS(gpath("data/hellaswag-sub-200.rds"))$rmses.test,
   MMLU = readRDS(gpath("data/mmlu-sub-141.rds"))$rmses.test,
   TruthfulQA = readRDS(gpath("data/truthfulqa-sub-65.rds"))$rmses.test,
   Winogrande = readRDS(gpath("data/winogrande-sub-100.rds"))$rmses.test,
   metabench = readRDS(gpath("data/meta-random-rmses.rds"))$rmses.test
)
rand.list <- lapply(rand.list, function(x) data.frame(rmse = x))
rand.list <- lapply(names(rand.list), function(x) cbind(rand.list[[x]], bm = x))
rand <- do.call(rbind, rand.list)
rand$bm <- factor(rand$bm, levels = c("ARC", "GSM8K", "HellaSwag", "MMLU", "TruthfulQA", "Winogrande", "metabench"))


# percentage of entries that are smaller than 
arc.better <- sum(get.rmse(arc.sub) <= rand.list[[1]]$rmse) / nrow(rand.list[[1]])
gsm8k.better <- sum(get.rmse(gsm8k.sub) <= rand.list[[2]]$rmse) / nrow(rand.list[[2]])
hs.better <- sum(get.rmse(hs.sub) <= rand.list[[3]]$rmse) / nrow(rand.list[[3]])
mmlu.better <- sum(get.rmse(mmlu.sub) <= rand.list[[4]]$rmse) / nrow(rand.list[[4]])
tfqa.better <- sum(get.rmse(tfqa.sub) <= rand.list[[5]]$rmse) / nrow(rand.list[[5]])
wg.better <- sum(get.rmse(wg.sub) <= rand.list[[6]]$rmse) / nrow(rand.list[[6]])



ds = 4.5
v =0.8
color = "#444444"
la = "*"
p.rand <- plot.violin(rand) + scale_x_continuous(limits=c(0.5,6), breaks = seq(1,6)) +
   # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
   geom_text(aes(y = 7, x = get.rmse(arc.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 6, x = get.rmse(gsm8k.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 5, x = get.rmse(hs.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 4, x = get.rmse(mmlu.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 3, x = get.rmse(tfqa.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 2, x = get.rmse(wg.sub), label = la), color = color, size = ds, vjust = v) +
   geom_text(aes(y = 1, x = rmse.mb, label = la), color = color, size = ds, vjust = v) +
   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
p.rand
(p.right <- cowplot::plot_grid(p.rand, p.mb, ncol = 1, align= "v", labels = c("B", "C"), label_x = 0.05))
(p.sub.combined <- cowplot::plot_grid(p.sub, p.right, nrow = 1, label_x = 0,
                                      labels = c("A", NA), rel_widths = c(3, 1)))
outpath <- gpath("paper/figures/score.sub.pdf")
ggplot2::ggsave(outpath, p.sub.combined, width = 15, height = 8, dpi = 500)

# =============================================================================
# IRT score reconstruction - meta

# =============================================================================
# item characteristic curves
plot.icc <- function(difficulties, loadings, labels){
   box::use(latex2exp[TeX])
   solving.probability <- function(theta, b, a){
      1 / (1 + exp(-(a * theta + b)))
   }
   theta <- seq(-4, 4, 0.1)
   p1 <- solving.probability(theta, difficulties[1], loadings[1])
   p2 <- solving.probability(theta, difficulties[2], loadings[2])
   p3 <- solving.probability(theta, difficulties[3], loadings[3])
   p4 <- solving.probability(theta, difficulties[4], loadings[4])
   df <- data.frame(theta = theta, p1 = p1, p2 = p2, p3 = p3, p4 = p4)
   df <- tidyr::gather(df, key = "item", value = "probability", -theta)
   ggplot(df, aes(x = theta, y = probability, color = item)) +
      geom_line(linewidth = 1) +
      labs(x = TeX("$\\theta$"),
           y = TeX("$\\P(correct)$"),
           color = "") +
      scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"), labels = labels) +
      mytheme() +
     papertheme() +
     #legend is on top and colors are indicated by dots, not lines
     theme( legend.position = "bottom", legend.key = element_rect(fill = "white", color = "white"))
}

plot.item.information <- function(difficulties, loadings){
   box::use(latex2exp[TeX])
   solving.probability <- function(theta, b, a){
      1 / (1 + exp(-(a * theta + b)))
   }
   item.information <- function(theta, b, a){
      p <- solving.probability(theta, b, a)
      a^2 * p * (1 - p)
   }
   theta <- seq(-4, 4, 0.1)
   i1 <- item.information(theta, difficulties[1], loadings[1])
   i2 <- item.information(theta, difficulties[2], loadings[2])
   i3 <- item.information(theta, difficulties[3], loadings[3])
   i4 <- item.information(theta, difficulties[4], loadings[4])
   df <- data.frame(theta = theta, i1 = i1, i2 = i2, i3 = i3, i4 = i4)
   df <- tidyr::gather(df, key = "item", value = "information", -theta)
   ggplot(df, aes(x = theta, y = information, color = item)) +
      geom_line(linewidth = 1) +
      labs(x = TeX("$\\theta$"),
           y = TeX("$I(\\theta)$"),
           color = "Item") +
      scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
      mytheme() +
      papertheme() +
     theme(legend.position = "none")
}
difficulties <- c(2, 1, 0, 0)
loadings <- c(1, 1, 2.5, 0.5)
(p.icc1 <- plot.icc(difficulties, loadings, c("d = 2  ", "d = 1  ", "a = 2.5  ", "a = 0.5")))
(p.icc2 <- plot.item.information(difficulties, loadings))
(p.icc <- cowplot::plot_grid(p.icc1, p.icc2, ncol = 1, rel_heights = c(2, 1.7), align = "v"))
outpath <- gpath("paper/figures/icc.pdf")
ggplot2::ggsave(outpath, p.icc, width = 5.5, height = 8)
