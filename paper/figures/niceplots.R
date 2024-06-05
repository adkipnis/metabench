#niceify(hs.evo) + labs(x = "Score", y = "Predicted", title = "HellaSwag")) Gather some saved figures and make them paper ready
# usage: Rscript niceplots.R

# =============================================================================
# custom utils, args, path, seed
here::i_am("paper/figures/niceplots.R")
box::use(../../analysis/utils[mkdir, gprint, gpath, mytheme], ggplot2[...])
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

# =============================================================================
papertheme <- function(){
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.border = element_rect(size = 2))
         
}
niceify <- function(p){
   p <- p + papertheme()
   p[["layers"]][[3]][["aes_params"]][["size"]] <- 5
   p[["layers"]][[3]][["data"]][["x"]] <- 75
   p[["layers"]][[3]][["data"]][["y"]] <- 25
   p
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
p.samp <- plot.evo(evo) 

# baseline comparison
hs.rand <- readRDS(gpath("paper/figures/hellaswag-random-rmses.rds"))
mmlu.rand <-  readRDS(gpath("paper/figures/mmlu-random-rmses.rds"))
rand <- data.frame(rmse = hs.rand, bm = "HellaSwag")
rand <- rbind(rand, data.frame(rmse = mmlu.rand, bm = "MMLU"))

plot.violin <- function(df){
  ggplot(df, aes(x = bm, y = rmse, fill = bm)) +
    geom_violin(draw_quantiles = c(0.5)) +
    labs(x="", y = "RMSE", title = "Random") +
    scale_fill_manual(values = cbPalette) +
    mytheme() +
    papertheme() +
    theme(legend.position = "none")
}
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

# =============================================================================
# IRT score reconstruction - reduced
plot.score <- function(result, bm, color){
   box::use(ggplot2[...])
   df.plot <- result$df.score.val |>
      dplyr::filter(set == "test")
   rmse <- get.rmse(result)
   n.items <- nrow(result$items)
   text <- glue::glue("RMSE = {round(rmse, 2)}")
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         geom_point(alpha = 0.5, color = color) +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         annotate("text", x = 75, y = 25, label = text, size = 5) +
         labs(
            title = glue::glue("{bm} (n = {n.items})"),
            x = "Score",
            y = "Predicted",
            ) +
         mytheme() +
         papertheme()
}

get.rmse <- function(result){
   df.plot <- result$df.score.val |>
      dplyr::filter(set == "test") |>
      dplyr::mutate(error = p - score)
   rmse <- sqrt(mean(df.plot$error^2))
   rmse
}

arc.sub <- readRDS(gpath("analysis/reduced/arc-4PL-EAPsum-0.01.rds"))
gsm8k.sub <- readRDS(gpath("analysis/reduced/gsm8k-4PL-EAPsum-0.01.rds"))
hs.sub <- readRDS(gpath("analysis/reduced/hellaswag-4PL-MAP-0.rds"))
mmlu.sub <- readRDS(gpath("analysis/reduced/mmlu-4PL-EAPsum-0.rds"))
tfqa.sub <- readRDS(gpath("analysis/reduced/truthfulqa-2PL-EAPsum-0.015.rds"))
wg.sub <- readRDS(gpath("analysis/reduced/winogrande-2PL-MAP-0.rds"))


p.arc <- plot.score(arc.sub, "ARC", cbPalette[1])
p.gsm8k <- plot.score(gsm8k.sub, "GSM8K", cbPalette[2])
p.hs <- plot.score(hs.sub, "HellaSwag", cbPalette[3])
p.mmlu <- plot.score(mmlu.sub, "MMLU", cbPalette[4])
p.tfqa <- plot.score(tfqa.sub, "TruthfulQA", cbPalette[5])
p.wg <- plot.score(wg.sub, "Winogrande", cbPalette[6])

(p.sub <- cowplot::plot_grid(p.arc, p.gsm8k, p.hs, p.mmlu, p.tfqa, p.wg))
outpath <- gpath("paper/figures/score.sub.pdf")
ggplot2::ggsave(outpath, p.sub, width = 12, height = 8)

# violin plots for RMSE
rand.list = list(
   ARC = readRDS(gpath("data/arc-sub-150.rds"))$rmses.val,
   GSM8K = readRDS(gpath("data/gsm8k-sub-189.rds"))$rmses.val,
   HellaSwag = readRDS(gpath("data/hellaswag-sub-200.rds"))$rmses.val,
   MMLU = readRDS(gpath("data/mmlu-sub-141.rds"))$rmses.val,
   TruthfulQA = readRDS(gpath("data/truthfulqa-sub-65.rds"))$rmses.val,
   Winogrande = readRDS(gpath("data/winogrande-sub-100.rds"))$rmses.val
)
rand.list <- lapply(rand.list, function(x) data.frame(rmse = x))
rand.list <- lapply(names(rand.list), function(x) cbind(rand.list[[x]], bm = x))
rand <- do.call(rbind, rand.list)

p.rand <- plot.violin(rand) + ylim(1, 6) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
   geom_text(aes(x = 1, y = get.rmse(arc.sub), label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 2, y = get.rmse(gsm8k.sub), label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 3, y = get.rmse(hs.sub), label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 4, y = get.rmse(mmlu.sub), label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 5, y = get.rmse(tfqa.sub), label = "*"), color = "black", size = 8) +
   geom_text(aes(x = 6, y = get.rmse(wg.sub), label = "*"), color = "black", size = 8)
# make labels of x-axis vertical
 


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
