# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme])
here::i_am("figures/f.info.R")

# =============================================================================
# helper functions
theta <- seq(-4, 4, 0.1)

palette <- c("#CC6666", "#9999CC", "#66CC99", "#0066CC")

solving.probability <- function(theta, b, a){
  1 / (1 + exp(-(a * theta + b)))
}

item.information <- function(theta, b, a){
  p <- solving.probability(theta, b, a)
  a^2 * p * (1 - p)
}

plot.icc <- function(difficulties, loadings, labels){
  box::use(ggplot2[...], latex2exp[TeX])
  p1 <- solving.probability(theta, difficulties[1], loadings[1])
  p2 <- solving.probability(theta, difficulties[2], loadings[2])
  p3 <- solving.probability(theta, difficulties[3], loadings[3])
  p4 <- solving.probability(theta, difficulties[4], loadings[4])
  df <- data.frame(theta = theta, p1 = p1, p2 = p2, p3 = p3, p4 = p4)
  df <- tidyr::gather(df, key = "item", value = "probability", -theta)
  ggplot(df, aes(x = theta, y = probability, color = item)) +
    geom_line(linewidth = 1) +
    labs(
      # x = TeX("$\\theta$"),
      x = "",
      y = TeX("$\\P(correct)$"),
      color = "") +
    scale_color_manual(values = palette,
                       labels = labels) +
    mytheme() +
    theme(legend.position = "inside", legend.position.inside = c(0.75,0.25),
          legend.key = element_rect(fill = NA, color = NA),
          plot.margin = margin(0, 0, -0.5, 0, "cm"))
}

plot.item.information <- function(difficulties, loadings){
  box::use(ggplot2[...], latex2exp[TeX])
  
  i1 <- item.information(theta, difficulties[1], loadings[1])
  i2 <- item.information(theta, difficulties[2], loadings[2])
  i3 <- item.information(theta, difficulties[3], loadings[3])
  i4 <- item.information(theta, difficulties[4], loadings[4])
  df <- data.frame(theta = theta, i1 = i1, i2 = i2, i3 = i3, i4 = i4)
  df <- tidyr::gather(df, key = "item", value = "information", -theta)
  ggplot(df, aes(x = theta, y = information, color = item)) +
    geom_line(linewidth = 1) +
    labs(x = TeX("$\\vartheta$"),
         y = TeX("$I(\\vartheta)$"),
         color = "Item") +
    scale_color_manual(values = palette) +
    mytheme() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm"))
}

# ==============================================================================
difficulties <- c(2, 1, 0, 0)
loadings <- c(1, 1, 2.5, 0.5)
(p.icc1 <- plot.icc(difficulties, loadings,
                    c("d = 2  ", "d = 1  ", "a = 2.5  ", "a = 0.5")))
(p.icc2 <- plot.item.information(difficulties, loadings))
(p.icc <- cowplot::plot_grid(p.icc1, p.icc2,
                             ncol = 1, rel_heights = c(2, 1.7), align = "v"))
outpath <- gpath("figures/f.info.pdf")
ggplot2::ggsave(outpath, p.icc, width = 5.5, height = 8)

