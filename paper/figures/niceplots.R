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
mmlu.evo.data <- mmlu.evo$data |> dplyr::select(means, p) |> dplyr::mutate(bm = "MMLU")
hs.evo <- readRDS(gpath("plots/hellaswag-reduced.rds"))[[3]]
hs.evo.data <- hs.evo$data |> dplyr::select(means, p) |> dplyr::mutate(bm = "HellaSwag")
evo <- rbind(mmlu.evo.data, hs.evo.data)

plot.evo <- function(df.scores){
  sfs <- df.scores |>
    dplyr::mutate(error = p - means) |>
    dplyr::group_by(bm)  |>
    dplyr::summarise(rmse = sqrt(mean(error^2)))
  rmse.1 <- sfs[sfs$bm=="MMLU",]$rmse
  rmse.2 <- sfs[sfs$bm!="MMLU",]$rmse
  idx <- df.scores$bm == "MMLU"
  df.scores$bm[idx] = glue::glue("MMLU (RMSE = {round(rmse.1,2)})")
  df.scores$bm[!idx] = glue::glue("HellaSwag (RMSE = {round(rmse.2,2)})")
  
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
      title = glue::glue("Evolutionary Subsampling")
    ) +
    # positon of the legend in left upper corner
    mytheme() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.75, 0.25))
}
plot.evo(evo) |> niceify()

#(p.evo <- cowplot::plot_grid(hs.evo, hs.rand, mmlu.evo, mmlu.rand, ncol = 2, labels = "AUTO"))
#outpath <- gpath("paper/figures/evo.pdf")
#ggplot2::ggsave(outpath, p.evo, width = 12, height = 8)
