# =============================================================================
box::use(.. / analysis / utils[mytheme, cbPalette])
cbp <- cbPalette()

# =============================================================================
# helper function to create a violin plot
viodensity <- function(x, maxval = 7.0){
   box::use(ggplot2[...])
   p <- ggplot(data.frame(x = x, y = ""), aes(x = x, y = y)) +
     geom_violin() +
     scale_x_continuous(limits = c(0.5, maxval), breaks = seq(1, maxval))
   den.x <- ggplot_build(p)$data[[1]][["x"]]
   den.y <- ggplot_build(p)$data[[1]][["density"]]
   # den.y <- den.y / max(den.y)
   data.frame(x = den.x, y = den.y)
}

data2violin <- function(data.list, distance, maxval){
   names <- names(data.list)
   den.list <- lapply(data.list, function(data) viodensity(data, maxval))
   names(den.list) <- names
   den2violin <- function(den, name){
      data.frame(x = c(den$x, rev(den$x)), y = c(den$y, -rev(den$y)), bm = name)
   }
   violin.list <- lapply(names, function(name) den2violin(den.list[[name]], name))
   for (i in 2:length(violin.list)){
     violin.list[[i]]$y <- violin.list[[i]]$y + distance * (i-1)
   }
   do.call(rbind, violin.list)
}

get.medians <- function(data.list, distance, maxval){
  names <- names(data.list)
  median.list <- lapply(data.list, function(data) stats::median(data))
  den.list <- lapply(data.list, function(data) viodensity(data, maxval))
  names(median.list) <- names
  for (i in 1:length(names)){
    name <- names[i]
    median <- median.list[[name]]
    den <- den.list[[name]]
    idx <- which.min(abs(den$x - median))
    median.list[[name]] <- data.frame(bm = name,
                                      x = median,
                                      y = -den$y[idx] + distance * (i-1),
                                      yend = den$y[idx] + distance * (i-1))
  }
  do.call(rbind, median.list)
}

#" @export
plot.violin <- function(data.list, distance = 10.0, maxval = 7){ 
  box::use(ggplot2[...])
  names <- names(data.list)
  df.violin <- data2violin(data.list, distance, maxval)
  df.violin$bm <- factor(df.violin$bm, levels = names(data.list))
  df.medians <- get.medians(data.list, distance, maxval)
  ggplot() +
    scale_x_continuous(limits = c(0.5, maxval), breaks = seq(1, maxval)) +
    geom_polygon(data = df.violin, aes(x = x, y = y, fill = bm), color = "black") +
    geom_segment(data = df.medians, aes(x = x, xend = x, y = y, yend = yend),
              color = "black", size = 0.5) +
    scale_y_reverse(breaks = seq(distance * (length(names)-1), 0, -distance),
                       labels = rev(names)) +
    scale_fill_manual(values = cbp) +
    labs(y="", x = "RMSE", title = "Random") +
    mytheme() +
    theme(plot.margin = margin(0.05, 0.05, 0.05, 0.05, "cm"),
          legend.position = "None")
}


# =============================================================================
# Generate some sample data
# library(ggplot2)
# names <- c("A", "B")
# data.list <- list(A = data.frame(x = rnorm(100, mean = 2, sd = 1), bm = "A"),
#                   B = data.frame(x = rnorm(100, mean = 3, sd = 1), bm = "B"))
# plot.violin(data.list)
# data.test <- do.call(rbind, data.list)
# ggplot(data.test, aes(x = x, y = bm)) + geom_violin(draw_quantiles = 0.5)
