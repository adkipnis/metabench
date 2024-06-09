# =============================================================================
box::use(.. / analysis / utils[mkdir, gprint, gpath, mytheme, cbPalette])
here::i_am("figures/f.random.R")

# =============================================================================
# helper functions
plot.development <- function(results, benchmarks, sizes) {
  box::use(ggplot2[...])
  ggplot(results, aes(x = k, color = benchmark)) +
    geom_line(aes(y = rmse.val)) +
    geom_line(aes(y = rmse.test), linetype = "dashed") +
    facet_wrap( ~ benchmark) +
    scale_x_continuous(breaks = sizes,
                       labels = c("50", "", "150", "", "250", "", "350", "", "450", "")) +
    scale_y_continuous(limits = c(0, 4)) +
    scale_color_manual(values = cbPalette()) +
    labs(x = "Number of items", y = "RMSE") +
    mytheme() +
    theme(legend.position = "None")
}

# =============================================================================
# prepare data
benchmarks <-
  c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
sizes <- seq(50, 500, 50)
results.list <- list()
for (BM in benchmarks) {
  results <- matrix(NA, nrow = length(sizes), ncol = 4)
  results[, 1] <- BM
  results[, 2] <- sizes
  for (i in seq_along(sizes)) {
    n <- sizes[i]
    inpath <- gpath("data/{BM}-sub-{n}.rds")
    all <- readRDS(inpath)
    results[i, 3] <- min(all$rmses.val)
    results[i, 4] <- all$rmse.test
  }
  results.list[[BM]] <- results
}
results <- data.frame(do.call(rbind, results.list))
colnames(results) <- c("benchmark", "k", "rmse.val", "rmse.test")
results$k <- as.numeric(results$k)
results$rmse.val <- as.numeric(results$rmse.val)
results$rmse.test <- as.numeric(results$rmse.test)

# plot development
(p <- plot.development(results, benchmarks, sizes))
outpath <- gpath("plots/random-rmses.pdf")
ggplot2::ggsave(outpath, p, width = 8, height = 6)

