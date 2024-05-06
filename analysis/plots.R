

packages <-
  c("tidyr",
    "dplyr",
    "tibble",
    "ggplot2",
    "here",
    "glue")
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(sapply(packages, require, character.only = T))
here::i_am("analysis/plots.R")

BM <- "arc"
Model <- "2PL"

cv.extract <- function(results, type = "train") {
  dfs <- lapply(results, function(x)
    x[[type]][["df"]])
  for (i in 1:length(dfs)) {
    dfs[[i]]$fold <- i
    dfs[[i]]$type <- type
    dfs[[i]]$model <- Model
    dfs[[i]]$benchmark <- BM
  }
  dfs
}
cv.collect <- function(results) {
  train <- cv.extract(results, "train")
  test <- cv.extract(results, "test")
  bind_rows(train, test)
}

# =============================================================================
# gather cv results
modpath <-
  here::here("analysis/models", glue("{BM}-{Model}-cv.rds"))
results <- readRDS(modpath)
results[["Model"]] <- NULL
summary <- cv.collect(results)
summary$fold <- as.factor(summary$fold)
summary$type <- factor(summary$type, levels = c("train", "test"))

# plot (prediction vs. score)
p.score <- ggplot(summary, aes(x = score, y = p, color = type)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  scale_x_continuous(limits = c(200, 1000)) +
  scale_y_continuous(limits = c(200, 1000)) +
  facet_wrap( ~ fold) +
  # colors
  scale_color_manual(values = c("train" = "gray", "test" = "orange")) +
  labs(
    title = glue("{BM} Score Reconstruction ({Model})"),
    x = "Score",
    y = "Prediction",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10),
  )
p.score

# plot distribution of absolute errors as density
summary$error <- abs(summary$score - summary$p)
p.error <- ggplot(summary, aes(x=fold, y=error, fill=type)) +
  # geom_violin() +
  geom_boxplot() +
  scale_fill_manual(values = c("train" = "gray", "test" = "orange")) +
  labs(
    title = glue("{BM} Error Distribution ({Model})"),
    x = "Fold",
    y = "Absolute Error",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10),
  )
p.error
(summary %>% group_by(type) %>% 
   summarise(mean(error), sd(error)))

