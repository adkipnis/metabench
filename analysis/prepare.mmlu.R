# Jointly fit MMLU single model on all subtests 
# 1. Perform exploratory FA on scores and discard non-unique subtests
# 2. Fit IRT model on remaining subtests
# usage: Rscript prepare.mmlu.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[mkdir, gprint, gpath, df2data, rowmerge, do.fa, mytheme])
here::i_am("analysis/prepare.mmlu.R")
mkdir(gpath("plots"))
set.seed(1)

# =============================================================================
# helper functions

collect.data <- function(datapath){
  df <- readr::read_csv(datapath, show_col_types = F)
  data <- df2data(df)
  benchmark <- gsub("mmlu_", "", gsub(".csv", "", basename(datapath)))
  colnames(data) <- paste0(benchmark, ".", colnames(data))
  data
}

collect.scores <- function(dataset){
  scores <- data.frame(rowSums(dataset))
  colnames(scores) <- gsub("\\..*", "", colnames(dataset)[1])
  scores
}

plot.unique <- function(unique){
   box::use(ggplot2[...])
   data.frame(unique = unique, id = 1:length(unique)) |>
      ggplot(aes(x=id, y=unique)) +
         geom_bar(stat="identity", fill="white", color="black") +
         xlab("Test ID (sorted)") +
         ylab("Uniqueness") +
         ggtitle("Unique variance of MMLU subtests") +
         mytheme()
}

predict.scores <- function(scores, fa.res, full.points = NULL){
  fs <- psych::factor.scores(scores, fa.res, method = "Bartlett")$scores
  colnames(fs) <- paste0("F", 1:ncol(fs))
  if (is.null(full.points)){
     full.points <- rowSums(scores)
  }
  df.scores <- data.frame(points = full.points, fs)
  formula <- paste0("points ~ ", paste0("s(", colnames(fs), ")", collapse=" + "))
  mod.score <- mgcv::gam(as.formula(formula), data = df.scores)
  df.scores$p <- predict(mod.score)
  df.scores |>
     dplyr::mutate(error = points - p,
                   F1.rank = rank(F1),
                   points.rank = rank(points),
                   F1.perc = F1.rank / max(F1.rank),
                   points.perc = points.rank / max(points.rank))
}

plot.scores <- function(df.scores, text = ""){
  box::use(ggplot2[...], latex2exp[TeX])
  x.label <- 0.9 * diff(range(df.scores$F1)) + min(df.scores$F1)
  y.label <- 0.1 * diff(range(df.scores$points)) + min(df.scores$points)
  ggplot(df.scores, aes(x = F1, y = points)) +
   geom_point(alpha = 0.5) +
   geom_line(aes(y = p), color = "red") +
   annotate("text", x = x.label, y = y.label, label = text, size = 3) +
   labs(
      x = TeX("$\\theta$"),
      y = "Full score",
      title = "FA Theta vs. Score"
   ) +
   mytheme()
}

plot.perc <- function(df.scores, text = ""){
  box::use(ggplot2[...], latex2exp[TeX])
  # get 0.9 of x range and 0.1 of y range
  x.label <- 0.9 * diff(range(df.scores$F1.perc)) + min(df.scores$F1.perc)
  y.label <- 0.1 * diff(range(df.scores$points.perc)) + min(df.scores$points.perc)
  ggplot(df.scores, aes(x = F1.perc, y = points.perc)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   annotate("text", x = x.label, y = y.label, label = text, size = 3) +
  labs(
      x = TeX("% $\\theta$"),
      y = "% Full score",
      title = "Percentile comparison"
   ) +
   mytheme()
}

evaluate.scores <- function(scores, fa.res, full.points = NULL, labels = "AUTO"){
  df.scores <- predict.scores(scores, fa.res, full.points)
  r <- cor(df.scores$points, df.scores$F1, method = "spearman")
  gprint("Spearman correlation points x F1: {round(r, 2)}")
  summary <- df.scores |> 
    dplyr::summarise(
    RMSE = sqrt(mean((points - p)^2)),
    MAE = mean(abs(points - p))
  )
  gprint("Predicting benchmark points from latent ability...
         - RMSE: {round(summary$RMSE, 3)},
         - MAE: {round(summary$MAE, 3)}")
  cowplot::plot_grid(
    plot.scores(df.scores, text = glue::glue("RMSE = {round(summary$RMSE, 2)}\nMAE = {round(summary$MAE, 2)}")),
    plot.perc(df.scores, text = glue::glue("r = {round(r,2)}")),
    labels = labels,
    nrow = 1)
}


# =============================================================================
# prepare data
gprint("🚰 Loading  MMLU data...")
mmlu.files <- list.files(gpath("data"), pattern="mmlu_.*csv", full.names=T)
mmlu.files <- mmlu.files[!grepl("prompts", mmlu.files)]
mmlu.names <- gsub("mmlu_", "", gsub(".csv", "", basename(mmlu.files)))
data.list <- lapply(mmlu.files, collect.data)
names(data.list) <- mmlu.names
scores <- Reduce(rowmerge, lapply(data.list, collect.scores))

# exploratory factor analysis
cor(scores) |>
   corrplot::corrplot(method = "color",
                      order="hclust",
                      tl.cex = 0.3,
                      tl.col = "black",
                      col.lim = c(0,1))
fa.mmlu <- do.fa(scores, 1)
p.full <- evaluate.scores(scores, fa.mmlu)

# determine unique contribution of subtests
unique <- sort(fa.mmlu$uniquenesses, decreasing=T)
plot.unique(unique)
keepers <- names(unique[1:30]) # keep first n
# keepers <- names(unique[unique > 0.1]) # alternatively: keep most informative
scores.sub <- scores[keepers]
fa.mmlu.sub <- do.fa(scores.sub, 1)
p.sub <- evaluate.scores(scores.sub, fa.mmlu.sub, full.points = rowSums(scores), labels = c("C", "D"))

# save plot
p <- cowplot::plot_grid(p.full, p.sub, align = "v", nrow = 2)
outpath <- gpath("plots/mmlu_efa.png")
ggplot2::ggsave(outpath, p, width = 8, height = 8)
gprint("💾 Saved plot to {outpath}")

# subset data
data.sub <- Reduce(rowmerge, data.list[keepers])
outpath <- gpath("data/mmlu_sub.csv")
out <- list(data = data.sub, scores.sub = scores.sub, scores = scores)
saveRDS(out, outpath)
gprint("🏁 Saved subset data to {outpath}")

