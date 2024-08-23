# Evaluate the performance of the IRT models on the given benchmark
#   1. Score Recovery (Train Set)
#   2. Score Recovery (Test Set)
#
# usage: Rscript evaluate.cv.R {benchmark} {theta-method} {dimension}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, get.theta])
box::use(./utils[parse.args, gprint, gpath, rowmerge, mytheme, get.theta])
parse.args(
   names = c("BM", "METH", "DIM"),
   defaults = c("arc", "MAP", 1),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     METH = c("MAP", "EAPsum"),
     DIM = c(1, 2)
   )
)
here::i_am("analysis/evaluate.cv.R")
set.seed(1)

# =============================================================================
# helper functions  
cv.extract <- function(results, itemtype) {
   df <- results[[itemtype]]$df
   df$type <- itemtype
   rowmerge(df, leaderboard)
}

cv.collect <- function(results) {
  dfs <- lapply(names(results), function(itemtype) cv.extract(results, itemtype))
  names(dfs) <- names(results)
  dfs <- do.call(rbind, dfs)
  dfs$set <- factor(dfs$set, levels = c("train", "test"))
  dfs$error <- dfs$score - dfs$p
  dfs
}

fit.gam <- function(df.train){
  # get columns that start with F
  if ("F2" %in% colnames(df.train)){
    formula <- "score ~ s(F1, bs = 'ad') + s(F2, bs = 'ad')"
  } else {
    formula <- "score ~ s(F1, bs = 'ad')"
  }
  mgcv::gam(as.formula(formula), data = df.train)
}

refit <- function(result, data.train, data.test){
  # load data
  model <- result$model
  df <- result$df
  train <- df |> dplyr::filter(set == "train") |> dplyr::select(-F1)
  test <- df |> dplyr::filter(set == "test") |> dplyr::select(-F1)
  if ("SE_F1" %in% colnames(train)){
    train <- train |> dplyr::select(-SE_F1)
    test <- test |> dplyr::select(-SE_F1)
  }
  
  # refit theta
  theta.train <- get.theta(model, method = METH, resp = data.train)
  train <- cbind(train, theta.train[, 1, drop = F])
  theta.test <- get.theta(model, method = METH, resp = data.test)
  test <- cbind(test, theta.test[, 1, drop = F])
  
  # refit gam
  mod.score <- fit.gam(train)
  train$p <- predict(mod.score, train)
  test$p <- predict(mod.score, test)
  
  # export
  result$df <- rbind(train, test) |>
    dplyr::mutate(rank.theta = rank(F1),
                  perc.theta = rank.theta/max(rank.theta))
  result
}

refit.wrapper <- function(cvs){
  gprint("Refitting theta using {METH}...")
  datapath <- gpath("data/{BM}-sub-350.rds")
  all <- readRDS(datapath)
  data.train <- all$data.train
  data.test <- all$data.test
  cvs.re <- list()
  for (i in 1:length(cvs)){
    tryCatch({
      cvs.re[[i]] <- refit(cvs[[i]], data.train, data.test)
    }, error = function(e){
      print(e)
      gprint("Could not re-estimate theta for {names(cvs)[i]}")
    })
  }
  names(cvs.re) <- names(cvs)[1:length(cvs.re)]
  cvs.re[!sapply(cvs.re, is.null)]
}

evaluate.fit <- function(df.score) {
   out <-df.score |> 
      dplyr::group_by(type, set) |>
      dplyr::summarize(
            rmse = sqrt(mean(error^2)),
            mae = mean(abs(error)),
            r = cor(F1, score, method = "spearman"),
            r2 = ifelse("F2" %in% colnames(df.score),
                        cor(F2, score, method = "spearman"), NA),
            .groups = 'drop')
}

plot.theta.score <- function(df.score, itemtype){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |> 
      dplyr::filter(set == "test", type == itemtype)
   sfs <- evaluate.fit(df.plot)
   text <- glue::glue(
     "r = {round(sfs$r, 3)}")
   x.label <- 0.8 * diff(range(df.plot$F1)) + min(df.plot$F1)
   y.label <- 0.1 * diff(range(df.plot$score)) + min(df.plot$score)
   ggplot(df.plot, aes(x = F1, y = score)) +
      geom_point(alpha = 0.5) +
      ylim(0,100) +
      annotate("text", x = x.label, y = y.label, label = text, size = 3) +
      labs(
         title = glue::glue("Theta vs. Score ({itemtype})"),
         x = TeX("$\\theta$"),
         y = "Score",
      ) +
      mytheme()
}

plot.theta2d <- function(df.score, itemtype){
  box::use(ggplot2[...], latex2exp[TeX])
  df.plot <- df.score |> 
    dplyr::filter(set == "test", type == itemtype)
  ggplot(df.plot, aes(x = F1, y = F2, color = score, size = size)) +
    geom_point(alpha = 0.5)+ 
    labs(
      title = glue::glue("{BM} 2-dim ability"),
      x = TeX("$\\theta 1$"),
      y = TeX("$\\theta 2$"),
    ) +
    # more contrasting colors
    scale_color_viridis_c(option = "magma") +
    mytheme()
}

plot.perc <- function(df.score, itemtype){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
      dplyr::filter(type == itemtype) |>
     dplyr::mutate(rank.theta = rank(F1),
                   perc.theta = rank.theta/max(rank.theta),
                   rank.score = rank(score),
                   perc.score = rank.score/max(rank.score)) |>
     dplyr::filter(set == "test") 
   ggplot(df.plot, aes(x = 100 * perc.theta, y = 100 * perc.score)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0,
                  slope = 1,
                  linetype = "dashed") +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Percentile Comparison ({itemtype})"),
            x = TeX("$\\% \\theta$"),
            y = "% Score",
            ) +
         mytheme()
}


plot.score <- function(df.score, itemtype){
   box::use(ggplot2[...])
   df.plot <- df.score |>
      dplyr::filter(set == "test", type == itemtype)
   sfs <- evaluate.fit(df.plot)
   text <- glue::glue("RMSE = {round(sfs$rmse, 3)}")
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         geom_point(alpha = 0.5) +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         annotate("text", x = 75, y = 25, label = text, size = 3) +
         labs(
            title = glue::glue("{BM} ({itemtype}-{METH}-{DIM})"),
            x = "Score",
            y = "Predicted",
            ) +
         mytheme()
}

plot.error <- function(df.score, itemtype){
  box::use(ggplot2[...])
  df.plot <- df.score |>
    dplyr::filter(set == "test", type == itemtype) |>
    dplyr::mutate(ae = abs(error))
  ggplot(df.plot, aes(x = ae)) +
   # histogram
     geom_histogram(aes(x = ae), bins = 20, fill = "white", color = "black") +
      coord_cartesian(xlim = c(0, 100)) +
         labs(
            title = glue::glue("Error Distribution ({itemtype})"),
            x = "Absolute Error",
            y = "Frequency"
            ) +
         mytheme()
}
# =============================================================================
# Leaderboard
leaderboard <- read.csv(gpath("scraping/open-llm-leaderboard.csv"))
rownames(leaderboard) <- leaderboard$name
leaderboard <- leaderboard |> dplyr::select(size) |> dplyr::filter(size > 0)

# =============================================================================
# load cv results
if (DIM == 1){
  cvs <- list(
   "2PL" = readRDS(gpath("analysis/models/{BM}-2PL-{DIM}-cv.rds")),
   "3PL" = readRDS(gpath("analysis/models/{BM}-3PL-{DIM}-cv.rds")),
   "4PL" = readRDS(gpath("analysis/models/{BM}-4PL-{DIM}-cv.rds"))
   )
} else {
  cvs <- list(
    "2PL" = readRDS(gpath("analysis/models/{BM}-2PL-{DIM}-cv.rds"))
  )
}
if (METH == "EAPsum"){
  cvs <- refit.wrapper(cvs)
}
df.score <- cv.collect(cvs)

# evaluate
sfs <- evaluate.fit(df.score)
print(sfs)

# main paper plots
p.2 <- plot.score(df.score, "2PL")
p.3 <- plot.score(df.score, "3PL")
p.4 <- plot.score(df.score, "4PL")
saveRDS(list(p.2, p.3, p.4), gpath("plots/{BM}-{METH}-{DIM}-cv.rds"))

# overview plots
p.ps <- cowplot::plot_grid(
   p.2, p.3, p.4,
  nrow = 1)

p.ts <- cowplot::plot_grid(
  plot.theta.score(df.score, "2PL"),
  plot.theta.score(df.score, "3PL"),
  plot.theta.score(df.score, "4PL"),
  nrow = 1)

p.pc <- cowplot::plot_grid(
  plot.perc(df.score, "2PL"),
  plot.perc(df.score, "3PL"),
  plot.perc(df.score, "4PL"),
  nrow = 1)

p.er <- cowplot::plot_grid(
  plot.error(df.score, "2PL"),
  plot.error(df.score, "3PL"),
  plot.error(df.score, "4PL"),
  nrow = 1, align = "h")

p <- cowplot::plot_grid(
  p.ps, p.ts, p.pc,  p.er, ncol = 1
)

# scatter plot for 2dim models
if (DIM == 2){
  p2d <- plot.theta2d(df.score, "2PL")
  outpath <- gpath("plots/{BM}-{METH}-2d-theta.png")
  ggplot2::ggsave(outpath, p2d, width = 8, height = 8)
}

# save
outpath <- gpath("plots/{BM}-{METH}-{DIM}-cv.png")
ggplot2::ggsave(outpath, p, width = 16, height = 16)
gprint("💾 Saved plot to {outpath}")

