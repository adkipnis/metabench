# Evaluate the performance of the IRT models on the given benchmark
#   1. Score Recovery (Train Set)
#   2. Score Recovery (Test Set)
#
# usage: Rscript evaluate.cv.R {benchmark} {theta-method}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, get.theta])
parse.args(
   names = c("BM", "METHOD"),
   defaults = c("arc", "MAP"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
     METHOD = c("MAP", "EAPsum")
   )
)
here::i_am("analysis/evaluate.cv.R")
set.seed(1)

# =============================================================================
# helper functions  
cv.extract <- function(results, itemtype) {
   df <- results[[itemtype]]$df
   df$type <- itemtype
   df
}

cv.collect <- function(results) {
  dfs <- lapply(names(results), function(itemtype) cv.extract(results, itemtype))
  names(dfs) <- names(results)
  dfs <- do.call(rbind, dfs)
  dfs$set <- factor(dfs$set, levels = c("train", "test"))
  dfs$error <- dfs$score - dfs$p
  dfs
}

refit <- function(result, data.train, data.test){
  # load data
  model <- result$model
  df <- result$df
  train <- df |> dplyr::filter(set == "train")
  test <- df |> dplyr::filter(set == "test")
  
  # refit theta
  if (METHOD != "MAP"){
    train$theta <- get.theta(model, method = METHOD, resp = data.train)[,1]
    test$theta <- get.theta(model, method = METHOD, resp = data.test)[,1]
  }
  
  # refit gam
  mod.score <- mgcv::gam(score ~ s(theta), data = train)
  train$p <- predict(mod.score, train)
  test$p <- predict(mod.score, test)
  
  # export
  result$df <- rbind(train, test) |>
    dplyr::mutate(rank.theta = rank(theta),
                  perc.theta = rank.theta/max(rank.theta))
  result
}

refit.wrapper <- function(cvs){
  gprint("Refitting theta using {METHOD}...")
  if (BM %in% c("hellaswag", "mmlu")){
    datapath <- gpath("data/{BM}-sub.rds")
  } else {
    datapath <- gpath("data/{BM}-preproc-split.rds")
  }  
  all <- readRDS(datapath)
  data.train <- all$data.train
  data.test <- all$data.test
  cvs.re <- list()
  for (i in 1:length(cvs)){
    tryCatch({
      cvs.re[[i]] <- refit(cvs[[i]], data.train, data.test)
    }, error = function(e){
      gprint("Could not re-estimate theta for {names(cvs)[i]}")
    })
  }
  names(cvs.re) <- names(cvs)[1:length(cvs.re)]
  cvs.re[!sapply(cvs.re, is.null)]
}

evaluate.fit <- function(df.score) {
   df.score |> 
      dplyr::group_by(type, set) |>
      dplyr::summarize(
            rmse = sqrt(mean(error^2)),
            mae = mean(abs(error)),
            r = cor(theta, score, method = "spearman"),
            .groups = 'drop')
}

plot.theta.score <- function(df.score, itemtype){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |> 
      dplyr::filter(set == "test", type == itemtype)
   sfs <- evaluate.fit(df.plot)
   text <- glue::glue(
     "RMSE = {round(sfs$rmse, 3)}\nMAE = {round(sfs$mae, 3)}\nr = {round(sfs$r, 3)}")
   x.label <- 0.8 * diff(range(df.plot$theta)) + min(df.plot$theta)
   y.label <- 0.1 * diff(range(df.plot$score)) + min(df.plot$score)
   ggplot(df.plot, aes(x = theta, y = score)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = p), color = "red") +
      ylim(0,100) +
      annotate("text", x = x.label, y = y.label, label = text, size = 3) +
      labs(
         title = glue::glue("Theta vs. Score ({itemtype})"),
         x = TeX("$\\theta$"),
         y = "Score",
      ) +
      mytheme()
}

plot.perc <- function(df.score, itemtype){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
     dplyr::filter(set == "test", type == itemtype)
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
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Score Reconstruction ({itemtype})"),
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
# load cv results
cvs <- list(
   "2PL" = readRDS(gpath("analysis/models/{BM}-2PL-cv.rds")),
   "3PL" = readRDS(gpath("analysis/models/{BM}-3PL-cv.rds")),
   "3PLu" = readRDS(gpath("analysis/models/{BM}-3PLu-cv.rds")),
   "4PL" = readRDS(gpath("analysis/models/{BM}-4PL-cv.rds"))
)
if (METHOD == "EAPsum"){
  cvs <- refit.wrapper(cvs)
}
df.score <- cv.collect(cvs)

# evaluate
sfs <- evaluate.fit(df.score)
print(sfs)
p.ts <- cowplot::plot_grid(
  plot.theta.score(df.score, "2PL"),
  plot.theta.score(df.score, "3PL"),
  plot.theta.score(df.score, "3PLu"),
  plot.theta.score(df.score, "4PL"),
  nrow = 1)

p.pc <- cowplot::plot_grid(
  plot.perc(df.score, "2PL"),
  plot.perc(df.score, "3PL"),
  plot.perc(df.score, "3PLu"),
  plot.perc(df.score, "4PL"),
  nrow = 1)

p.ps <- cowplot::plot_grid(
  plot.score(df.score, "2PL"),
  plot.score(df.score, "3PL"),
  plot.score(df.score, "3PLu"),
  plot.score(df.score, "4PL"),
  nrow = 1)

p.er <- cowplot::plot_grid(
  plot.error(df.score, "2PL"),
  plot.error(df.score, "3PL"),
  plot.error(df.score, "3PLu"),
  plot.error(df.score, "4PL"),
  nrow = 1, align = "h")

p <- cowplot::plot_grid(
  p.ts, p.pc, p.ps, p.er, ncol = 1
)

# save
outpath <- gpath("plots/{BM}-cv-{METHOD}.png")
ggplot2::ggsave(outpath, p, width = 16, height = 16)
gprint("💾 Saved plot to {outpath}")

