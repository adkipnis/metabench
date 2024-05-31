# Evaluate the performance of the IRT models on the given benchmark
#   1. Score Recovery (Train Set)
#   2. Score Recovery (Test Set)
#
# usage: Rscript evaluate.cv.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme])
parse.args(
   names = c("BM"),
   defaults = c("hellaswag"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
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

refit <- function(result, data.test){
  # load data
  model <- result$model
  df <- result$df
  train <- df |> dplyr::filter(set == "train")
  test <- df |> dplyr::filter(set == "test")
  
  # refit theta
  train$theta <- get.theta(model, method = "EAPsum")[,1]
  test$theta <- get.theta(model, method = "EAPsum", resp = data.test)[,1]
  
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
cvpath <- gpath("analysis/models/{BM}-cv.rds")
cvs <- readRDS(cvpath)
df.score <- cv.collect(cvs)

