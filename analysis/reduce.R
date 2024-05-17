# Get the information per item and construct a reduced test set:
#   1. 
# usage: Rscript reduce.R {benchmark} {model}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme, merge.params])
parse.args(
   names = c("BM", "Model"),
   defaults = c("hellaswag", "2PL"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande"),
     Model = c("2PL", "3PL", "3PLu", "4PL")
   )
)
here::i_am("analysis/reduce.R")
set.seed(1)

# =============================================================================
# helper functions

# -----------------------------------------------------------------------------
# Score prediction
get.score.table <- function(theta, scores){
   colnames(theta) <- 'theta'
   df <- data.frame(theta=theta, score=scores) |>
      dplyr::arrange(theta) |>
      dplyr::mutate(rank.score = rank(score),
                    rank.theta = rank(theta),
                    perc.score = rank.score/max(rank.score),
                    perc.theta = rank.theta/max(rank.theta))
   mod.score <- mgcv::gam(score ~ s(theta), data = df)
   df$p <- predict(mod.score)
   df |> dplyr::mutate(error = score - p)
}

plot.theta.score <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   p <- df.score |> 
      ggplot(aes(x = theta, y = score)) +
         geom_point(alpha = 0.5) +
         labs(
            title = glue::glue("Theta vs. Score"),
            x = TeX("$\\theta$"),
            y = "Full Score",
            ) +
         mytheme() 

   # print prediction line
   if ("p" %in% colnames(df.score)) {
      p + geom_line(aes(y = p), color = "red")
   } else {
      p
   }
}

plot.perc <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   df.score |> 
      ggplot(aes(x = perc.theta, y = perc.score)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         labs(
            title = glue::glue("Percentile comparison"),
            x = TeX("$\\% \\theta$"),
            y = "% Full Score",
            ) +
         mytheme()
}

plot.score.error <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   df.score |> 
      ggplot(aes(x = p, y = error)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 0,
                     linetype = "dashed") +
         labs(
            title = glue::glue("Predicted vs. Error"),
            x = "Predicted Score",
            y = "Error",
            ) +
         mytheme()
}

plot.error.dist <- function(df.score){
   box::use(ggplot2[...], latex2exp[TeX])
   max_score <- max(df.score$score)
   df.score |> 
      ggplot(aes(x = abs(error)/max_score)) +
         geom_histogram(bins = 50, fill="white", color="black") +
         labs(
            title = glue::glue("Error Distribution"),
            x = "Relative Error",
            y = "Count",
            ) +
         mytheme()
}

score.stats <- function(df.score){
   abs.error <- abs(df.score$error)
   out <- list(
      mae = mean(abs.error),
      sd = sd(abs.error),
      max = max(abs.error),
      min = min(abs.error),
      total = sum(abs.error)
   )
   gprint("📊 Score absolute error:
          Mean: {round(out$mae, 2)}
          SD: {round(out$sd, 2)}
          Range: [{round(out$min, 2)}, {round(out$max, 2)}]
          Total: {round(out$total, 2)}")
}


