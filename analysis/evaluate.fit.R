# Evaluate the performance of the IRT models on the given benchmark
#   1. Theta and parameter distributions
#   2. Item fits (Infit, Outfit)
#   3. Model comparisons (AIC, Chi-Square)
#
# usage: Rscript evaluate.fit.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, mytheme])
parse.args(
   names = c("BM"),
   defaults = c("gsm8k"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
   )
)
here::i_am("analysis/evaluate.fit.R")
mkdir("plots")
mkdir("analysis/itemfits")
set.seed(1)

# =============================================================================
# helper functions  

plot.theta.ests <- function(results){
   box::use(ggplot2[...], latex2exp[TeX])
   n <- length(results)
   thetas <- list()
   for (i in 1:n) {
      thetas[[i]] <- data.frame(results[[i]]$theta)
      thetas[[i]]$itemtype <- names(results)[i]
   }
   do.call(rbind, thetas) |>
      ggplot(aes(x = F1)) +
      geom_density() +
      facet_wrap(~itemtype, ncol = n) +
      labs(
         title = "est. Theta Posterior",
         x = TeX("$\\theta$"),
         y = TeX("$f(\\theta)$")
      ) +
      mytheme()
   
}

plot.theta.dists <- function(results){
   box::use(ggplot2[...], latex2exp[TeX])
   n <- length(results)
   dists <- list()
   for (i in 1:n) {
      post <- results[[i]]$model@Internals$thetaPosterior$all
      # post$posterior <- post$posterior / sum(post$posterior)
      dists[[i]] <- data.frame(post, itemtype = names(results)[i])
   }
   do.call(rbind, dists) |> 
      ggplot(aes(x = Theta, y = posterior)) +
      geom_line() +
      facet_wrap(~itemtype, ncol = n) +
      labs(
         title = "unnorm. Theta Posterior",
         x = TeX("$\\theta$"),
         y = TeX("$f(\\theta)$"),
         ) +
      mytheme()

}

plot.parameters <- function(results){
   box::use(ggplot2[...], latex2exp[TeX])
   coefs <- lapply(results, function(result){
         mirt::coef(result$model, simplify = T, rotate = "none")$items
   })
   for (i in 1:length(coefs)) {
      coefs[[i]] <- data.frame(coefs[[i]])
      coefs[[i]]$itemtype <- names(results)[i]
   }
   do.call(rbind, coefs) |>
      ggplot(aes(x = d, y = a1)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~itemtype) +
      labs(
         title = "Parameter estimates",
         x = "difficulty",
         y = "loading"
      ) +
      mytheme()
}

compare.models <- function(results) {
   gprint("🔍 Comparing models...")
   model_names <- names(results)
   args <- glue::glue("results[['{model_names}']]$model")
   args <- paste(args, collapse = ", ")
   call <- paste0("mirt::anova(", args, ")")
   comparisons <- eval(parse(text = call))
   rownames(comparisons) <- model_names
   comparisons
}

summarize.comparisons <- function(comparisons) {
   gprint("📊 Summary of model comparisons...")
   best_aic <- which.min(comparisons$AIC)
   aic <- round(min(comparisons$AIC))
   best_bic <- which.min(comparisons$BIC)
   bic <- round(min(comparisons$BIC))
   gprint("Best AIC: {rownames(comparisons)[best_aic]} ({aic})")
   gprint("Best BIC: {rownames(comparisons)[best_bic]} ({bic})")
}

get.itemfit <- function(result){
   model <- result$model
   theta <- result$theta
   itemtype <- model@Model[["itemtype"]][1]
   item.fit <- mirt::itemfit(model, fit_stats = 'infit', Theta = theta) |>
      dplyr::mutate(outlier = infit <= 0.5 |
                    infit >= 1.5 |
                    outfit <= 0.5 |
                    outfit >= 1.5)
   item.fit$itemtype <- itemtype
   gprint("{itemtype} - Proportion of bad item fits: {round(100 * sum(item.fit$outlier) / nrow(item.fit), 2)}%")
   item.fit
}

wrap.itemfits <- function(results){
   gprint("⚙️  Computing item fits...")
   item.fits <- lapply(results, get.itemfit)
   item.fits <- do.call(rbind, item.fits)
   rownames(item.fits) <- NULL
   item.fits
}

plot.itemfit <- function(item.fits, fittype) {
  box::use(ggplot2[...])
  # TODO: add text info
  # summary <- item.fits |>
  #   dplyr::group_by(itemtype) |>
  #   dplyr::summarise(percentage = 100 * sum(outlier) / dplyr::n())
  #
  item.fits |> 
    ggplot(aes(x = get(fittype), y = item)) +
    facet_wrap(~itemtype) +
    geom_point(alpha = 0.25) +
    scale_x_continuous(limits = c(0, 2)) +
    geom_vline(xintercept = 0.5, color = "gray", linetype = "dashed") +
    geom_vline(xintercept = 1.5, color = "gray", linetype = "dashed") +
    labs(
       x = fittype,
       y = "index"
    ) +
    mytheme() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

plot.modelcomp <- function(comparisons) {
  box::use(ggplot2[...], scales[rescale_none])
  df.comparison <- comparisons |>
    dplyr::select(AIC, BIC, SABIC, HQ) |>
    dplyr::mutate(model = rownames(comparisons)) |>
    tidyr::gather(key = "metric", value = "value", -model)

  ymin <- min(df.comparison$value) - 0.1 * min(df.comparison$value)
  ymax <- max(df.comparison$value) + 0.1 * max(df.comparison$value)
  df.comparison |>
    ggplot(aes(x = model, y = value)) +
    facet_wrap(~metric) +
    geom_bar(stat="identity", fill = "lightgrey", color = "black") +
    scale_y_continuous(limits = c(ymin, ymax), oob = rescale_none) +
    geom_text(data = df.comparison |>
                dplyr::group_by(metric) |>
                dplyr::filter(value == min(value)),
              aes(label = "."), vjust = -0.5) +
    labs(
      x = "model",
      y = "value"
    ) +
    mytheme()
}

# =============================================================================
# load fit results
gprint("🚰 Loading {BM} fits...")
path <- gpath("analysis/models/{BM}-all.rds")
results <- readRDS(path)

# plot theta and params
p.theta <- cowplot::plot_grid(
  plot.theta.dists(results),
  plot.theta.ests(results),
  nrow = 2, align = "v"
)
p.params <- plot.parameters(results)

# analyze item fits
item.fits <- wrap.itemfits(results)
saveRDS(item.fits, gpath("analysis/itemfits/{BM}.rds"))

# plot item fits
p.itemfit <- cowplot::plot_grid(
  plot.itemfit(item.fits, "infit"),
  plot.itemfit(item.fits, "outfit"),
  nrow = 2, align = "v"
)

# compare models
comparisons <- compare.models(results)
print(comparisons)
p.comp <- plot.modelcomp(comparisons)
summarize.comparisons(comparisons)

# save plots
p <- cowplot::plot_grid(p.theta, p.params, p.itemfit, p.comp, nrow = 2, ncol = 2, labels = "AUTO")
ggplot2::ggsave(gpath("plots/{BM}-fit.png"), p, width = 16, height = 16)


