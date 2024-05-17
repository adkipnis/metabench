# Evaluate the performance of the IRT models on the given benchmark
#   1. Model comparisons (AIC, Chi-Square)
#   2. Item fits (Infit, Outfit)
#
# usage: Rscript evaluate.fit.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, mkdir, gprint, gpath, mytheme])
parse.args(
   names = c("BM"),
   defaults = c("gsm8k"),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "truthfulqa", "winogrande")
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

plot.itemfit <- function(item.fits, outpath = NULL) {
   box::use(ggplot2[...])
   subroutine <- function(fittype){
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

   infits <- subroutine("infit")
   outfits <- subroutine("outfit")
   p <- cowplot::plot_grid(infits, outfits, nrow = 1)

   # save or print
   if (!is.null(outpath)) {
      ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Item fit plot saved to {outpath}")
   } else {
      print(p)
   }
}


# =============================================================================
# load fit results
gprint("🚰 Loading {BM} fits...")
path <- gpath("analysis/models/{BM}-all.rds")
results <- readRDS(path)
# TODO: plot thetas and params
comparisons <- compare.models(results)
print(comparisons)
summarize.comparisons(comparisons)
item.fits <- wrap.itemfits(results)
plot.itemfit(item.fits, gpath("plots/itemfit-{BM}.png"))
saveRDS(item.fits, gpath("analysis/itemfits/{BM}.rds"))

