box::use(./utils[mytheme])

#" @export
score.stats <- function(df.score){
  df.score |>
    dplyr::filter(set == "test") |>
    dplyr::summarise(
      mae = mean(abs(error)),
      ub = mean(abs(error)) + 1.96 * stats::sd(abs(error)),
      rmse = sqrt(mean(error^2)),
      sse = sum(error^2),
      r = stats::cor(theta, score, method = "spearman")
    )
}

#" @export
compare.score.stats <- function(sfs, sfs.sub){
   out <- list()
   for (key in names(sfs)) {
      out[[key]] <- sfs.sub[[key]] - sfs[[key]]
   }
   gprint("📊 Score error change (subtest - full, negative means improvement):
          Δ RMSE: {round(out$rmse, 3)}
          Δ MAE: {round(out$mae, 3)}
          Δ (MAE + 1.96 SD): {round(out$ub, 3)}
          Δ Total SSE: {round(out$sse, 3)}")
}

compare.parameters <- function(model, model.sub){
   get.estimates <- function(model){
      mirt::coef(model, simplify=T, rotate="none")$items |>
         data.frame() |>
         tibble::rownames_to_column(var='item')
   }
   estimates <- get.estimates(model)
   estimates.sub <- get.estimates(model.sub)
   df.comparison <- merge(estimates, estimates.sub, by='item')
   r1 <- stats::cor(df.comparison$d.x, df.comparison$d.y)
   r2 <- stats::cor(df.comparison$a1.x, df.comparison$a1.y)
   df.comparison
}

#" @export
plot.theta.score <- function(df.score, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |> dplyr::filter(set == "test")
   sfs <- score.stats(df.plot)
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
         title = glue::glue("Theta vs. Score {suffix}"),
         x = TeX("$\\theta$"),
         y = "Score",
      ) +
      mytheme()
}

#" @export
plot.perc <- function(df.score, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
     dplyr::filter(set == "test")
   ggplot(df.plot, aes(x = 100 * perc.theta, y = 100 * perc.score)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0,
                  slope = 1,
                  linetype = "dashed") +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Percentile Comparison {suffix}"),
            x = TeX("$\\% \\theta$"),
            y = "% Score",
            ) +
         mytheme()
}

#" @export
plot.score <- function(df.score, suffix = ""){
   box::use(ggplot2[...])
   df.plot <- df.score |>
      dplyr::filter(set == "test")
   ggplot(df.plot, aes(x = score, y = p)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
         labs(
            title = glue::glue("Score Reconstruction {suffix}"),
            x = "Score",
            y = "Predicted",
            ) +
         mytheme()
}

#" @export
plot.score.error <- function(df.score, suffix = "", ylim = NULL){
   box::use(ggplot2[...], latex2exp[TeX])
   df.plot <- df.score |>
      dplyr::filter(set == "test")
   ymax <- ifelse(is.null(ylim), max(abs(df.plot$error)), ylim)
   ggplot(df.plot, aes(x = theta, y = error)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 0,
                     linetype = "dashed") +
         coord_cartesian(ylim = c(-ymax, ymax)) +
         labs(
            title = glue::glue("Theta vs. Error {suffix}"),
            x = "Theta",
            y = "Error",
            ) +
         mytheme()
}

#" @export
plot.theta <- function(theta, suffix=""){
   box::use(ggplot2[...], latex2exp[TeX])
   as.data.frame(theta) |> 
      ggplot(aes(x = F1)) +
         geom_density(color="black") +
         labs(
            title = glue::glue("Theta Distribution ({suffix})"),
            x = TeX("$\\theta$"),
            y = TeX("$f(\\theta)$"),
            ) +
         mytheme()
}

#" @export
plot.testinfo <- function(model, theta, ylim=NULL, title="Testinfo") {
   box::use(ggplot2[...], latex2exp[TeX])
   info.test <- mirt::testinfo(model, Theta = theta)
   ymax <- ifelse(is.null(ylim), max(info.test), ylim)
   data.frame(theta=theta, info=info.test) |>
      ggplot(aes(x = F1, y = info)) +
      # increase linewidth
         geom_line(linewidth = 1) +
         ylim(0, ymax) +
         labs(
            title = title,
            x = TeX("$\\theta$"),
            y = TeX("$I(\\theta)$"),
            ) +
         mytheme()
}

#" @export
plot.expected.testinfo <- function(info.items, index.set, ylim=NULL, title="Expected Testinfo"){
   box::use(ggplot2[...], latex2exp[TeX])
   quantiles <- info.items$theta
   info.sub <- info.items[, as.character(index.set$item)]
   info.sub$cum <- rowSums(info.sub)
   info.sub$theta <- quantiles
   ymax <- ifelse(is.null(ylim), max(info.sub$cum), ylim)
   info.sub |>
      ggplot(aes(x = theta, y = cum)) +
         geom_line() +
         ylim(0, ymax) +
         labs(
            title = title,
            x = TeX("$\\theta$"),
            y = TeX("$I(\\theta)$"),
            ) +
         mytheme()
}

#" @export
plot.quantiles <- function(info.quantiles, theta) {
   box::use(ggplot2[...], latex2exp[TeX])
   n <- nrow(info.quantiles)
   info.ecdf <- info.quantiles
   info.ecdf$F <- ecdf(theta)(info.ecdf$quantile)
   info.ecdf$type <- "ecdf"
   info.quantiles$F <- 1:n/n
   info.quantiles$type <- "quantile"
   rbind(info.ecdf, info.quantiles) |> 
      ggplot(aes(x=quantile, y=F, color=type)) +
         geom_line() +
         scale_color_manual(values=c("darkorange", "black")) +
         labs(
            x = TeX("$\\theta$"),
            y = TeX("$F(\\theta)$"),
            title = "Quantiles vs. ECDF",
            fill = "source"
         ) +
         mytheme()
}

#" @export
plot.recovery.d <- function(df.comparison){
   box::use(ggplot2[...], latex2exp[TeX])
   df.comparison |> 
      ggplot(aes(x = d.x, y = d.y)) +
         geom_point(alpha = 0.5) +
         labs(
            title = "Difficulty Recovery",
            x = "Full Difficulty",
            y = "Subtest Difficulty",
            ) +
         mytheme()
}

#" @export
plot.recovery.a1 <- function(df.comparison){
   box::use(ggplot2[...], latex2exp[TeX])
   df.comparison |> 
      ggplot(aes(x = a1.x, y = a1.y)) +
         geom_point(alpha = 0.5) +
         labs(
            title = "Loading Recovery",
            x = "Full Loading",
            y = "Subtest Loading",
            ) +
         mytheme()
}

#" @export
plot.estimates <- function(model, model.sub, theta.train, theta.sub){
   param.compare <- compare.parameters(model, model.sub)
   cowplot::plot_grid(
      plot.theta(theta.train, "Original"),
      plot.recovery.d(param.compare),
      plot.theta(theta.sub, "Reduced"),
      plot.recovery.a1(param.compare),
      align = "v"
   )
}
