# Get the information per item and construct a reduced test set:
#
# usage: Rscript reduce.R {benchmark} {model}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mytheme])
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
# item info
collect.item.info <- function(model, theta, itemnames){
   n <- length(theta)
   d <- model@Data[["nitems"]]
   info.items <- matrix(NA, nrow=n, ncol=d+1)
   info.items[,1] <- theta
   for (j in 1:d){info.items[,j+1] <- iteminfo(extract.item(model, j), Theta=theta)}
   info.items <- data.frame(info.items[order(theta),])
   colnames(info.items) <- c("theta", itemnames)
   return(info.items)
}

summarize.item.info <- function(info.items){
   info.items.summary <- data.frame(item=colnames(info.items)[-1])
   index <- apply(info.items, 2, which.max)[-1]
   info.items.summary$argmax <- info.items$theta[index]
   info.items.summary$max <- sapply(info.items, max)[-1]
   info.items.summary$sd <- sapply(info.items, sd)[-1]
   info.items.summary <- info.items.summary |> arrange(by=argmax)
   return(info.items.summary)
}

plot.info <- function(itemnum, new=T, ymax=10){
   func <- ifelse(new, 'plot', 'lines')
   args <- list(info.items[,1], info.items[,as.character(itemnum)],
                type='l',
                ylim=c(0,ymax),
                xlim=c(info.items$theta[1], tail(info.items$theta, n=1)),
                xlab=expression(theta),
                ylab=expression(I(theta)),
                main=paste0('item #', itemnum))
   do.call(func, args)
}

plot.testinfo <- function(model, theta, main='test info') {
  info.test <- testinfo(model, Theta = theta)
  df_tmp <- data.frame(theta=theta, info=info.test)[order(theta),]
  plot(df_tmp, type='l', lwd=3,
       xlab=expression(theta),
       ylab=expression(I(theta)),
       main=main)
}

plot.expected.testinfo <- function(info.items, index.set){
   info.items.sub <- info.items[, index.set$item]
   info.items.sub$cum <- rowSums(info.items.sub)
   lines(info.items$theta, info.items.sub$cum,
        type='l',
        main='expected subtest info',
        xlab=expression(theta),
        ylab=expression(I(theta))
   )
}

plot.info.summary <- function(model, theta, info.items, index.set){
   plot.testinfo(model, theta, main='test info (full vs. reduced)')
   plot.expected.testinfo(info.items, index.set)
}

# -----------------------------------------------------------------------------
# subtest creation

select.items <- function(info.summary, info.quantiles, n_max=6L, threshold=3.0){
   index.set <- list()
   # iterate over quantiles (get the current and next quantile)
   for (i in 1:nrow(info.quantiles)) {
      q0 <- info.quantiles$quantile[i]
      q1 <- info.quantiles$quantile[i+1]
      # get the items in the current quantile
      selection <- info.summary |>
         filter(argmax >= q0 & argmax < q1 & max >= threshold) |>
         arrange(desc(max)) |>
         head(n_max)
      index.set[[i]] <- selection
   }
   df.index <- do.call(rbind, index.set) |> distinct() %>% arrange(argmax)
   return(df.index)
}

# -----------------------------------------------------------------------------
# parameter recovery

get.estimates <- function(model){
  estimates <- coef(model, simplify=T, rotate="none")$items
  estimates <- data.frame(estimates) |>
      rownames_to_column(var='item') |>
      mutate(item = as.numeric(item))
   return(estimates)
}

compare.parameters <- function(model, model.sub){
   estimates <- get.estimates(model)
   estimates.sub <- get.estimates(model.sub)
   df.comparison <- merge(estimates, estimates.sub, by='item')
   par(mfrow=c(1,2))
   plot(d.y ~ d.x, data=df.comparison,
      main='difficulty', xlab='original', ylab='recovered')
   plot(a1.y ~ a1.x, data=df.comparison,
      main='loading', xlab='original', ylab='recovered')
   par(mfrow=c(1,1))
   r1 <- cor(df.comparison$d.x, df.comparison$d.y)
   r2 <- cor(df.comparison$a1.x, df.comparison$a1.y)
   print(glue("correlation difficulty: {round(r1, 2)}"))
   print(glue("correlation loading: {round(r2, 2)}"))
}

compare.theta <- function(theta, theta.sub){
   df.comparison <- data.frame(theta, theta.sub) |>
      arrange(by=theta) |>
      mutate(
             rank = rank(theta),
             rank.sub = rank(theta.sub),
             perc = rank/max(rank),
             perc.sub = rank.sub/max(rank.sub),
      )
   plot(perc.sub ~ perc,
        data=df.comparison,
        main="Theta (percentiles)",
        xlab="original",
        ylab="recovered")
   r <- cor(theta, theta.sub, method='spearman')
   print(glue("theta x theta - spearman correlation: {round(r, 2)}"))
}

# -----------------------------------------------------------------------------
# score modeling

# compare.score <- function(score.table){
#    r <- cor(score.table$theta, score.table$score, method='spearman')
#    gprint("Theta x Score - Spearman correlation: {round(r, 2)}")
#    par(mfrow=c(2,1))
#    plot(perc.score ~ perc.theta,
#         data=score.table,
#         main="Percentile Recovery",
#         xlab=glue::glue('% {expression(theta)}'),
#         ylab="% Full Score")
#    score.table |>
#      reframe(abs.error = abs(perc.score - perc.theta)) |> 
#      pull(abs.error) |>
#      hist(breaks=100, main='Error Distribution', xlab='Absolute Percentile Error')
#    par(mfrow=c(1,1))
# }
#
plot.perc <- function(df.score, outpath = NULL){
   box::use(ggplot2[...])
   p <- df.score |> 
      ggplot(aes(x = perc.theta, y = perc.theta)) +
         geom_point(alpha = 0.5) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "dashed") +
         labs(
            title = glue::glue("{BM} Theta percentiles vs. Score percentiles"),
            x = expression(theta),
            y = "Full Score",
            ) +
         mytheme()
   # save or print
   if (!is.null(outpath)) {
      ggsave(outpath, p, width = 8, height = 8)
      gprint("💾 Percentile plot saved to {outpath}")
   } else {
      print(p)
   }
}

# #===============================================================================
# # subtest creation
# # 2. select up to n_max items with the highest information within each quantile
# index.set <- select.items(info.items.summary, info.quantiles,
#                           n_max=5L, threshold=1.0)
# plot.info.summary(model, theta, info.items, index.set)
#
# # 3. create subtest
# data.sub <- data[, index.set$item]
# model.sub <- mirt(data.sub, 1, itemtype='2PL',
#                   method='EM', dentype='Davidian-4', TOL=1e-4,
#                   technical = list(NCYCLES=2000))
# theta.sub <- fscores(model.sub, method="MAP")
#
# # 4. check test info
# par(mfrow=c(2,1))
# plot.testinfo(model, theta)
# plot.testinfo(model.sub, theta.sub, 'test info (reduced)')
# par(mfrow=c(1,1))
#
# # 5. check recovery for parameters and theta
# compare.parameters(model, model.sub)
# compare.theta(theta, theta.sub)
#
# # 6. check score recovery
# score.table.sub <- get.score.table(theta.sub, scores)
# compare.score(score.table.sub)
#
# # 7. check exact score prediction
# score.table.sub$p <- predict(mod.score, data=data.sub)
# plot.score(score.table.sub)
# sfs.sub <- score.fit.statistics(score.table.sub)

