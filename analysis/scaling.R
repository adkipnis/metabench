# =============================================================================
# load packages
packages <- c("tidyr", "dplyr", "tibble", "readr", "mirt", "here", "glue")
install.packages(setdiff(packages, rownames(installed.packages())))  
invisible(suppressMessages(sapply(packages, require, character.only=T)))

# =============================================================================
# parse args
args <- commandArgs(trailingOnly=T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "arc"
}

Model <- args[2]
if (is.na(Model)) {
  Model <- "2PL"
} else if (!Model %in% c('2PL', '3PL', '3PLu', '4PL')) {
  stop("Invalid model option.")
}

# =============================================================================
# path and seed
here::i_am("analysis/scaling.R")
if (!dir.exists(here::here("analysis/scales"))) {
  dir.create(here::here("analysis/scales"))
}
set.seed(1)

# =============================================================================
# helper functions

# -----------------------------------------------------------------------------
# itemfit
plot.itemfit <- function(item.fit) {
  par(mfrow=c(1,2))
  # infit
  plot(item ~ infit, data=item.fit, type='n', xlab="", ylab="index", main="Infit")
  abline(v=1, col="gray")
  abline(v=0.5, col="gray", lty=2)
  abline(v=1.5, col="gray", lty=2)
  
  # outfit
  plot(item ~ outfit, data=item.fit, type='n', xlab="", ylab="index", main="Outfit")
  abline(v=1, col="gray")
  abline(v=0.5, col="gray", lty=2)
  abline(v=1.5, col="gray", lty=2)
  par(mfrow=c(1,1))
}

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
   info.items.summary <- info.items.summary %>% arrange(by=argmax)
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
   # par(new=T)
   # plot(density(theta), col='red', lwd=2, lty=2, ylab='', xlab='', axes=F, main='')
   # axis(side=4)
   # mtext('density', side=4, line=3)
}

# -----------------------------------------------------------------------------
# subtest creation

get.info.quantiles <- function(info.items, steps=40){
  theta.quantiles <- quantile(info.items$theta, probs = 0:steps/steps, type=4)
  item.selection <- data.frame(quantile=theta.quantiles) %>%
    rownames_to_column(var="percent")
  item.selection$index <- findInterval(theta.quantiles, info.items$theta)
  return(item.selection)
}

plot.quantiles <- function(info.quantiles, theta) {
   n <- nrow(info.quantiles)
   plot(info.quantiles$quantile, 1:n/n,
        t='l',
        xlab=expression(theta),
        ylab=expression(F(theta)),
        main='quantiles vs. ecdf (orange)'
   )
   lines(ecdf(theta), col='darkorange')
}

select.items <- function(info.summary, info.quantiles, n_max=6L, threshold=3.0){
   index.set <- list()
   # iterate over quantiles (get the current and next quantile)
   for (i in 1:nrow(info.quantiles)) {
      q0 <- info.quantiles$quantile[i]
      q1 <- info.quantiles$quantile[i+1]
      # get the items in the current quantile
      selection <- info.summary %>%
         filter(argmax >= q0 & argmax < q1 & max >= threshold) %>%
         arrange(desc(max)) %>%
         head(n_max)
      index.set[[i]] <- selection
   }
   df.index <- do.call(rbind, index.set) %>% distinct() %>% arrange(argmax)
   return(df.index)
}

# -----------------------------------------------------------------------------
# parameter recovery

get.estimates <- function(model){
  estimates <- coef(model, simplify=T, rotate="none")$items
  estimates <- data.frame(estimates) %>%
      rownames_to_column(var='item') %>%
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
   df.comparison <- data.frame(theta, theta.sub) %>%
      arrange(by=theta) %>%
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

get.score.table <- function(theta, scores){
   colnames(theta) <- 'theta'
   df.score <- data.frame(theta=theta, score=scores) %>%
      arrange(by=theta) %>%
      mutate(rank.score = rank(score),
             rank.theta = rank(theta),
             perc.score = rank.score/max(rank.score),
             perc.theta = rank.theta/max(rank.theta))
   return(df.score)
}

plot.score <- function(df.score){
   plot(score ~ theta, data=df.score,
        xlab=expression(theta),
        ylab='Full Score',
        main='Score Recovery')
   # if p is a column in df.score
   if ('p' %in% colnames(df.score)) {
      lines(p ~ theta, data=df.score, col='red')
   }
}

compare.score <- function(score.table){
   r <- cor(score.table$theta, score.table$score, method='spearman')
   print(glue("theta x score - spearman correlation: {round(r, 2)}"))
   par(mfrow=c(2,1))
   plot(perc.score ~ perc.theta,
        data=score.table,
        main="Percentile Recovery",
        xlab=glue('% {expression(theta)}'),
        ylab="% Full Score")
   score.table %>%
     reframe(abs.error = abs(perc.score - perc.theta)) %>% 
     pull(abs.error) %>%
     hist(breaks=100, main='Error Distribution', xlab='Absolute Percentile Error')
   par(mfrow=c(1,1))
}

score.fit.statistics <- function(score.table){
   abs.error <- score.table %>%
      reframe(abs.error = abs(score - p)) %>%
      pull(abs.error)
   hist(abs.error, breaks=100, main='Error Distribution', xlab='Absolute Error')
   out <- list(
      mae = mean(abs.error),
      sd = sd(abs.error),
      max = max(abs.error),
      min = min(abs.error),
      total = sum(abs.error)
   )
   return(out)
}

# =============================================================================
# prepare data

# load data
fits <- readRDS(here::here(glue("analysis/models/{BM}-all.rds")))
data <- fits$data
scores <- rowSums(data)
model <- fits[[Model]]$model
theta <- fits[[Model]]$theta
rm(fits)

# score table
score.table <- get.score.table(theta, scores)
mod.score <- mgcv::gam(score ~ s(theta), data = score.table)
score.table$p <- predict(mod.score)
compare.score(score.table)
plot.score(score.table)
sfs <- score.fit.statistics(score.table)

# item fits
item.fit <- itemfit(model, fit_stats = 'infit', Theta = theta)
item.fit$item <- colnames(data)
plot.itemfit(item.fit)
item.bad <- item.fit %>%
   filter(infit <= 0.5 | infit >= 1.5 | outfit <= 0.5 | outfit >= 1.5)
print(glue(
  "percentage of bad items: {round(100* nrow(item.bad)/nrow(item.fit), 2)}"))

# item info
info.items <- collect.item.info(model, theta, colnames(data))
info.items <- info.items %>% select(!item.bad$item)
info.items.summary <- summarize.item.info(info.items)
# plot.info(42)

#===============================================================================
# subtest creation

# 2. decide on a range on theta quantiles
info.quantiles <- get.info.quantiles(info.items, steps=40)
# plot.quantiles(info.quantiles, theta)

# 3 select m items with the highest information in each quantile
index.set <- select.items(info.items.summary, info.quantiles,
                          n_max=10L, threshold=1.0)
plot.info.summary(model, theta, info.items, index.set)
