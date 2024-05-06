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

plot.itemfit <- function(item.fit) {
  par(mfrow=c(1,2))
  # infit
  plot(y=item.fit$item, x=item.fit$infit, xlab="", ylab="index", main="Infit")
  abline(v=1, col="gray")
  abline(v=0.5, col="gray", lty=2)
  abline(v=1.5, col="gray", lty=2)
  
  # outfit
  plot(y=item.fit$item, x=item.fit$outfit, xlab="", ylab="index", main="Outfit")
  abline(v=1, col="gray")
  abline(v=0.5, col="gray", lty=2)
  abline(v=1.5, col="gray", lty=2)
  par(mfrow=c(1,1))
}

plot.test.info <- function(model, theta) {
  info.test <- testinfo(model, Theta = theta)
  df_tmp <- data.frame(theta=theta, info=info.test)[order(theta),]
  plot(df_tmp, type='l', lwd=3,
       xlab=expression(theta),
       ylab=expression(I(theta)),
       main='Testinfo')
}

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

plot.info <- function(itemnum, new=T){
  if (new) {
    plot(info.items[,1], info.items[,as.character(itemnum)],
          type='l',
          ylim=c(0,7),
          xlim=c(info.items$theta[1], tail(info.items$theta, n=1)),
          xlab=expression(theta),
          ylab=expression(I(theta)),
          main=paste0('item #', itemnum),
         )
  } else {
    lines(info.items[,1], info.items[,as.character(itemnum)],
        type='l',
        ylim=c(0,7),
        xlim=c(info.items$theta[1], tail(info.items$theta, n=1)),
        xlab=expression(theta),
        ylab=expression(I(theta)),
        lty=2,
        # main=paste0('item #', itemnum),
        )
  }
}

info.quantiles <- function(info.items, steps=40){
  theta.quantiles <- quantile(info.items$theta, probs = 0:steps/steps, type=4)
  item.selection <- data.frame(quantile=theta.quantiles) %>%
    rownames_to_column(var="percent")
  item.selection$index <- findInterval(theta.quantiles, info.items$theta)
  return(item.selection)
}

select.items <- function(info.items, item.selection, m=6){
  index.set <- c()
  info.items.tmp <- info.items
  for (i in item.selection$index){
    row <- t(info.items.tmp[i,-1]) # information per item at given quantile
    index.row <- order(row, decreasing=T)[1:m] # m best indices
    index.subset <- names(row[index.row,]) # row indices to item numbers
    index.set <- c(index.set, index.subset)
    info.items.tmp <- info.items.tmp %>% select(-all_of(index.subset)) # remove selected items
  }
  # index.set <- as.character(sort(as.numeric(unique(index.set))))
  return(index.set)
}

# =============================================================================
# prepare data

# load data
fits <- readRDS(here::here(glue("analysis/models/{BM}-all.rds")))
data <- fits$data
model <- fits[[Model]]$model
theta <- fits[[Model]]$theta
rm(fits)

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
info.items.summary <- summarize.item.info(info.items)
# plot.info(42)

#===============================================================================
# subtest creation

# 1. remove badly fitting items
# get object type of info.items
info.items <- info.items %>% select(!item.bad$item)

# 2. decide on a range on theta quantiles
item.selection <- info.quantiles(info.items, steps=40)

# 3 select m items with the highest information in each quantile
index.set <- select.items(info.items, item.selection, m=6)

