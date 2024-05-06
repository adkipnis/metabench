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

collect.item.info <- function(model, theta){
   n <- length(theta)
   d <- dim(model)[2]
   info.items <- matrix(NA, nrow=n, ncol=d+1)
   info.items[,1] <- theta
   for (j in 1:d){info.items[,j+1] <- iteminfo(extract.item(model, j), Theta=theta)}
   info.items <- data.frame(info.items[order(theta),])
   colnames(info.items) <- c("theta", colnames(model))
   return(info.items)
}

# =============================================================================
# load data

fits <- readRDS(here::here(glue("analysis/models/{BM}-all.rds")))
fits <- fits_final
saveRDS(fits, file=here::here(glue("analysis/models/{BM}-all.rds")))
model <- fits$model
theta <- fits$theta

# item fits
item.fit <- itemfit(model, fit_stats = 'infit', Theta = theta)
plot.itemfit(item.fit)
item.bad <- item.fit %>%
   filter(infit <= 0.5 | infit >= 1.5 | outfit <= 0.5 | outfit >= 1.5)
print(glue("percentage of bad items: {nrow(item.bad)/nrow(item.fit)}"))

# item Info
info.items <- collect.item.info(model, theta)

info.items.summary <- data.frame(item=colnames(info.items)[-1])
rownames(info.items.summary) <- info.items.summary$item
index <- apply(info.items, 2, which.max)[-1]
info.items.summary$argmax <- info.items$theta[index]
info.items.summary$max <- sapply(info.items, max)[-1]
info.items.summary$sd <- sapply(info.items, sd)[-1]
info.items.summary <- info.items.summary %>% arrange(by=argmax)

# Plot information curves
infoplot <- function(itemnum, new=T){
  if (new) {
    plot(info.items[,1], info.items[,as.character(itemnum)],
          type='l',
          ylim=c(0,7),
          xlim=c(info.items$theta[1], tail(info.items$theta, n=1)),
          xlab=expression(theta),
          ylab=expression(I(theta)),
          # main=paste0('item #', itemnum),
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
info.items.summary["42",]
infoplot(42)



