#rm(list = ls())
library(tidyverse)
library(readr)
library(mirt)
mirtCluster()
mirtCluster(remove=T)
here::i_am("analysis/gsm8k-mirt.r")
set.seed(1)
TOL <- 1e-4
LOAD <- T

# prepare data
df <- read_csv(here::here("data/gsm8k_clean.csv"))
data <- df %>% select(!name) %>%
  mutate(output = as.integer(output), item = item+1) %>%
  pivot_wider(names_from = item, values_from = output) %>%
  column_to_rownames(var = "model")
data <- data[!(rowSums(data) < 50),] # remove tail outliers
scores <- rowSums(data)
# plot(ecdf(scores))

# LLMs

# drop items without variance
std <- apply(data, 2, sd)
data <- data[, std > 0]
n <- nrow(data)
d <- ncol(data)

# prepare mirt
internaldat <- mirt(data, 1, large='return')

#===============================================================================
# 2PL Model
modpath <- here::here("analysis/models/gsm8k-2pl.rds") 
if (!LOAD) {
  mod.2pl <- mirt(data, 1, itemtype='2PL',
                  method='EM',
                  density='Davidian-4',
                  large=intenaldat,
                  TOL=TOL,
                  technical=list(NCYC=2000)) 
  saveRDS(mod.2pl, file=modpath) 
} else {
  mod.2pl <- readRDS(modpath)
}
mod.2pl

#===============================================================================
# Comparison with 3PL
modpath <- here::here("analysis/models/gsm8k-3pl.rds")
if (!LOAD) {
  mod.3pl <- mirt(data, 1, itemtype='3PL',
                  method='EM',
                  density='Davidian-4',
                  large=internaldat,
                  TOL=TOL,
                  technical=list(NCYC=2000)) 
  saveRDS(mod.3pl, file=modpath) 
} else {
  mod.3pl <- readRDS(modpath)
}
mod.3pl
anova(mod.2pl, mod.3pl)

#===============================================================================
# Comparison with 3PLu
modpath <- here::here("analysis/models/gsm8k-3plu.rds")
if (!LOAD) {
  mod.3plu <- mirt(data, 1, itemtype='3PLu',
                   method='EM',
                   density='Davidian-4',
                   large=internaldat,
                   TOL=TOL,
                   technical=list(NCYC=2000)) 
  saveRDS(mod.3plu, file=modpath) 
} else {
  mod.3plu <- readRDS(modpath)
}
mod.3plu
anova(mod.3pl, mod.3plu)


#===============================================================================
# Comparison with 4PL
modpath <- here::here("analysis/models/gsm8k-4pl.rds")
if (!LOAD) {
  mod.4pl <- mirt(data, 1, itemtype='4PL',
                  method='EM',
                  density='Davidian-4',
                  large=internaldat,
                  TOL=TOL,
                  technical=list(NCYC=3000))
  saveRDS(mod.4pl, file=modpath)
} else {
  mod.4pl <- readRDS(modpath)
}
mod.4pl
anova(mod.3plu, mod.4pl)

mod.final <- mod.2pl

#===============================================================================
# parameter plots
theta <- fscores(mod.final, method='EAPsum', use_dentype_estimate=T)
colnames(theta) = 'theta'
estimates <- data.frame(coef(mod.final, simplify=T, rotate="none")$items) %>%
  rownames_to_column(var='item') %>% mutate(item = as.numeric(item))

par(mfrow=c(3,1))
plot(density(theta), main='Theta')
abline(v=0, lty=2)
plot(density(-estimates$d), main='Difficulty')
abline(v=0, lty=2)
plot(density(estimates$a1), main='Loading')
abline(v=0, lty=2)

par(mfrow=c(1,1))
plot(-estimates$d, estimates$a1, xlab='difficulty', ylab='loading')
cor.test(-estimates$d, estimates$a1)

#===============================================================================
# item fits
item.fit <- itemfit(mod.final, fit_stats = 'infit', Theta = theta)

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
item.bad <- item.fit %>% filter(infit <= 0.5 | infit >= 1.5 |
                         outfit <= 0.5 | outfit >= 1.5)
nrow(item.bad)/nrow(item.fit)

#===============================================================================
# # person fits (assumes normality)
# fit.person <- personfit(mod.final, Theta = theta)
# q <- 2.576 # 99% quantile
# 
# plot(density(fit.person$z.infit), xlab="", main="Person z-Infit")
# abline(v=0, col="gray")
# abline(v=-q, col="gray", lty=2)
# abline(v=q, col="gray", lty=2)
# 
# plot(density(fit.person$z.outfit), xlab="", main="Person z-Outfit")
# abline(v=0, col="gray")
# abline(v=-q, col="gray", lty=2)
# abline(v=q, col="gray", lty=2)
# 
# badllms <- fit.person %>% filter(z.infit <= -q | z.infit >= q |
#                                  z.outfit <= -q | z.outfit >= q)
# nrow(badllms)/nrow(fit.person)

#===============================================================================
# Relation between Theta and Testscore
# plot(mod.final)
cor(theta, scores, method='spearman')
df.score <- data.frame(theta=theta, score=scores) %>%
  arrange(by=theta) %>%
  mutate(rank.score = rank(score), perc.score = rank.score/max(rank.score),
         rank.theta = rank(theta), perc.theta = rank.theta/max(rank.theta))
plot(perc.score ~ perc.theta, data=df.score, type='l')

# GAM of test score
mod.score = mgcv::gam(score ~ s(theta), data = df.score)
mod.score

df.score$p <- predict(mod.score)
plot(df.score$theta, df.score$score, type='b', pch=16, 
     xlab=expression(theta), ylab='Test Score')
lines(df.score$theta, df.score$p, col="red")
df.score %>% summarize(mae = mean(abs(score-p)))

# Test Info (sum over item infos)
info.test <- testinfo(mod.final, Theta = theta)
df_tmp <- data.frame(theta=theta, info=info.test)[order(theta),]
plot(df_tmp, type='l', lwd=3,
     xlab=expression(theta),
     ylab=expression(I(theta)),
     main='Testinfo')

# Item Info
info.items <- matrix(NA, nrow=n, ncol=d+1)
info.items[,1] <- theta
for (j in 1:d){info.items[,j+1] <- iteminfo(extract.item(mod.final, j), Theta=theta)}
info.items <- data.frame(info.items[order(theta),])
colnames(info.items) <- c("theta", colnames(data))

# Summary
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



#===============================================================================
# Subtest creation

# 1. remove badly fitting items
info.items <- data.frame(info.items)
colnames(info.items) <- c('theta', colnames(data))
info.items <- info.items %>% select(!item.bad$item)

# 2. decide on a range on theta quantiles
steps <- 40
theta.quantiles <- quantile(info.items$theta, probs = 0:steps/steps, type=4)
item.selection <- data.frame(quantile=theta.quantiles) %>%
  rownames_to_column(var="percent")
item.selection$index <- findInterval(theta.quantiles, info.items$theta)
item.selection

# # 3.1 select items with highest information
# infosums <- colSums(info.items)
# infosums.best <- sort(infosums, decreasing=T)[1:50]
# index.set <- names(infosums.best)
# info.items.tmp <- info.items %>% select(-all_of(index.set))

# 3.2 select m items with the highest information in each quantile
index.set <- c()
m <- 6
info.items.tmp <- info.items
for (i in item.selection$index){
  row <- t(info.items.tmp[i,-1]) # information per item at given quantile
  index.row <- order(row, decreasing=T)[1:m] # m best indices
  # plot(row[index.row])
  index.subset <- names(row[index.row,]) # row indices to item numbers
  index.set <- c(index.set, index.subset)
  info.items.tmp <- info.items.tmp %>% select(-all_of(index.subset))
}
index.set <- as.character(sort(as.numeric(unique(index.set))))
length(index.set)

# expected info plot
for (i in index.set){
  item <- info.items.summary[i,]$item
  infoplot(item, new=i==index.set[1])
}

# expected test info plot
info.items.sub <- info.items[,as.character(index.set)]
info.items.sub$cum <- rowSums(info.items.sub)
plot(info.items$theta, info.items.sub$cum, type='l',
     main='expected subtest info',
     xlab=expression(theta), ylab=expression(I(theta)))


# 4. refit irt model
data.sub <- data %>% select(all_of(index.set))
mod.sub <- mirt(data.sub, 1, itemtype='2PL',
                method='EM', dentype='Davidian-4', TOL=1e-4,
                technical = list(NCYCLES=2000))

# 5. check parameter recovery
estimates.sub <- data.frame(coef(mod.sub, simplify=T, rotate="none")$items) %>%
  rownames_to_column(var='item') %>% mutate(item = as.numeric(item))
df.comparison <- merge(estimates, estimates.sub, by='item')
plot(d.y ~ d.x, data=df.comparison,
     main='difficulty', xlab='original', ylab='recovered')
cor.test(df.comparison$d.x, df.comparison$d.y)
plot(a1.y ~ a1.x, data=df.comparison,
     main='loading', xlab='original', ylab='recovered')
cor.test(df.comparison$a1.x, df.comparison$a1.y)

# 6. check latent recovery
theta.sub <- fscores(mod.sub, method="EAPsum", use_dentype_estimate = T)
colnames(theta.sub) <- "theta"
cor.test(theta.sub, theta)
plot(theta, theta.sub)

# # Optionally: estimate theta under the original parameters and the reduced data
# mod.meta <- mod.sub
# j <- 1
# for (i in as.numeric(index.set)){
#   mod.meta@ParObjects$pars[[j]]@par <- mod.final@ParObjects$pars[[i]]@par
#   j <- j+1
# }
# theta.meta <- fscores(mod.meta, method="EAPsum", use_dentype_estimate = T)
# plot(theta.meta, theta)
# cor.test(theta.meta, theta)

# 7. test info
info.test.sub <- testinfo(mod.sub, Theta = theta.sub)
df_tmp <- data.frame(theta=theta.sub, info=info.test.sub)[order(theta.sub),]
plot(df_tmp, type='l', lwd=3,
     xlab=expression(theta),
     ylab=expression(I(theta)),
     main='Testinfo')

# 8. check score estimation
cor(theta.sub, scores, method='spearman')
df.score.sub <- data.frame(theta=theta.sub, score=scores) %>%
  arrange(by=theta) %>%
  mutate(rank.score = rank(score), perc.score = rank.score/max(rank.score),
         rank.theta = rank(theta), perc.theta = rank.theta/max(rank.theta))
plot(perc.score ~ perc.theta, data=df.score.sub, main="Percentile Matching",
     xlab="theta.sub", ylab="Full Score")
hist(abs(df.score.sub$perc.score - df.score.sub$perc.theta), breaks=20,
     main='absolute percentile error')


df.score.sub$p <- predict(mod.score, df.score.sub)
plot(score ~ theta, data=df.score.sub, xlab="theta.sub", ylab="Test Score")
lines(p ~ theta, data=df.score.sub, col="red")
df.score.sub %>% summarize(mae = mean(abs(score-p)))

# with adapted GAM
mod.score.sub = mgcv::gam(score ~ s(theta), data = df.score.sub)
df.score.sub$p.sub <- predict(mod.score.sub)
lines(p.sub ~ theta, data=df.score.sub, col="yellow")
df.score.sub %>% summarize(mae = mean(abs(score-p.sub)))
hist(abs(df.score.sub$score-df.score.sub$p.sub), breaks=20,
     main='absolute score prediction error')
