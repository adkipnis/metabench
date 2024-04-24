library(readr)
library(tidyverse)
library(mirt)

# set benchmark (first arg when calling this file with Rscript)
args <- commandArgs(trailingOnly = TRUE)
BM <- args[1]
if (is.na(BM)) {
  BM <- "gsm8k"
}

# options
here::i_am("analysis/fit.r")
set.seed(1)
TOL <- 1e-4
LOAD <- T

# prepare data
df <- read_csv(here::here(paste0("data/", BM, ".csv")))
data <- df %>% 
   mutate(correct = as.integer(correct)) %>%
   pivot_wider(names_from = item, values_from = correct) %>%
   column_to_rownames(var = "source")
sum(is.na(data))

# remove outliers
data <- data[!(rowSums(data) < 30),] # remove tail outliers
scores <- rowSums(data)
# plot(ecdf(scores))

# drop items without variance
std <- apply(data, 2, sd)
data <- data[, std > 0]
(n <- nrow(data))
(d <- ncol(data))

# prepare mirt
mirtCluster()
mirtCluster(remove=T)
internaldat <- mirt(data, 1, large='return')

#===============================================================================
# 2PL Model
# set modpath using 
modpath <- here::here(paste0("analysis/models/", BM, "-2pl.rds"))
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
modpath <- here::here(paste0("analysis/models/", BM, "-3pl.rds"))
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
modpath <- here::here(paste0("analysis/models/", BM, "-3plu.rds"))
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
modpath <- here::here(paste0("analysis/models/", BM, "-4pl.rds"))
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

