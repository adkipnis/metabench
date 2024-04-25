packages <- c("tidyr", "dplyr", "tibble", "readr", "mirt", "here")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, require, character.only=T)

# set benchmark (first arg when calling this file with Rscript)
args <- commandArgs(trailingOnly = TRUE)
BM <- args[1]
if (is.na(BM)) {
  BM <- "gsm8k"
}

# options
here::i_am("analysis/fit.R")
set.seed(1)
TOL <- 1e-4
LOAD <- F

# prepare data
df <- read_csv(paste0("~/Documents/data/", BM, ".csv"))
#df <- read_csv(here::here(paste0("data/", BM, ".csv")))
data <- df %>% 
   mutate(correct = as.integer(correct)) %>%
   pivot_wider(names_from = item, values_from = correct) %>%
   column_to_rownames(var = "source")
(sum(is.na(data)))

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
mirtrun <- function(itemtype){
  out <- mirt(data, 1, itemtype=itemtype,
              method='EM',
              density='Davidian-4',
              large=internaldat,
              TOL=TOL,
              technical=list(NCYCLES=2000))
  return(out)
}

#===============================================================================
# 2PL Model
# set modpath using 
modpath <- here::here(paste0("analysis/models/", BM, "-2pl.rds"))
if (!LOAD) {
  mod.2pl <- mirtrun('2PL')
  saveRDS(mod.2pl, file=modpath) 
} else {
  mod.2pl <- readRDS(modpath)
}
mod.2pl

#===============================================================================
# Comparison with 3PL
modpath <- here::here(paste0("analysis/models/", BM, "-3pl.rds"))
if (!LOAD) {
  mod.3pl <- mirtrun('3PL')
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
  mod.3plu <- mirtrun('3PLu')
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
  mod.4pl <- mirtrun('4PL')
  saveRDS(mod.4pl, file=modpath)
} else {
  mod.4pl <- readRDS(modpath)
}
mod.4pl
anova(mod.3plu, mod.4pl)

