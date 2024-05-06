packages <- c("tidyr", "dplyr", "tibble", "readr", "mirt", "here", "glue")
install.packages(setdiff(packages, rownames(installed.packages())))  
invisible(suppressMessages(sapply(packages, require, character.only=T)))
args <- commandArgs(trailingOnly=T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "gsm8k"
}
print(glue("Fitting {BM}..."))

# options
set.seed(1)
here::i_am("analysis/fit.R")

# prepare data
df <- read_csv(here::here(glue("data/{BM}.csv")))
data <- df %>% 
   mutate(correct = as.integer(correct)) %>%
   pivot_wider(names_from = item, values_from = correct) %>%
   column_to_rownames(var = "source")
print(glue("Number of missing values: {sum(is.na(data))}"))
rm(df)

# remove outliers and items without variance
n <- nrow(data)
data <- data[!(rowSums(data) < 30),] # remove tail outliers
print(glue("Removed {n - nrow(data)} outlier subjects"))
std <- apply(data, 2, sd)
m <- ncol(data)
data <- data[, std > 0]
print(glue("Removed {m - ncol(data)} items without variance"))
print(glue("Nubmer of subjects: {nrow(data)}"))
print(glue("Number of items: {ncol(data)}"))

# prepare mirt
internaldat <- mirt(data, 1, large='return')
run.mirt <- function(itemtype){
  out <- mirt(data, 1, itemtype=itemtype,
              method='EM',
              density='Davidian-4',
              large=internaldat,
              TOL=1e-4,
              technical=list(NCYCLES=2000))
  return(out)
}

wrapper <- function(itemtype){
   modpath <- here::here(glue("analysis/models/{BM}-{itemtype}.rds"))
   model <- run.mirt(itemtype)
   saveRDS(model, file=modpath)
   theta <- fscores(mod, method='MAP')
   return(list(model, theta))
}

#===============================================================================
mirtCluster()
mirtCluster(remove=T)
fit.2pl <- wrapper("2PL")
fit.3pl <- wrapper("3PL")
fit.3plu <- wrapper("3PLu")
fit.4pl <- wrapper("4PL")
fits <- list(fit.2pl, fit.3pl, fit.3plu, fit.4pl)
saveRDS(fits, file=here::here(glue("analysis/models/{BM}-all.rds")))

