# =============================================================================
# load packages
packages <- c("tidyr", "dplyr", "tibble", "readr", "mirt", "here", "glue")
install.packages(setdiff(packages, rownames(installed.packages())))  
invisible(suppressMessages(sapply(packages, require, character.only=T)))

# =============================================================================
# parse args
args <- commandArgs(trailingOnly = T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "gsm8k"
}

# =============================================================================
# path and seed
here::i_am("analysis/fit.R")
if (!dir.exists(here::here("analysis/models"))) {
  dir.create(here::here("analysis/models"))
}
set.seed(1)

# =============================================================================
# helper functions

run.mirt <- function(itemtype){
  out <- mirt(data, 1, itemtype=itemtype,
              method='EM',
              density='Davidian-4',
              large=internaldat,
              TOL=1e-4,
              technical=list(NCYCLES=2000))
  return(out)
}

wrapper <- function(itemtype, save=F){
   model <- run.mirt(itemtype)
   theta <- fscores(model, method='MAP')
   out <- list(model=model, theta=theta)
   if (save) {
      modpath <- here::here(glue("analysis/models/{BM}-{itemtype}.rds"))
      saveRDS(out, file=modpath)
   }
   return(out)
}

# =============================================================================
# prepare data
df <- read_csv(here::here(glue("data/{BM}.csv")), show_col_types = F)
data <- df %>% 
   mutate(correct = as.integer(correct)) %>%
   pivot_wider(names_from = item, values_from = correct) %>%
   column_to_rownames(var = "source")
n_missing <- sum(is.na(data))
rm(df)


# remove outliers and items without variance
scores <- rowSums(data)
# hist(scores, breaks=100)
# threshold <- as.numeric(quantile(scores, probs=c(0.001)))
threshold <- 0
n <- nrow(data)
data <- data[!(scores <= threshold),] # remove tail outliers
std <- apply(data, 2, sd)
m <- ncol(data)
data <- data[, std > 0]

# print summary
summary.str <- glue(
  "Prepared preprocessing for {BM}:\n",
  "{n_missing} missing values (check data if > 0)\n",
  "Removed {n - nrow(data)} tail outliers (lowest 0.1% of score, threshold: {threshold})\n",
  "Removed {m - ncol(data)} items without variance\n",
  "Nubmer of subjects: {nrow(data)}\n",
  "Number of items: {ncol(data)}\n"
  )
print(summary.str)

# prepare mirt
internaldat <- mirt(data, 1, large='return')


#===============================================================================
# fit models
# mirtCluster()
# mirtCluster(remove=T)
fit.2pl <- wrapper("2PL")
fit.3pl <- wrapper("3PL")
fit.3plu <- wrapper("3PLu")
fit.4pl <- wrapper("4PL")
fits <- list(data=data, `2PL`=fit.2pl, `3PL`=fit.3pl, `3PLu`=fit.3plu, `4PL`=fit.4pl)
saveRDS(fits, file=here::here(glue("analysis/models/{BM}-all.rds")))

