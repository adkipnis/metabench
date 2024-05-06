packages <- c("tidyr", "dplyr", "tibble", "readr", "mirt", "here", "glue")
install.packages(setdiff(packages, rownames(installed.packages())))  
invisible(suppressMessages(sapply(packages, require, character.only=T)))
args <- commandArgs(trailingOnly=T)
BM <- args[1]
if (is.na(BM)) {
  BM <- "arc"
}
print(glue("Fitting {BM}..."))

# options
set.seed(1)
here::i_am("analysis/fit.R")

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
threshold <- as.numeric(quantile(scores, probs=c(0.01)))
n <- nrow(data)
data <- data[!(scores < threshold),] # remove tail outliers
std <- apply(data, 2, sd)
m <- ncol(data)
data <- data[, std > 0]

# print summary
summary.str <- glue("Prepared preprocessing for {BM}:\n",
                    "Removed {n - nrow(data)} tail outliers (lowest 1%)\n",
                    "Removed {m - ncol(data)} items without variance\n",
                    "Nubmer of subjects: {nrow(data)}\n",
                    "Number of items: {ncol(data)}\n",
                    "Number of missing values: {n_missing}")
print(summary.str)

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

wrapper <- function(itemtype, save=T){
   model <- run.mirt(itemtype)
   theta <- fscores(model, method='MAP')
   out <- list(model, theta)
   if (save) {
      modpath <- here::here(glue("analysis/models/{BM}-{itemtype}.rds"))
      saveRDS(out, file=modpath)
   }
   return(out)
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

