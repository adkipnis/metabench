# full IRT fitting (3pl, 3plu, 4pl) of preprocessed data
# usage: Rscript fit.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, mkdir, run.mirt, get.theta])
parse.args(names = c("BM"),
           defaults = c("arc"),
           legal = list(
             BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"))
           )
here::i_am("analysis/fit.R")
mkdir("analysis/models")
set.seed(1)

# =============================================================================
# helper functions
wrapper <- function(itemtype, large=F, save=F){
   gprint("⚙️  Fitting {itemtype} model...")
   model <- run.mirt(data, itemtype, large=large)
   gprint("🤖 Estimating person parameters...")
   theta <- get.theta(model, method="MAP")
   out <- list(model=model, theta=theta)
   if (save) saveRDS(out, gpath("analysis/models/{BM}-{itemtype}.rds"))
   return(out)
}

# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
if (BM %in% c("hellaswag", "mmlu")){
   datapath <- gpath("data/{BM}-sub.rds")
} else {
   datapath <- gpath("data/{BM}-preproc.rds")
}
data <- readRDS(datapath)$data
internaldat <- mirt::mirt(data, 1, large='return')

#===============================================================================
# fit models
fit.3pl <- wrapper("3PL", internaldat)
fit.3plu <- wrapper("3PLu", internaldat)
fit.4pl <- wrapper("4PL", internaldat)

fits <- list(`3PL`=fit.3pl, `3PLu`=fit.3plu, `4PL`=fit.4pl)
outpath <- gpath("analysis/models/{BM}-all.rds")
saveRDS(fits, outpath)
gprint("💾 Saved to '{outpath}'.")

