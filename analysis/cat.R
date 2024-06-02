# Get reduced test set and adaptively test models to estimate theta:
#   1. load reduced test set


# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, get.theta])
box::use(mirt[...], catR[...], dplyr[...])
Saveplots <- T
verbose.errors <- T
here::i_am("analysis/cat.R")
set.seed(1)


#==============================================================================
# helper functions

collect.theta.all <- function(benchmark.name, model.type, lambda.value, theta.type){
  fit.full.path <- gpath("analysis/models/{benchmark.name}-all.rds")
  full.results <- readRDS(fit.full.path)
  full.model <- full.results[[model.type]]$model
  full.theta <- full.results[[model.type]]$theta
  if (theta.type != "MAP"){
    full.theta <- get.theta(full.model, theta.type)
  }
  datapath <- gpath("data/{benchmark.name}_preproc.rds")
  names <- rownames(readRDS(datapath)$data)
  full.theta <- as.data.frame(full.theta)
  rownames(full.theta) <- names
  colnames(full.theta) <- "thetas"
  
  return(full.theta)
}

collect.item.responses <- function(benchmark.name, item.bank){
  datapath <- gpath("data/{benchmark.name}-preproc.rds")
  data <- readRDS(datapath)$data[rownames(item.bank)]
  data
}

# =============================================================================
# load data

benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu_sub", "truthfulqa", "winogrande")
models <- c("2PL", "3PL", "3PLu", "4PL")

for (BM in benchmarks){
  for (MOD in models){
    set.seed(1) #reset one each iter.
  
    datapath.model <- gpath("analysis/models/{BM}-{MOD}-cv.rds")
    full <- readRDS(datapath.model)
                        
    datapath.itemfits <- gpath("analysis/itemfits/{BM}.rds")
    itemfits <- readRDS(datapath.itemfits)
    model <- full$model
    
    gprint("🚰 Preparing CAT simulation with full set of {nrow(itemfits)} items on responses of {nrow(full$df)} models on {BM} benchmark...")
    
    itemfits <- itemfits |>
      filter(itemtype == MOD,
             outlier == FALSE) 
    
    gprint("🚰 Keeping {nrow(itemfits)} that are not outliers...")
    
    model <- full$model
    
    # Generate item bank (matrix of model parameters per item) from reduced set
    itemBank <- coef(model, IRTpars=T, simplify=T)$items 
    itemBank <- itemBank[itemfits$item,] #filter outliers
    
    rm(model)
    
    # Generation of thetas and response matrix for existing models

    thetas <- full$df %>%
      dplyr::select(theta)
    responses <- collect.item.responses(BM, itemBank)
    theta.response.matrix <- merge(thetas, responses, by=0)[-1] #merge by rowname, remove spurious col
    
    rm(full)
    
    gprint("⚙️ Simulating...")
    tryCatch({
      simulation <- simulateRespondents(thetas = theta.response.matrix$theta, 
                                        itemBank = itemBank[,1:4], 
                                        rmax = 1, 
                                        #Mrmax = "IE", #or "restricted"
                                        start = list(theta = round(mean(theta.response.matrix$theta))-1:round(mean(theta.response.matrix$theta))+1, 
                                                     randomesque = 1,
                                                     startSelect = "MFI"), 
                                        test = list(method = "BM", 
                                                    priorDist = "norm",
                                                    itemSelect = "MFI"), 
                                        stop = list(rule = "precision", 
                                                    thr = 0.1), 
                                        final = list(method = "BM", 
                                                     priorDist = "norm"), 
                                        genSeed = sample.int(nrow(theta.response.matrix)), #generate seed list of random +ve ints
                                        responsesMatrix = theta.response.matrix[-1]
                                        )

      p <- plot(simulation,
                save.plot = Saveplots,
                save.options = c(gpath("analysis/cat/"),
                                 paste0(c("/catsim", BM, MOD), collapse = "-"),
                                 "pdf"
                                 )
                )

      outpath <- gpath("analysis/cat/catsim-{BM}-{MOD}.rds")
      saveRDS(simulation, outpath)
      gprint("✅ Simulation success!")
      rm(simulation)
    }, error = function(err){
      gprint("❌ Error in simulation!")
      if (verbose.errors) {
        cat(paste("Error readout:\n", err))
      }
    })
     
  }
}

rm(list = ls())
gprint("✅ Finished!")



