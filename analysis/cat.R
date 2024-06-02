# Get test items for each benchmark and adaptively test models to estimate theta:
#   1. Load data and models
#   2. Remove outlier items
#   3. Estimate thetas given trained model for full dataset
#   4. Simulate Computerized Adaptive Tests using real model responses


# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, get.theta])
Saveplots <- T
verbose.errors <- T
here::i_am("analysis/cat.R")
set.seed(1)


#==============================================================================
# helper functions

generate.item.bank <- function(model, benchmark.name, model.type, filter.outliers=FALSE){
  
  datapath.itemfits <- gpath("analysis/itemfits/{benchmark.name}.rds")
  item.fits <- readRDS(datapath.itemfits) |>
    dplyr::filter(itemtype == model.type)
  
  gprint("🚰 Preparing CAT simulation with full set of {nrow(item.fits)} items on {benchmark.name} benchmark...")
  
  item.fits <- item.fits |>
    dplyr::filter(outlier == filter.outliers) 
  
  gprint("🚰 Keeping {nrow(item.fits)} that are not outliers...")
  
  # Generate item bank (matrix of model parameters per item) from reduced set
  item.bank <- mirt::coef(model, IRTpars=T, simplify=T)$items 
  item.bank <- item.bank[item.fits$item,] #filter outliers
  
  return(item.bank)
}

generate.theta.response.matrix <- function(model, benchmark.name, item.bank){
  if (benchmark.name %in% c("hellaswag", "mmlu")){
    datapath <- gpath("data/{benchmark.name}-sub.rds")
  } else {
    datapath <- gpath("data/{benchmark.name}-preproc-split.rds")
  }
  data <- readRDS(datapath)
  full.data <- dplyr::bind_rows(data$data.train, data$data.test)
  
  data.item.bank <- full.data[,rownames(item.bank)]
  
  thetas <- get.theta(model,
                      method="MAP",
                      resp = full.data)
  
  rownames(thetas) <- rownames(full.data)
  
  theta.response.matrix <- merge(thetas[,1], data.item.bank, by=0)[-1] |> #merge by rowname, remove spurious col
    dplyr::rename(theta = x)
  
  return(theta.response.matrix)
}

# =============================================================================
# load data

benchmarks <- c("hellaswag", "mmlu_sub", "arc", "gsm8k", "truthfulqa", "winogrande")
models <- c("2PL", "3PL", "3PLu", "4PL")

for (BM in benchmarks){
  for (MOD in models){
    set.seed(1) #reset one each iter
    
    gprint("🚰 Loading data...")
    
    datapath.model <- gpath("analysis/models/{BM}-{MOD}-cv.rds")
    full <- readRDS(datapath.model)
    model <- full$model
    
    rm(full)
    
    item.bank <- generate.item.bank(model, BM, MOD)
    
    # Generation of thetas and response matrix for existing models
    gprint("⚙️ Computing thetas for {BM} on full set.")
    
    theta.response.matrix <- generate.theta.response.matrix(model, BM, item.bank)
    
    thetas <- theta.response.matrix$theta
    
    gprint("⚙️ Simulating CAT...")
    tryCatch({
      simulation <- catR::simulateRespondents(thetas = thetas, 
                                        itemBank = item.bank[,1:4], 
                                        rmax = 1, 
                                        start = list(theta = round(mean(thetas))-1:round(mean(thetas))+1, 
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



