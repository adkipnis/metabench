# Get reduced test set and adaptively test models to estimate theta:
#   1. load reduced test set


# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, get.theta])
box::use(mirt[...], catR[...])
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
  fit.part.path <- gpath("analysis/reduced/{benchmark.name}-{model.type}-{lambda.value}.rds")
  part.results <- readRDS(fit.part.path)
  part.model <- part.results$model
  part.theta <- part.results$theta
  if (theta.type != "MAP"){
    full.theta <- get.theta(full.model, theta.type)
    part.theta <- get.theta(part.model, theta.type)
  }
  datapath <- gpath("data/{benchmark.name}_preproc.rds")
  names <- rownames(readRDS(datapath)$data)
  full.theta <- as.data.frame(full.theta)
  part.theta <- as.data.frame(part.theta)
  rownames(full.theta) <- names
  rownames(part.theta) <- names
  colnames(full.theta) <- "thetas"
  colnames(part.theta) <- "thetas"
  
  out <- list(full.theta = full.theta, part.theta=part.theta)
  out
}

collect.item.responses <- function(benchmark.name, item.bank){
  datapath <- gpath("data/{benchmark.name}_preproc.rds")
  data <- readRDS(datapath)$data[rownames(item.bank)]
  data
}

# =============================================================================
# load data

# best-fitting benchmark IRT models
benchmarks <- list(arc=list(name="arc", mod="4PL", est="EAPsum"),
                   gsm8k=list(name="gsm8k", mod="3PLu", est="EAPsum"),
                   hellaswag=list(name="hellaswag",mod="3PL", est="MAP"),
                   mmlu_sub=list(name="mmlu_sub",mod="3PLu", est="EAPsum"),
                   truthfulqa=list(name="truthfulqa",mod="3PL", est="EAPsum"),
                   winogrande=list(name="winogrande",mod="3PL", est="EAPsum"))
lams <- c(0,0.1)

for (BM in benchmarks) {
  for (LAMBDA in lams) {
    set.seed(1) #reset one each iter.
    
    gprint("🚰 Loading {BM$name} data...")
    datapath <- gpath("analysis/reduced/{BM$name}-{BM$mod}-{LAMBDA}.rds")
    reduced <- readRDS(datapath)
    
    info.items <- reduced$info.items
    items <- reduced$items 
    model <- reduced$model
    
    rm(reduced)
    
    gprint("🚰 Preparing CAT simulation with reduced set of {nrow(items)} items on responses of {nrow(info.items)} models on {BM$name} benchmark...")
    
    # Generate item bank (matrix of model parameters per item) from reduced set
    itemBank <- coef(model, IRTpars=T, simplify=T)$items
    
    # Generation of thetas and response matrix for existing models
    theta.list <- collect.theta.all(BM$name, BM$mod, LAMBDA, BM$est)
    thetas <- theta.list$full.theta
    responses <- collect.item.responses(BM$name, itemBank)
    theta.response.matrix <- merge(thetas, responses, by=0)[-1] #merge by rowname, remove spurious col
    
    gprint("🚰 Simulating...")
    tryCatch({
      simulation <- simulateRespondents(thetas = theta.response.matrix$thetas, 
                                        itemBank = itemBank[,1:4], 
                                        rmax = 5, 
                                        #Mrmax = "IE", #or "restricted"
                                        start = list(theta = -5:3, 
                                                     randomesque = 1,
                                                     startSelect = "MFI"), 
                                        test = list(method = "ML", 
                                                    itemSelect = "MFI"), 
                                        stop = list(rule = "precision", 
                                                    thr = 0.1), 
                                        final = list(method = "ML"), 
                                        genSeed = sample.int(nrow(theta.response.matrix)), #generate seed list of random +ve ints
                                        responsesMatrix = theta.response.matrix[-1]
                                        )
      
      p <- plot(simulation,
                save.plot = Saveplots,
                save.options = c(gpath("analysis/cat/"),
                                 paste0(c("/catsim", BM$name, BM$mod, LAMBDA, BM$est), collapse = "-"),
                                 "pdf"
                                 )
                )
      
      outpath <- gpath("analysis/cat/catsim-{BM$name}-{BM$mod}-{LAMBDA}-{BM$est}.rds")
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



