# Get test items for each benchmark and adaptively test models to estimate theta:
#   0. Load helper functions
#   1. Load data and models
#   3. Estimate thetas given trained model for full dataset
#   4. Simulate Computerized Adaptive Tests using real model responses

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, get.theta])
box::use(doParallel[...], foreach[...])
here::i_am("analysis/cat.R")
set.seed(1)


#==============================================================================
# helper functions

generate.item.bank <- function(model,
                               benchmark.name,
                               model.type,
                               filter.outliers = FALSE) {
  datapath.itemfits <- gpath("analysis/itemfits/{benchmark.name}.rds")
  item.fits <- readRDS(datapath.itemfits) |>
    dplyr::filter(itemtype == model.type)
  
  gprint(
    "🚰 Preparing CAT simulation item bank with full set of {nrow(item.fits)} items on {benchmark.name} benchmark ({model.type} model)..."
  )
  
  if (filter.outliers == TRUE) {
    item.fits <- item.fits |>
      dplyr::filter(outlier == FALSE)
    
    gprint("🚰 Keeping {nrow(item.fits)} that are not outliers...")
  }
  
  # Generate item bank (matrix of model parameters per item) from reduced set
  item.bank <- mirt::coef(model, IRTpars = T, simplify = T)$items
  item.bank <- item.bank[item.fits$item, ] #filter outliers
  
  return(item.bank)
}

generate.theta.response.matrix <- function(model, benchmark.name,
                                           model.type, item.bank,
                                           method = "EAP") {
  if ((benchmark.name == "mmlu" | benchmark.name == "hellaswag") && method == "EAP") {
    datapath <- gpath("data/{benchmark.name}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{benchmark.name}-preproc-split.rds")
  }
  
  data <- readRDS(datapath)
  full.data <- dplyr::bind_rows(data$data.train, data$data.test)
  
  data.item.bank <- full.data[, rownames(item.bank)]
  
  if (method == "EAP") {
    thetas <- get.theta(model, method = "EAPsum", resp = full.data)
    
    rownames(thetas) <- rownames(full.data)
    theta.response.matrix <- merge(thetas[, 1], data.item.bank, by = 0)[-1] |> #merge by rowname, remove spurious col
      dplyr::rename(F1 = x)
    
  } else if (method == "MAP") {
    datapath.model <- gpath("analysis/models/{benchmark.name}-{model.type}-1-cv-big.rds")
    thetas <- readRDS(datapath.model)$df |>
      dplyr::select("F1")
    
    theta.response.matrix <- merge(thetas, data.item.bank, by = 0)[-1]
  } else {
    stop("Method not recognised. MAP or EAP supported.")
  }
  
  return(theta.response.matrix)
}

sim.wrapper <- function(list.element){
  theta.response.matrix <- list.element$theta.response.matrix
  item.bank <- list.element$item.bank
  BM <- list.element$benchmark.name
  MOD <- list.element$model.type
  METH <- list.element$method
  METH <- switch(METH,
                   "EAP" = "EAP",
                   "MAP" = "BM")
  simulation <- catR::simulateRespondents(thetas = theta.response.matrix$F1,
    itemBank = item.bank[, 1:4],
    rmax = 1,
    start = list(
      theta = c(-2:2),
      randomesque = 1,
      startSelect = "MFI"
    ),
    test = list(
      method = METH,
      #MAP is appropriate for hellaswag, otherwise, EAP.
      priorDist = "norm",
      priorPar = c(0, 1),
      itemSelect = "MFI",
      range = c(-7, 5),
      parInt = c(-7, 5, 52)
    ),
    stop = list(rule = "precision", thr = 0.1),
    final = list(
      method = METH,
      priorDist = "norm",
      priorPar = c(0, 1),
      range = c(-7, 5),
      parInt = c(-7, 5, 52)
    ),
    genSeed = sample.int(nrow(theta.response.matrix)),
    responsesMatrix = theta.response.matrix[-1],
  )
  output <- list(sim.results = simulation, benchmark.name = BM, model.type = MOD, method = METH)
  return(output)
}

get.bm.list <- function(bms){
  models <- c("2PL", "3PL", "4PL")
  methods <- c("EAP", "MAP")
  data <- list()
  for (BM in bms){
      for (MOD in models){
          for (METH in methods){
            tryCatch({
                    set.seed(1) #reset one each iter
                    
                    gprint("🚰 Loading data...")
                    
                    datapath.model <- gpath("analysis/models/{BM}-{MOD}-1-cv-big.rds")
                    full <- readRDS(datapath.model)
                    model <- full$model
                    rm(full)
                    
                    item.bank <- generate.item.bank(model, BM, MOD)
                    
                    # Generation of thetas and response matrix for existing models
                    gprint("⚙️ Computing thetas for {BM}-{MOD} on full set.")
                    
                    theta.response.matrix <- generate.theta.response.matrix(model, BM, MOD, item.bank, method = METH)
                    
                    bm.mod.list <- list(model = model,
                                        item.bank = item.bank,
                                        theta.response.matrix = theta.response.matrix,
                                        benchmark.name = BM,
                                        model.type = MOD,
                                        method = METH)
                    
                    data[[length(data)+1]] <- bm.mod.list
            }, error = function(e){
              print("Error loading data, skipping..")
            })
          }
      }
  }
  return(data)
}

run.simulation <- function(data.list){
  n.cores <- length(data.list) + 2

  gprint("⚙️ Simulating CAT on {n.cores} cores...")

  mu.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile="")
  doParallel::registerDoParallel(mu.cluster)

  sim.outputs <- foreach(i = 1:length(data.list), .errorhandling = "remove") %dopar% {
    sim.wrapper(data.list[[i]])
  }
  
  parallel::stopCluster(mu.cluster)

  rm(data.list)

  gprint("✅ Simulation success!")

  return(sim.outputs)
}

# =============================================================================
# Run CAT simulations
# Run in two batches for memory

warnStatus <- getOption("warn")
options(warn=-1)

bm.list <- c("hellaswag", "arc", "gsm8k")
bm.names <- paste0(bm.list, collapse = "-")
data <- get.bm.list(bm.list)

options(warn=warnStatus)

sim.outputs <- run.simulation(data)

outpath <- gpath("analysis/cat/catsim-results-{bm.names}.rds")
saveRDS(sim.outputs, outpath)

rm(bm.list)
rm(bm.names)
rm(data)
rm(sim.outputs)

warnStatus <- getOption("warn")
options(warn=-1)

bm.list <- c("mmlu", "truthfulqa", "winogrande")
bm.names <- paste0(bm.list, collapse = "-")
data <- get.bm.list(bm.list)

options(warn=warnStatus)

sim.outputs <- run.simulation(data)

outpath <- gpath("analysis/cat/catsim-results-{bm.names}.rds")
saveRDS(sim.outputs, outpath)

rm(bm.list)
rm(bm.names)
rm(data)
rm(sim.outputs)

gprint("🏁 Finished!")