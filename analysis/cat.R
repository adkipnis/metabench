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
  
  gprint("🚰 Preparing CAT simulation with full set of {nrow(item.fits)} items on {benchmark.name} benchmark ({model.type} model)...")
  
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

benchmarks <- c("hellaswag", "mmlu", "arc", "gsm8k", "truthfulqa", "winogrande")
models <- c("2PL", "3PL", "3PLu", "4PL")

for (BM in benchmarks){
  for (MOD in models){
    
    set.seed(1) #reset one each iter
    
    gprint("🚰 Loading data...")
    
    datapath.model <- gpath("analysis/models/{BM}-{MOD}-cv.rds")
    full <- readRDS(datapath.model)
    model <- full$model
    nextItem
    rm(full)
    
    item.bank <- generate.item.bank(model, BM, MOD)
    
    # Generation of thetas and response matrix for existing models
    gprint("⚙️ Computing thetas for {BM}-{MOD} on full set.")
    
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
gprint("✅ Simulation Finished!")


# =============================================================================
# Summarizing

here::i_am("analysis/cat.R")
gprint("⚙️ Producing summaries...")

benchmarks <- c("hellaswag", "mmlu", "arc", "gsm8k", "truthfulqa", "winogrande")
models <- c("2PL", "3PL", "3PLu", "4PL")

test.lengths <- data.frame(benchmark.model = c(), test.length = c())
for (BM in benchmarks){
  for (MOD in models){
    datapath <- gpath("analysis/cat/catsim-{BM}-{MOD}.rds")
    sim <- readRDS(datapath)
    test.length <- sim$numberItems
    df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ")"), length(test.length)), test.length = test.length)
    test.lengths <- dplyr::bind_rows(test.lengths, df)
  }
}

cat.accuracies <- data.frame(benchmark.model = c(), assigned.theta = c(), cat.estimated.theta = c())
for (BM in benchmarks){
  for (MOD in models){
    datapath <- gpath("analysis/cat/catsim-{BM}-{MOD}.rds")
    sim <- readRDS(datapath)
    assigned.thetas <- sim$final.values.df$true.theta
    estimated.thetas <- sim$final.values.df$estimated.theta
    df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ")"), length(assigned.thetas)), assigned.theta = assigned.thetas, cat.estimated.theta = estimated.thetas)
    cat.accuracies <- dplyr::bind_rows(cat.accuracies, df)
  }
}

cat.accuracies.summary <- cat.accuracies |>
  dplyr::group_by(benchmark.model) |>
  dplyr::summarise(RMSE = ModelMetrics::rmse(actual = assigned.theta, predicted = cat.estimated.theta))

test.lengths.summary <- test.lengths |>
  dplyr::group_by(`benchmark.model`) |>
  dplyr::summarise(mean.unadj = mean(test.length),
                   median.unadj = median(test.length))

test.lengths.prop.summary <- test.lengths |>
  dplyr::group_by(benchmark.model) |>
  dplyr::mutate(proportion.items = test.length/max(test.length)) |>
  dplyr::arrange(proportion.items) |>
  dplyr::transmute(proportion.items = proportion.items,
                   benchmark.model = benchmark.model,
                   proportion.llms = dplyr::row_number(),
                   proportion.llms = proportion.llms/dplyr::n()) |>
  dplyr::group_by(benchmark.model) |>
  dplyr::summarise(mean.adj = mean(proportion.items),
                   median.adj = median(proportion.items))

overall.summary <- cat.accuracies.summary |>
  dplyr::inner_join(test.lengths.summary) |>
  dplyr::inner_join(test.lengths.prop.summary)

outpath <- gpath("analysis/cat/overall-summaries.rds")
saveRDS(overall.summary, outpath)

rm(list = ls())
gprint("✅ Finished!")