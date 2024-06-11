# Get test items for each benchmark and adaptively test models to estimate theta:
#   0. Load helper functions
#   1. Load data and models
#   2. Remove outlier items
#   3. Estimate thetas given trained model for full dataset
#   4. Simulate Computerized Adaptive Tests using real model responses
#   5. Estimate score on full benchmark using CAT estimated thetas
#   6. Summarize the CAT results


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
  
  if (filter.outliers == TRUE){
    item.fits <- item.fits |>
      dplyr::filter(outlier == filter.outliers) 
    
    gprint("🚰 Keeping {nrow(item.fits)} that are not outliers...")
  }
  
  # Generate item bank (matrix of model parameters per item) from reduced set
  item.bank <- mirt::coef(model, IRTpars=T, simplify=T)$items 
  item.bank <- item.bank[item.fits$item,] #filter outliers
  
  return(item.bank)
}

generate.theta.response.matrix <- function(model, benchmark.name, model.type, item.bank, method = "EAPsum"){
  
  if (benchmark.name == "mmlu" && method == "EAPsum") {
    datapath <- gpath("data/{benchmark.name}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{benchmark.name}-preproc-split.rds")
  }

  data <- readRDS(datapath)
  full.data <- dplyr::bind_rows(data$data.train, data$data.test)
  
  data.item.bank <- full.data[,rownames(item.bank)]
  
  if (method == "EAPsum") {
    thetas <- get.theta(model,
                        method="EAPsum",
                        resp = full.data) 
    
    rownames(thetas) <- rownames(full.data)
    theta.response.matrix <- merge(thetas[,1], data.item.bank, by=0)[-1] |> #merge by rowname, remove spurious col
      dplyr::rename(F1 = x)
    
  } else if (method == "MAP") {
    datapath.model <- gpath("analysis/models/{benchmark.name}-{model.type}-1-cv-big.rds")
    thetas <- readRDS(datapath.model)$df |>
      dplyr::select("F1")
    
    theta.response.matrix <- merge(thetas, data.item.bank, by=0)[-1]
  } else {
    stop("Method not recognised. MAP or EAPsum supported.")
  }
  
    #merge by rowname, remove spurious col
  
  return(theta.response.matrix)
}

fit.gam <- function(df.train){
  # get columns that start with F
  if ("F2" %in% colnames(df.train)){
    formula <- "score ~ s(F1, bs = 'ad') + s(F2, bs = 'ad')"
  } else {
    formula <- "score ~ s(F1, bs = 'ad')"
  }
  mgcv::gam(as.formula(formula), data = df.train)
}

# =============================================================================
# Run CAT simulations

best.models <- data.frame(benchmark.name = c("hellaswag", "mmlu", "arc", "gsm8k", "truthfulqa", "winogrande"),
                          model.type = c("4PL", "3PL", "4PL", "2PL", "2PL", "4PL"))

for (row in 6:nrow(best.models)){
  BM <- best.models$benchmark.name[row]
  MOD <- best.models$model.type[row]
    
  set.seed(1) #reset one each iter
  
  gprint("🚰 Loading data...")
  
  datapath.model <- gpath("analysis/models/{BM}-{MOD}-1-cv-big.rds")
  full <- readRDS(datapath.model)
  model <- full$model
  rm(full)
  
  item.bank <- generate.item.bank(model, BM, MOD)
  
  # Generation of thetas and response matrix for existing models
  gprint("⚙️ Computing thetas for {BM}-{MOD} on full set.")
  
  theta.response.matrix <- generate.theta.response.matrix(model, BM, MOD, item.bank, method = ifelse(BM == "hellaswag", "MAP", "EAPsum"))
  
  gprint("⚙️ Simulating CAT...")
  tryCatch({
    simulation <- catR::simulateRespondents(thetas = theta.response.matrix$F1, 
                                            itemBank = item.bank[,1:4], 
                                            rmax = 1, 
                                            start = list(theta = c(-1:1),
                                                         randomesque = 1,
                                                         startSelect = "MFI"), 
                                            test = list(method = ifelse(BM == "hellaswag", "BM", "EAP"), #MAP is appropriate for hellaswag, otherwise, EAP.
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
                               paste0(c("/catsim-big", BM, MOD), collapse = "-"),
                               "pdf"
              )
    )
    
    outpath <- gpath("analysis/cat/catsim-big-{BM}-{MOD}.rds")
    saveRDS(simulation, outpath)
    gprint("✅ Simulation success!")
    rm(simulation, model, p, theta.response.matrix, thetas, item.bank)
  }, error = function(err){
    gprint("❌ Error in simulation!")
    if (verbose.errors) {
      gprint("Error readout: {err}")
    }
  })
}

gprint("✅ CAT simulation finished!")

# =============================================================================
# Recover overall benchmark score using CAT-estimated thetas

gprint("⚙️ Attempting score recovery from estimated abilities...")

rmses <- data.frame(benchmark.model = c(), rmse = c())
for (row in 1:nrow(best.models)){
  BM <- best.models$benchmark.name[row]
  MOD <- best.models$model.type[row]
  
  datapath <- gpath("analysis/cat/catsim-big-{BM}-{MOD}.rds")
  sim <- readRDS(datapath)
  estimated.thetas <- sim$thetas
  rm(sim)
  
  if (benchmark.name == "mmlu") {
    datapath <- gpath("data/{benchmark.name}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{benchmark.name}-preproc-split.rds")
  }
  
  preproc <- readRDS(datapath)
  data.train <- preproc$data.train
  data.test <- preproc$data.test
  nc <- preproc$max.points.orig
  scores.train <- preproc$scores.train / nc * 100
  scores.test <- preproc$scores.test / nc * 100
  
  rm(preproc, data.train, data.test, nc)
  
  theta.train <- estimated.thetas[1:length(scores.train)]
  theta.test <- estimated.thetas[(length(scores.train)+1):(length(scores.train)+length(scores.test))]
  
  df.train <- data.frame(score = scores.train, F1 = theta.train)
  
  mod.score <- fit.gam(df.train)
  df.train$p <- predict(mod.score)
  
  df.test <- data.frame(score = scores.test, F1 = theta.test)
  
  df.test$p <- predict(mod.score, newdata = df.test)
  
  rmse <- df.test |>
    dplyr::mutate(error = score - p) |>
    dplyr::summarise(rmse = sqrt(mean(error^2)))
  df <- data.frame(benchmark.model = paste0(BM, " (", MOD, ")"), rmse = rmse$rmse[1])
  rmses <- dplyr::bind_rows(rmses, df)
  
  gprint("✅ RMSE for CAT simulations on score recovery for {BM} with model {MOD}...")
  gprint("RMSE: {rmse$rmse[1]}")
  
  rm(theta.train, theta.test, df.train, df.test, mod.score, rmse, df)
  
}

outpath <- gpath("analysis/cat/big-score-recovery-rmse.rds")
saveRDS(rmses, outpath)

rm(outpath, rmses)

# =============================================================================
# Summarize test lengths and theta accuracies

gprint("⚙️ Producing summaries of CAT simulations...")

test.lengths <- data.frame(benchmark.model = c(), test.length = c())
for (row in 1:nrow(best.models)){
  BM <- best.models$benchmark.name[row]
  MOD <- best.models$model.type[row]
  datapath <- gpath("analysis/cat/catsim-big-{BM}-{MOD}.rds")
  sim <- readRDS(datapath)
  test.length <- sim$numberItems
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ")"), length(test.length)), test.length = test.length)
  test.lengths <- dplyr::bind_rows(test.lengths, df)
}

cat.accuracies <- data.frame(benchmark.model = c(), assigned.theta = c(), cat.estimated.theta = c())
for (row in 1:nrow(best.models)){
  BM <- best.models$benchmark.name[row]
  MOD <- best.models$model.type[row]
  datapath <- gpath("analysis/cat/catsim-big-{BM}-{MOD}.rds")
  sim <- readRDS(datapath)
  assigned.thetas <- sim$final.values.df$true.theta
  estimated.thetas <- sim$final.values.df$estimated.theta
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ")"), length(assigned.thetas)), assigned.theta = assigned.thetas, cat.estimated.theta = estimated.thetas)
  cat.accuracies <- dplyr::bind_rows(cat.accuracies, df)
}

cat.accuracies.summary <- cat.accuracies |>
  dplyr::group_by(benchmark.model) |>
  dplyr::summarise(RMSE = sqrt(mean((assigned.theta - cat.estimated.theta)^2)))

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

outpath <- gpath("analysis/cat/big-overall-summaries.rds")
saveRDS(overall.summary, outpath)

rm(list = ls())
gprint("✅ Finished!")