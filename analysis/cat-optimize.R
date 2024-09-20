# Get test items for each benchmark and adaptively test models to estimate theta:
#   0. Load helper functions
#   1. Load data and models
#   2. Run CAT on a subset of the respondents to find good parameters

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath, get.theta])
box::use(doParallel[...], foreach[...])
here::i_am("analysis/cat.R")
num.data.points <- 40 # The number of models to sample for optimizing CAT simulation
set.seed(1)
warnStatus <- getOption("warn")

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

cat.optimize <- function(parameter.space.row, bm.data, num.data.points=10, start.select=NULL,test.method=NULL, item.select=NULL){
  start.select <- if (is.null(start.select)) parameter.space.row$startSelect else start.select
  test.method <- if (is.null(test.method)) parameter.space.row$testMethod else test.method
  item.select <- if (is.null(item.select)) parameter.space.row$itemSelect else item.select
  
  gprint("Finding optimal parameters for {bm.data$benchmark.name} fitted with {bm.data$model.type} ({bm.data$method}) using {num.data.points} data points...")
  gprint("Attempting the following parameters")
  gprint("Initial item selection criterion: {start.select}")
  gprint("Iterative theta estimation procedure: {test.method}")
  gprint("Next item selection criterion: {item.select}")
  theta.response.matrix <- bm.data$theta.response.matrix |>
    dplyr::arrange(F1) |> 
    dplyr::filter(dplyr::row_number() %% floor(5221/num.data.points) == 0) # assuming constant sample size
  
  item.bank <- bm.data$item.bank
  BM <- bm.data$benchmark.name
  MOD <- bm.data$model.type
  METH <- bm.data$method
  METH <- switch(METH,
                 "EAP" = "EAP",
                 "MAP" = "BM")
  
  simulation <- catR::simulateRespondents(thetas = theta.response.matrix$F1,
                                          itemBank = item.bank[, 1:4],
                                          rmax = 1,
                                          start = list(
                                            fixItems = NULL, #default
                                            seed = NULL, #default
                                            nrItems = 1, #default
                                            theta = c(-2:2),
                                            randomesque = 1,
                                            random.seed = NULL, #default
                                            startSelect = start.select,
                                            cb.control = FALSE, #default
                                            random.cb = NULL #default
                                          ),
                                          test = list(
                                            method = test.method,
                                            priorDist = "norm", #default
                                            priorPar = c(0, 2), #slightly larger than default (0,1)
                                            weight = "Huber", #default
                                            tuCo = 1, #default
                                            sem.type = "classic", #default
                                            sem.exact = FALSE, #default
                                            se.ase = 10, #default
                                            range = c(-7, 7),
                                            D = 1, #default - logistic metric
                                            parInt = c(-7, 7, 65), 
                                            itemSelect = item.select,
                                            infoType = "observed", #default
                                            randomesque = 1, #default
                                            random.seed = NULL, #default
                                            AP = 1, #default
                                            proRule = "precision",
                                            proThr = 0.1,
                                            constantPatt = NULL
                                          ),
                                          stop = list(rule = "precision", 
                                                      thr = 0.1),
                                          final = list(
                                            method = METH, # use the original estimation method used in fitting
                                            priorDist = "norm",
                                            priorPar = c(0, 2),
                                            range = c(-7, 7),
                                            parInt = c(-7, 7, 64)
                                          ),
                                          genSeed = sample.int(nrow(theta.response.matrix)),
                                          responsesMatrix = theta.response.matrix[-1],
  )
  output <- data.frame(benchmark = bm.data$benchmark.name,
                       finalMethod = bm.data$method,
                       modelType = bm.data$model.type,
                       startSelect = start.select,
                       testMethod = test.method,
                       itemSelect = item.select,
                       RMSE = simulation$RMSE, 
                       testLength = simulation$testLength)
  return(output)
}

# =============================================================================
# Find optimal CAT parameters

options(warn=-1)

bm.list <- c("hellaswag", "arc", "gsm8k", "mmlu", "truthfulqa", "winogrande")
bm.names <- paste0(bm.list, collapse = "-")
data <- get.bm.list(bm.list)

options(warn=warnStatus)

start.select <- c("MFI", "bOpt", "thOpt")
test.method <- c("BM", "ML", "EAP", "WL", "ROB")
item.select <- c("MFI", "bOpt", "thOpt", "proportional", "progressive") # no MLWI, MPWI, MEPV, MEI, KL, or KLP because they are slow...

parameter.space <- expand.grid(startSelect = start.select,
                               testMethod = test.method,
                               itemSelect = item.select) |>
  dplyr::mutate(startSelect = as.character(startSelect),
                testMethod = as.character(testMethod),
                itemSelect = as.character(itemSelect))

results <- data.frame(benchmark = c(),
                      finalMethod = c(),
                      modelType = c(),
                      startSelect = c(),
                      testMethod = c(),
                      itemSelect = c(),
                      RMSE = c(),
                      testLength = c()
                      )

for (e in length(data)){
  n.cores <- parallel::detectCores()  - 1
  gprint("Running on {n.cores}...")
  mu.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile="")
  doParallel::registerDoParallel(mu.cluster)
  
  opt.outputs <- foreach(i = 1:nrow(parameter.space), .errorhandling = "remove", .combine = "rbind", .inorder = FALSE) %dopar% {
    cat.optimize(parameter.space[i,], data[[e]], num.data.points = num.data.points)
  }
  
  parallel::stopCluster(mu.cluster)
  
  results <- dplyr::bind_rows(results, opt.outputs)
  rm(opt.outputs)
}

saveRDS(results, gpath("analysis/cat/cat-optimize-results.rds"))

gprint("🏁 Finished!")
