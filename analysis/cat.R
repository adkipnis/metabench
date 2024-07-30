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
box::use(doParallel[...], foreach[...])
Saveplots <- T
here::i_am("analysis/cat.R")
set.seed(1)
n.sample <- NULL

#==============================================================================
# helper functions

generate.item.bank <- function(model, benchmark.name, model.type, filter.outliers=FALSE){
  
  datapath.itemfits <- gpath("analysis/itemfits/{benchmark.name}.rds")
  item.fits <- readRDS(datapath.itemfits) |>
    dplyr::filter(itemtype == model.type)
  
  gprint("🚰 Preparing CAT simulation item bank with full set of {nrow(item.fits)} items on {benchmark.name} benchmark ({model.type} model)...")
  
  if (filter.outliers == TRUE){
    item.fits <- item.fits |>
      dplyr::filter(outlier == FALSE) 
    
    gprint("🚰 Keeping {nrow(item.fits)} that are not outliers...")
  }
  
  # Generate item bank (matrix of model parameters per item) from reduced set
  item.bank <- mirt::coef(model, IRTpars=T, simplify=T)$items 
  item.bank <- item.bank[item.fits$item,] #filter outliers
  
  return(item.bank)
}

generate.theta.response.matrix <- function(model, benchmark.name, model.type, item.bank, method = "EAP"){
  
  if ((benchmark.name == "mmlu"|benchmark.name == "hellaswag") && method == "EAP") {
    datapath <- gpath("data/{benchmark.name}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{benchmark.name}-preproc-split.rds")
  }

  data <- readRDS(datapath)
  full.data <- dplyr::bind_rows(data$data.train, data$data.test)
  
  data.item.bank <- full.data[,rownames(item.bank)]
  
  if (method == "EAP") {
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
    stop("Method not recognised. MAP or EAP supported.")
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

# Adapted from catR::simulateRespondents. Identical apart from where commented

simulate.respondents.parallel <- function (thetas, itemBank, responsesMatrix = NULL, 
                                           model = NULL, genSeed = NULL, cbControl = NULL, 
                                           rmax = 1, Mrmax = "restricted", 
                                           start = list(fixItems = NULL, 
                                                        seed = NULL, nrItems = 1, 
                                                        theta = 0, D=1, randomesque = 1, 
                                                        random.seed = NULL, startSelect = "MFI", 
                                                        cb.control=FALSE, random.cb=NULL), 
                                           test = list(method = "BM", priorDist = "norm", 
                                                       priorPar = c(0, 1), weight = "Huber", 
                                                       tuCo = 1, sem.type = "classic",
                                                       sem.exact = FALSE, se.ase = 10, range = c(-4,4), 
                                                       D = 1, parInt = c(-4, 4, 33), itemSelect = "MFI",
                                                       infoType = "observed", randomesque = 1, 
                                                       random.seed = NULL, AP = 1, proRule = "length", 
                                                       proThr = 20, constantPatt = NULL), 
                                           stop = list(rule = "length", 
                                                       thr = 20, alpha = 0.05), 
                                           final = list(method = "BM", priorDist = "norm", 
                                                        priorPar = c(0, 1), weight = "Huber",
                                                        tuCo = 1, sem.type = "classic", 
                                                        sem.exact = FALSE, range = c(-4,4), 
                                                        D = 1, parInt = c(-4, 4, 33), alpha = 0.05),
                                           save.output = FALSE, 
                                           output = c("", "catR", "csv"),
                                           n.cores = NULL, type = "FORK") {
  if (length(thetas) == 1) {
    if (!is.null(responsesMatrix)) {
      resp <- as.matrix(responsesMatrix)
      res <- randomCAT(trueTheta = thetas, itemBank = itemBank, 
                       responses = resp[1, ], model = model, cbControl = cbControl, 
                       start = start, test = test, stop = stop, final = final, 
                       save.output = save.output, output = output, allTheta = TRUE)
    }
    else {
      res <- randomCAT(trueTheta = thetas, itemBank = itemBank, 
                       model = model, cbControl = cbControl, start = start, 
                       test = test, stop = stop, final = final, save.output = save.output, 
                       output = output, allTheta = TRUE)
    }
    return(res)
  }
  else {
    if (!is.null(genSeed)) {
      if (length(genSeed) != length(thetas)) 
        stop("'thetas' and 'genSeed' must have the same length!", 
             call. = FALSE)
    }
    internalSR <- function() {
      start.time <- Sys.time()
      respondents <- length(thetas)
      nAvailable <- NULL
      if (respondents < 1) 
        stop(paste("Length of 'thetas' has and invalid value: ", 
                   length(respondents), sep = ""))
      bank_size <- nrow(itemBank)
      #estimatedThetas <- NULL
      vItemExposure <- NULL
      #kpar <- rep(1, bank_size)
      #last_shown <- -1
      #exposure <- rep(0, bank_size)
      #numberItems <- NULL
      #totalSeFinal <- c()
      #thrOK <- NULL
      if (!is.null(responsesMatrix)) 
        resp <- as.matrix(responsesMatrix)
      if (sum(stop$rule == "length") == 1) 
        itemsRow = stop$thr[stop$rule == "length"]
      else itemsRow = nrow(itemBank)
      row.head1 <- rep("items.administrated", itemsRow)
      row.head1[1:length(row.head1)] <- paste(row.head1[1:length(row.head1)], 
                                              1:length(row.head1), sep = ".")
      row.head2 <- rep("responses", itemsRow)
      row.head2[1:length(row.head2)] <- paste(row.head2[1:length(row.head2)], 
                                              1:length(row.head2), sep = ".")
      row.head3 <- rep("estimated.theta", itemsRow)
      row.head3[1:length(row.head3)] <- paste(row.head3[1:length(row.head3)], 
                                              1:length(row.head3), sep = ".")
      row.head <- c()
      row.head <- c(row.head, "respondent", "true.theta", 
                    row.head1, row.head2, "start.theta", row.head3)
      responses.df <- data.frame()
      
      # Parallelise for loop
      
      if (is.null(n.cores)) {
      	tot.cores <- parallel::detectCores()
        n.cores <- if (tot.cores > 10) {tot.cores - 10} else {tot.cores - 2}
      } 
      
      mu.cluster <- parallel::makeCluster(n.cores, type = type) # to parameterise
      doParallel::registerDoParallel(mu.cluster)
      
      gprint("Running on {n.cores} cores...")
      
      results <- foreach(i = 1:respondents,
                         .packages = c("catR"), 
                         .export = c("rmax", "Mrmax", "thetas","itemBank", "model", 
                                     "genSeed", "cbControl", "start", "test", 
                                     "final", "stop")) %dopar% {
                           
                           kpar <- rep(1, bank_size)
                           exposure <- rep(0, bank_size)
                           exposureRates <- rep(0, bank_size)
                           
                           if (rmax < 1) {
                             nAvailable <- rep(1, bank_size)
                             if (Mrmax == "restricted") 
                               nAvailable[exposureRates >= rmax] <- 0
                             if (Mrmax == "IE") {
                               kpar[(exposureRates/kpar) <= rmax] <- 1
                               kpar[(exposureRates/kpar) > rmax] <- rmax * 
                                 kpar[(exposureRates/kpar) > rmax]/exposureRates[(exposureRates/kpar) > 
                                                                                   rmax]
                               nAvailable[runif(bank_size) > kpar] <- 0
                             }
                           }
                           
                           if (exists("resp")) {
                             rCAT <- catR::randomCAT(trueTheta = thetas[i], itemBank = itemBank, 
                                                     responses = resp[i, ], model = model, genSeed = genSeed[i], 
                                                     cbControl = cbControl, nAvailable = nAvailable, 
                                                     start = start, test = test, stop = stop, 
                                                     final = final, allTheta = TRUE)
                           }
                           else {
                             rCAT <- catR::randomCAT(trueTheta = thetas[i], itemBank = itemBank, 
                                                     model = model, genSeed = genSeed[i], cbControl = cbControl, 
                                                     nAvailable = nAvailable, start = start, test = test, 
                                                     stop = stop, final = final, allTheta = TRUE)
                           }
                           
                           #estimatedThetas <- c(estimatedThetas, rCAT$thFinal)
                           #vItemExposure <- c(vItemExposure, rCAT$testItems)
                           exposure[rCAT$testItems[1:length(rCAT$testItems)]] <- exposure[rCAT$testItems[1:length(rCAT$testItems)]] + 
                             1
                           exposureRates = exposure/i
                           #numberItems <- c(numberItems, length(rCAT$testItems))
                           #totalSeFinal <- c(totalSeFinal, rCAT$seFinal)
                           if (!is.null(rCAT$ruleFinal)) 
                             #thrOK <- c(thrOK, 1)
                             thrOK <- 1
                           else thrOK <- 0 #thrOK <- c(thrOK, 0)
                           items.administrated <- rep(-99, itemsRow)
                           responses <- rep(-99, itemsRow)
                           provisional.theta <- rep(-99, itemsRow)
                           items.administrated[1:length(rCAT$testItems)] <- rCAT$testItems
                           responses[1:length(rCAT$pattern)] <- rCAT$pattern
                           if (rCAT$startNrItems == 0) 
                             rCAT$thetaProv <- rCAT$thetaProv[2:length(rCAT$thetaProv)]
                           provisional.theta[1:length(rCAT$thetaProv)] <- rCAT$thetaProv
                           
                           row <- c(i, rCAT$trueTheta, items.administrated, 
                                    responses, rCAT$startTheta, provisional.theta)
                           
                           iter.res <- list(results = row,
                                            estimated.theta = rCAT$thFinal,
                                            v.item.exposure = rCAT$testItems,
                                            exposure.rates = exposureRates,
                                            number.items = length(rCAT$testItems),
                                            total.se.final = rCAT$seFinal,
                                            thr.ok = thrOK)
                           iter.res
                         }
      
      responses.df <- foreach (sim=1:length(results), .combine=rbind) %dopar% {
        results[[sim]]$results
      }
      
      estimated.thetas <- foreach(sim=1:length(results), .combine=c) %dopar% {
        results[[sim]]$estimated.theta
      }
      
      exposure.rates <- foreach(sim=1:length(results), .combine=c) %dopar% {
        results[[sim]]$exposure.rates
      }
      
      total.se.final <- foreach(sim=1:length(results), .combine=c) %dopar% {
        results[[sim]]$total.se.final
      }
      
      number.items <- foreach(sim=1:length(results), .combine=c) %dopar% {
        results[[sim]]$number.items
      }
      
      thr.ok <- foreach(sim=1:length(results), .combine=c) %dopar% {
        results[[sim]]$thr.ok
      }
      
      parallel::stopCluster(mu.cluster)
      
      responses.df <- as.data.frame(responses.df)
      colnames(responses.df) <- row.head
      final.values.df <- data.frame(thetas, estimated.thetas, 
                                    total.se.final, number.items)
      colnames(final.values.df) <- c("true.theta", "estimated.theta", 
                                     "final.SE", "total.items.administrated")
      resCor <- cor(thetas, estimated.thetas)
      RMSE <- sqrt(sum((estimated.thetas - thetas)^2)/respondents)
      bias <- sum(estimated.thetas - thetas)/respondents
      testLength = sum(exposure.rates)
      position_min <- which(exposure.rates == min(exposure.rates))
      position_max <- which(exposure.rates == max(exposure.rates))
      overlap <- sum(exposure.rates^2)/testLength
      condTheta <- rep(0, 10)
      condRMSE <- rep(0, 10)
      condBias <- rep(0, 10)
      condnItems <- rep(0, 10)
      condSE <- rep(0, 10)
      condthrOK <- rep(0, 10)
      ndecile <- rep(0, 10)
      for (z in 1:10) {
        if (z < 10) 
          subset <- which(findInterval(thetas, quantile(thetas, 
                                                        seq(0, 1, 0.1))) == z)
        else subset <- c(which(findInterval(thetas, quantile(thetas, 
                                                             seq(0, 1, 0.1))) == z), 
                         which(thetas == max(thetas)))
        condTheta[z] <- mean(thetas[subset])
        condRMSE[z] <- sqrt(sum((estimated.thetas[subset] - 
                                   thetas[subset])^2)/length(subset))
        condBias[z] <- sum(estimated.thetas[subset] - 
                             thetas[subset])/length(subset)
        condnItems[z] <- mean(number.items[subset])
        condSE[z] <- mean(total.se.final[subset])
        condthrOK[z] <- mean(thr.ok[subset])
        ndecile[z] <- length(subset)
      }
      finish.time <- Sys.time()
      res <- list(thetas = thetas, itemBank = itemBank, 
                  responsesMatrix = responsesMatrix, model = model, 
                  genSeed = genSeed, cbControl = cbControl, rmax = rmax, 
                  Mrmax = Mrmax, start = start, test = test, stop = stop, 
                  final = final, save.output = save.output, output = output, 
                  estimatedThetas = estimated.thetas, correlation = resCor, 
                  bias = bias, RMSE = RMSE, thrOK = thr.ok, exposureRates = exposure.rates, 
                  testLength = testLength, overlap = overlap, numberItems = number.items, 
                  condTheta = condTheta, condBias = condBias, condRMSE = condRMSE, 
                  condnItems = condnItems, condSE = condSE, condthrOK = condthrOK, 
                  ndecile = ndecile, final.values.df = final.values.df, 
                  responses.df = responses.df, start.time = start.time, 
                  finish.time = finish.time)
      class(res) <- "catResult"
      return(res)
    }
    resToReturn <- internalSR()
    if (save.output) {
      if (output[2] != "") 
        output[2] <- paste0(output[2], ".")
      if (output[1] == "") 
        wd <- paste(getwd(), "/", sep = "")
      else wd <- output[1]
      fileName1 <- paste(wd, output[2], "main.", output[3], 
                         sep = "")
      fileName2 <- paste(wd, output[2], "responses.", output[3], 
                         sep = "")
      fileName3 <- paste(wd, output[2], "tables.", output[3], 
                         sep = "")
      fileName4 <- paste(wd, output[2], "deciles.", output[3], 
                         sep = "")
      capture.output(resToReturn, file = fileName1)
      if (output[3] == "csv") 
        sep <- ";"
      else sep <- "\t"
      write.table(resToReturn$responses.df, fileName2, 
                  quote = FALSE, sep = sep, row.names = FALSE)
      write.table(resToReturn$final.values.df, file = fileName3, 
                  sep = sep, quote = FALSE, row.names = FALSE)
    }
    return(resToReturn)
  }
}

# =============================================================================
# Run CAT simulations

best.models <- data.frame(benchmark.name = c("hellaswag", "mmlu", "arc", "gsm8k", "truthfulqa", "winogrande"),
                          model.type = c("4PL", "3PL", "4PL", "2PL", "2PL", "4PL"),
                          method = c("EAP", "EAP", "MAP", "EAP", "EAP", "EAP"))

results <- list()

for (row in 1:nrow(best.models)){
  BM <- best.models$benchmark.name[row]
  MOD <- best.models$model.type[row]
  METH <- best.models$method[row]
  
  set.seed(1) #reset one each iter
  
  gprint("🚰 Loading data...")
  
  datapath.model <- gpath("analysis/models/{BM}-{MOD}-1-cv-big.rds")
  full <- readRDS(datapath.model)
  model <- full$model
  rm(full)
  
  item.bank <- generate.item.bank(model, BM, MOD)
  
  # Generation of thetas and response matrix for existing models
  gprint("⚙️ Computing thetas for {BM}-{MOD} on full set.")
  
  theta.response.matrix <- generate.theta.response.matrix(model, BM, MOD, item.bank, 
                                                          method = METH)
  
  rm(model)
  
  theta.response.matrix <- if (is.null(n.sample)) theta.response.matrix else dplyr::slice_sample(theta.response.matrix, n=n.sample) 
  
  simulation <- simulate.respondents.parallel(thetas = theta.response.matrix$F1, 
                                              itemBank = item.bank[,1:4], 
                                              rmax = 1, 
                                              start = list(theta = c(-1:1),
                                                           randomesque = 1,
                                                           startSelect = "MFI"), 
                                              test = list(method = METH, #MAP is appropriate for hellaswag, otherwise, EAP.
                                                          priorDist = "norm",
                                                          priorPar = c(0,1),
                                                          itemSelect = "MFI",
                                                          range = c(-7, 5),
                                                          parInt = c(-7, 5, 52)), 
                                              stop = list(rule = "precision", 
                                                          thr = 0.1), 
                                              final = list(method = METH, 
                                                           priorDist = "norm",
                                                           priorPar = c(0,1),
                                                           range = c(-7, 5),
                                                           parInt = c(-7, 5, 52)), 
                                              genSeed = sample.int(nrow(theta.response.matrix)), #generate seed list of random +ve ints
                                              responsesMatrix = theta.response.matrix[-1],
                                              n.cores = NULL,
                                              type = "FORK"
  )
  
  results[[row]] <- list(sim.results = simulation, benchmark.name = BM, 
                         model.type = MOD, method = method)
  
  saveRDS(results, gpath("analysis/cat/catsim.rds"))
  
}

gprint("✅ Simulation success!")

# =============================================================================
# Run post simulation analyses

if (!exists("sim.outputs")){
  inpath <- gpath("analysis/cat/catsim.rds")
  sim.outputs <- readRDS(inpath)
}

rmses <- data.frame(benchmark.model = c(), rmse = c())

for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- sim.outputs[[s]]$method
  
  gprint("📊 Plotting simulation results for {BM} {MOD} model...")
  for (subplot in c("trueEst", "expRate", "cumExpRate", "cumNumberItems", "expRatePara",
                    "condBias", "condRMSE", "numberItems", "sError", "condThr")){
    p <- plot(sim,
              type = subplot,
              save.plot = Saveplots,
              save.options = c(gpath("analysis/cat/"),
                               paste0(c("/catsim", BM, MOD, METH, subplot), collapse = "-"),
                               "pdf")
              )
  }
  
  gprint("⚙️ Computing RMSE for CAT simulations on score recovery for {BM} with model {MOD}...")
  
  estimated.thetas <- sim$thetas
  rm(sim)
  
  if ((BM == "mmlu" || BM == "hellaswag") && METH == "EAP") {
    datapath <- gpath("data/{BM}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{BM}-preproc-split.rds")
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
  
  gprint("RMSE: {rmse$rmse[1]}")
}

outpath <- gpath("analysis/cat/cat-score-recovery-rmse.rds")
saveRDS(rmses, outpath)

rm(outpath, rmses)

# =============================================================================
# Summarize test lengths and theta accuracies

if (!exists("sim.outputs")){
  inpath <- gpath("analysis/cat/catsim.rds")
  sim.outputs <- readRDS(inpath)
}

gprint("⚙️ Producing summaries of CAT simulations...")

test.lengths <- data.frame(benchmark.model = c(), test.length = c())
for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- sim.outputs[[s]]$method
  
  test.length <- sim$numberItems
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ", ", METH, ")"), length(test.length)), test.length = test.length)
  test.lengths <- dplyr::bind_rows(test.lengths, df)
}

cat.accuracies <- data.frame(benchmark.model = c(), assigned.theta = c(), cat.estimated.theta = c())
for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- sim.outputs[[s]]$method
  
  assigned.thetas <- sim$final.values.df$true.theta
  estimated.thetas <- sim$final.values.df$estimated.theta
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ", ", METH, ")"), length(assigned.thetas)), assigned.theta = assigned.thetas, cat.estimated.theta = estimated.thetas)
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

outpath <- gpath("analysis/cat/catsim-overall-summaries.rds")
saveRDS(overall.summary, outpath)

rm(list = ls())
gprint("🏁 Finished!")
