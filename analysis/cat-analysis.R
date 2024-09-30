# Get test items for each benchmark and adaptively test models to estimate theta:
#   0. Load helper functions
#   1. Load CAT simulation data
#   2. Plot CAT results
#   3. Compute error on score recovery
#   4. Summarise test lengths and accuracies for tables

# reconstruct scores from catR
# usage: Rscript cat-analysis.R

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, rowmerge, napply, mytheme, cbPalette, cbPalette2])
here::i_am("analysis/cat-analysis.R")

benchmarks <- c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
models <- c("2PL", "3PL", "4PL")
methods <- c("BM", "EAP")
simulations <- expand.grid(bm = benchmarks, mod = models, meth = methods, stringsAsFactors = FALSE)

best.benchmarks <- list(
  arc = list(bm = "arc", mod = "4PL", meth = "BM"),
  gsm8k = list(bm = "gsm8k", mod = "2PL", meth = "BM"),
  hellaswag = list(bm = "hellaswag", mod = "4PL", meth = "BM"),
  mmlu = list(bm = "mmlu", mod = "4PL", meth = "BM"),
  truthfulqa = list(bm = "truthfulqa", mod = "3PL", meth = "BM"),
  winogrande = list(bm = "winogrande", mod = "4PL", meth = "EAP")
)
# =============================================================================
# helper functions

find.res <- function(res.list, BM, MOD, METH){
  for (i in 1:length(res.list)){
    res <- res.list[[i]]
    cond.bm <- res$benchmark.name == BM
    cond.mod <- res$model.type == MOD
    cond.meth <- res$method == METH
    if (cond.bm & cond.mod & cond.meth) return(res$sim.results)
  }
}

preprocess <- function(bm, mod, meth){
  # load preprocessed data
  if (bm %in% c("mmlu", "hellaswag")) {
    datapath <- gpath("data/{bm}-sub-1189.rds")
  } else {
    datapath <- gpath("data/{bm}-preproc-split.rds")
  }
  preproc <- readRDS(datapath)
  data.joint <- dplyr::bind_rows(preproc$data.train, preproc$data.test)
  scores.joint <- c(preproc$scores.train, preproc$scores.test) /
    preproc$max.points.orig * 100
  scores.joint <- data.frame(score = scores.joint)
  
  # load cat results
  results <- find.res(catsim.results, bm, mod, meth)
  if (is.null(results)){
    gprint("{bm} result not found for {mod} & {meth}")
    return()
  }
  data.joint.cat <- results$responsesMatrix
  
  # compare joint data
  # nrow(unique(data.joint)) == length(unique(results$thetas))
  name.order <- order(row.names(data.joint))
  data.joint.sorted <- data.joint[name.order, ]
  if (!all(data.joint.sorted == data.joint.cat)) gprint("rownames mismatch")
  
  # prepare data and split
  df.full <- data.frame(theta = results$thetas,
                        theta.est = results$estimatedThetas,
                        num.items = results$numberItems)
  row.names(df.full) <- row.names(data.joint.sorted)
  df.full <- rowmerge(df.full, scores.joint)
  df.train <- df.full[row.names(preproc$data.train),]
  df.test <- df.full[row.names(preproc$data.test),]
  
  return(list(df.train=df.train, df.test=df.test))
}

merge.data <- function(data){
  # extract estimated theta
  df.train.list <- lapply(data, function(x) x$df.train |> dplyr::select(theta.est, num.items))
  df.test.list <- lapply(data, function(x) x$df.test |> dplyr::select(theta.est, num.items))
  
  # rename columns
  df.train.list <- lapply(names(df.train.list), function(name) {
    colnames(df.train.list[[name]]) <- c(name, glue::glue("num.items_{name}"))
    return(df.train.list[[name]])
  })
  df.test.list <- lapply(names(df.test.list), function(name) {
    colnames(df.test.list[[name]]) <- c(name, glue::glue("num.items_{name}"))
    return(df.test.list[[name]])
  })
  
  # add score column
  df.train.list[[7]] <- df.train |> dplyr::select(score)
  df.test.list[[7]] <- df.test |> dplyr::select(score)
  
  # merge data
  df.train.all <- Reduce(rowmerge, df.train.list) |>
    dplyr::mutate(num.items.total = rowSums(dplyr::across(dplyr::starts_with("num.items"))))
  df.test.all <- Reduce(rowmerge, df.test.list) |>
    dplyr::mutate(num.items.total = rowSums(dplyr::across(dplyr::starts_with("num.items"))))
  
  invisible(list2env(list(df.train.all = df.train.all, df.test.all = df.test.all), globalenv() ))
}

rmse <- function(df){
  sqrt(mean((df$p - df$score)^2))
}

plot.rmse.numitems <- function(rmse.data.test, rmse, colour){
  box::use(ggplot2[...], latex2exp[TeX])
  
  rmse.data.test$error <- round(sqrt((rmse.data.test$p - rmse.data.test$score)^2), 3)
  
  text <- glue::glue(
    "r = {round(rmse, 3)}")
  x.label <- 1000
  y.label <- 20
  plot <- ggplot(rmse.data.test, aes(x = num.items, y = error)) +
    geom_point(alpha = 0.5, colour = colour) +
    ylim(0,28) +
    xlim(0,1200) +
    annotate("text", x = x.label, y = y.label, label = text, size = 8) +
    labs(
      title = "Number of Items Administered vs. Score Recovery Error",
      x = "Number of Items Administered",
      y = "Score Recovery Error",
    ) +
    mytheme()
  
  return(plot)
}

# =============================================================================
# load catR results
catsim.results <- c(readRDS(gpath("analysis/cat/catsim-results-hellaswag-arc-gsm8k.rds")),
                    readRDS(gpath("analysis/cat/catsim-results-mmlu-truthfulqa-winogrande.rds"))
)

# preprocess data for all benchmarks
best.data <- lapply(best.benchmarks, function(x) do.call(preprocess, x))
names(best.data) <- names(best.benchmarks)
colourPalette <- cbPalette()

# =============================================================================
# Fit GAMs and store

rmses <- tibble::tibble(`Benchmark` = c(), 
                        `Model` = c(), 
                        `Method` = c(), 
                        `RMSE Train (True Theta)` = c(),
                        `RMSE Test (True Theta)` = c(),
                        `RMSE Train (Est. Theta)` = c(),
                        `RMSE Test (Est. Theta)` = c(),
                        `RMSE Test (Train True Theta, Test Est. Theta)` = c(),
                        `RMSE Train (Joint Est. Theta)` = c(),
                        `RMSE Test (Joint Est. Theta)` = c(),
                        `Median Number of Items (All)` = c(),
                        `Mean Number of Items (All)` = c(),
                        `Median Number of Items (Train)` = c(),
                        `Mean Number of Items (Train)` = c(),
                        `Median Number of Items (Test)` = c(),
                        `Mean Number of Items (Test)` = c(),
                        `Median Total Items (All)` = c(),
                        `Mean Total Items (All)` = c(),
                        `Median Total Items (Train)` = c(),
                        `Mean Total Items (Train)` = c(),
                        `Median Total Items (Test)` = c(),
                        `Mean Total Items (Test)` = c())

for (row in 1:nrow(simulations)){
  BM <- simulations$bm[row]
  MOD <- simulations$mod[row]
  METH <- simulations$meth[row]
  
  sim.results <- preprocess(BM, MOD, METH)
  if (!is.null(sim.results)){
    gprint("Reconstructing for {BM} ({MOD}, {METH})")
    df.train <- sim.results$df.train
    df.test <- sim.results$df.test
    median.num.items <- median(c(df.train$num.items, df.test$num.items))
    mean.num.items <- mean(c(df.train$num.items, df.test$num.items))
    
    # 1. only on true theta
    mod.single <- mgcv::gam(score ~ s(theta, bs = 'ad'), data = df.train)
    df.train$p <- predict(mod.single)
    df.test$p <- predict(mod.single, df.test)
    rmse.true.train <- rmse(df.train)
    rmse.true.test <- rmse(df.test)
    gprint("Score RMSE (true theta) - {round(rmse.true.train, 3)} (train), {round(rmse.true.test, 3)} (test)")
    
    # 2. only on est theta 
    mod.single.est <- mgcv::gam(score ~ s(theta.est, bs = 'ad'), data = df.train)
    df.train$p <- predict(mod.single.est)
    df.test$p <- predict(mod.single.est, df.test)
    rmse.est.train <- rmse(df.train)
    rmse.est.test <- rmse(df.test)
    gprint("Score RMSE (estimated theta) - {round(rmse.est.train, 3)} (train), {round(rmse.est.test, 3)} (test)")
    
    # Plot
    colour <- colourPalette[match(BM, names(best.data))]
    (plot <- plot.rmse.numitems(df.test, rmse.est.test, colour))
    saveRDS(plot, gpath("analysis/cat/cat-score-recovery-plot-{BM}-{MOD}-{METH}.rds"))
    
    # 3. train on true, test on est
    mod.single <- mgcv::gam(score ~ s(theta, bs = 'ad'), data = df.train)
    df.test.cross <- df.test
    df.test.cross$theta <- df.test.cross$theta.est
    df.test.cross$p <- predict(mod.single, df.test.cross)
    rmse.test <- rmse(df.test.cross)
    gprint("Score RMSE (train on true, test on est) - {round(rmse.test, 3)} (test)")
    
    # 4. train on all est theta
    merge.data(best.data)
    mod.joint <- mgcv::gam(score ~
                             s(arc, bs="ad") +
                             s(gsm8k, bs="ad") +
                             s(hellaswag, bs="ad") +
                             s(mmlu, bs="ad") +
                             s(truthfulqa, bs="ad") +
                             s(winogrande, bs="ad"),
                           data = df.train.all)
    df.train.all$p <- predict(mod.joint)
    df.test.all$p <- predict(mod.joint, df.test.all)
    rmse.joint.train <- rmse(df.train.all)
    rmse.joint.test <- rmse(df.test.all)
    median.total.items <- median(c(df.train.all$num.items.total, df.test$num.items.total))
    mean.total.items <- mean(c(df.train.all$num.items.total, df.test$num.items.total))
    gprint("Score RMSE (joint estimated theta) - {round(rmse.joint.train, 3)} (train), {round(rmse.joint.test, 3)} (test)")
    
    bm.rmse <- tibble::tibble(`Benchmark` = BM, 
                              `Model` = MOD, 
                              `Method` = METH, 
                              `RMSE Train (True Theta)` = rmse.true.train,
                              `RMSE Test (True Theta)` = rmse.true.test,
                              `RMSE Train (Est. Theta)` = rmse.est.train,
                              `RMSE Test (Est. Theta)` = rmse.est.test,
                              `RMSE Test (Train True Theta, Test Est. Theta)` = rmse.test,
                              `RMSE Train (Joint Est. Theta)` = rmse.joint.train,
                              `RMSE Test (Joint Est. Theta)` = rmse.joint.test,
                              `Median Number of Items (All)` = median.num.items,
                              `Mean Number of Items (All)` = mean.num.items,
                              `Median Number of Items (Train)` = median(df.train$num.items),
                              `Mean Number of Items (Train)` = mean(df.train$num.items),
                              `Median Number of Items (Test)` = median(df.test$num.items),
                              `Mean Number of Items (Test)` = mean(df.test$num.items),
                              `Median Total Items (All)` = median.total.items,
                              `Mean Total Items (All)` = mean.total.items,
                              `Median Total Items (Train)` = median(df.train.all$num.items.total),
                              `Mean Total Items (Train)` = mean(df.train.all$num.items.total),
                              `Median Total Items (Test)` = median(df.test.all$num.items.total),
                              `Mean Total Items (Test)` = mean(df.test.all$num.items.total))
    
    rmses <- dplyr::bind_rows(rmses, bm.rmse)
  }
}

outpath <- gpath("analysis/cat/cat-score-recovery-rmse-num-items.csv")
write.csv(rmses, outpath, row.names = FALSE)

rm(list = ls())
gprint("🏁 Finished!")
