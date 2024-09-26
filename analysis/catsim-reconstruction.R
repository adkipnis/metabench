# reconstruct scores from catR
# usage: Rscript catsim-reconstruction.R {benchmark}

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[parse.args, gprint, gpath, rowmerge, napply, mytheme])
parse.args(names = c("BM", "MOD", "METH"),
           defaults = c("winogrande", "4PL", "EAP"),
           legal = list(
             BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"),
             MOD = c("2PL", "3PL", "4PL"),
             METH = c("BM", "EAP")
            )
)
here::i_am("analysis/catsim-reconstruction.R")
benchmarks <- list(
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
                   theta.est = results$estimatedThetas)
  row.names(df.full) <- row.names(data.joint.sorted)
  df.full <- rowmerge(df.full, scores.joint)
  df.train <- df.full[row.names(preproc$data.train),]
  df.test <- df.full[row.names(preproc$data.test),]
  
  return(list(df.train=df.train, df.test=df.test))
}

merge.data <- function(){
  # extract estimated theta
  df.train.list <- lapply(data, function(x) x$df.train |> dplyr::select(theta.est))
  df.test.list <- lapply(data, function(x) x$df.test |> dplyr::select(theta.est))

  # rename columns
  df.train.list <- lapply(names(df.train.list), function(name) {
    colnames(df.train.list[[name]]) <- name
    return(df.train.list[[name]])
  })
  df.test.list <- lapply(names(df.test.list), function(name) {
    colnames(df.test.list[[name]]) <- name
    return(df.test.list[[name]])
  })
  
  # add score column
  df.train.list[[7]] <- df.train |> dplyr::select(score)
  df.test.list[[7]] <- df.test |> dplyr::select(score)
  
  # merge data
  df.train.all <- Reduce(rowmerge, df.train.list)
  df.test.all <- Reduce(rowmerge, df.test.list)

  invisible(list2env(list(df.train.all = df.train.all, df.test.all = df.test.all), globalenv() ))
}

rmse <- function(df){
  sqrt(mean((df$p - df$score)^2))
}

# =============================================================================
# load catR results
path1 = "~/catsim-results-hellaswag-arc-gsm8k.rds"
path2 = "~/catsim-results-mmlu-truthfulqa-winogrande.rds"
catsim.results <- c(readRDS(path1), readRDS(path2))

# preprocess data for all benchmarks
data <- lapply(benchmarks, function(x) do.call(preprocess, x))
names(data) <- names(benchmarks)
df.train <- data[[BM]]$df.train
df.test <- data[[BM]]$df.test


# =============================================================================
# Fit GAM
# 1. only on true theta
mod.single <- mgcv::gam(score ~ s(theta, bs = 'ad'), data = df.train)
df.train$p <- predict(mod.single)
df.test$p <- predict(mod.single, df.test)
gprint("Score RMSE (true theta) - {round(rmse(df.train), 3)} (train), {round(rmse(df.test), 3)} (test)")

# 2. only on est theta 
mod.single.est <- mgcv::gam(score ~ s(theta.est, bs = 'ad'), data = df.train)
df.train$p <- predict(mod.single.est)
df.test$p <- predict(mod.single.est, df.test)
gprint("Score RMSE (estimated theta) - {round(rmse(df.train), 3)} (train), {round(rmse(df.test), 3)} (test)")

# 3. train on true, test on est
mod.single <- mgcv::gam(score ~ s(theta, bs = 'ad'), data = df.train)
df.test.cross <- df.test
df.test.cross$theta <- df.test.cross$theta.est
df.test.cross$p <- predict(mod.single, df.test.cross)
gprint("Score RMSE (train on true, test on est) - {round(rmse(df.test.cross), 3)} (test)")

# 4. train on all est theta
merge.data()
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
gprint("Score RMSE (joint estimated theta) - {round(rmse(df.train.all), 3)} (train), {round(rmse(df.test.all), 3)} (test)")
