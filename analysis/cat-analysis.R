# Get test items for each benchmark and adaptively test models to estimate theta:
#   0. Load helper functions
#   1. Load CAT simulation data
#   2. Plot CAT results
#   3. Compute error on score recovery
#   4. Summarise test lengths and accuracies for tables

# =============================================================================
# custom utils, args, path, seed
box::use(./utils[gprint, gpath])
box::use(catR)
here::i_am("analysis/cat.R")
set.seed(1)

#==============================================================================
# helper functions

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
# Load data

sim.outputs <- c(readRDS(gpath("analysis/cat/catsim-results-hellaswag-arc-gsm8k.rds")),
                 readRDS(gpath("analysis/cat/catsim-results-mmlu-truthfulqa-winogrande.rds"))
)

rmses <- data.frame(benchmark.model = c(), rmse = c())

for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- switch(sim.outputs[[s]]$method,
                 "BM" = "MAP",
                 "EAP" = "EAP")

  gprint("📊 Plotting simulation results for {BM} {MOD} {METH} model...")
  p <- plot(sim,
            save.plot = T,
            save.options = c(gpath("analysis/cat/"),
                             paste0(c("/catsim", BM, MOD, METH), collapse = "-"),
                             "pdf")
            )
  rm(p)

  gprint("⚙️ Computing RMSE for CAT simulations on score recovery for {BM} with model {MOD}...")

  estimated.thetas <- sim$thetas
  rm(sim)

  if ((BM == "mmlu" | BM == "hellaswag") && METH == "EAP") {
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
    df <- data.frame(benchmark.model = paste0(BM, " (", MOD, ", ", METH, ")"), rmse = rmse$rmse[1])
  rmses <- dplyr::bind_rows(rmses, df)

  gprint("RMSE: {rmse$rmse[1]}")
}

rmses <- rmses |>
  dplyr::rename(`RMSE (Score Recovery)` = rmse)

outpath <- gpath("analysis/cat/cat-score-recovery-rmse.rds")
saveRDS(rmses, outpath)

rm(outpath)

# # =============================================================================
# # Summarize test lengths and theta accuracies

gprint("⚙️ Producing summaries of CAT simulations...")

test.lengths <- data.frame(benchmark.model = c(), test.length = c())
for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- switch(sim.outputs[[s]]$method,
                 "BM" = "MAP",
                 "EAP" = "EAP")
  
  test.length <- sim$numberItems
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ", ", METH, ")"), length(test.length)), test.length = test.length)
  test.lengths <- dplyr::bind_rows(test.lengths, df)
}

cat.accuracies <- data.frame(benchmark.model = c(), assigned.theta = c(), cat.estimated.theta = c())
for (s in 1:length(sim.outputs)){
  sim <- sim.outputs[[s]]$sim.results
  BM <- sim.outputs[[s]]$benchmark.name
  MOD <- sim.outputs[[s]]$model.type
  METH <- switch(sim.outputs[[s]]$method,
                 "BM" = "MAP",
                 "EAP" = "EAP")
  
  assigned.thetas <- sim$final.values.df$true.theta
  estimated.thetas <- sim$final.values.df$estimated.theta
  df <- data.frame(benchmark.model = rep(paste0(BM, " (", MOD, ", ", METH, ")"), length(assigned.thetas)), 
                                          assigned.theta = assigned.thetas, cat.estimated.theta = estimated.thetas)
  cat.accuracies <- dplyr::bind_rows(cat.accuracies, df)
}

cat.accuracies.summary <- cat.accuracies |>
  dplyr::group_by(benchmark.model) |>
  dplyr::summarise(`RMSE (Theta)` = sqrt(mean((assigned.theta - cat.estimated.theta)^2)))

test.lengths.summary <- test.lengths |>
  dplyr::group_by(`benchmark.model`) |>
  dplyr::summarise(`Mean Test Length` = mean(test.length),
                   `Median Test Length` = median(test.length))

test.lengths.prop.summary <- test.lengths |>
  dplyr::group_by(benchmark.model) |>
  dplyr::mutate(proportion.items = test.length/max(test.length)) |>
  dplyr::arrange(proportion.items) |>
  dplyr::transmute(proportion.items = proportion.items,
                   benchmark.model = benchmark.model,
                   proportion.llms = dplyr::row_number(),                    
                   proportion.llms = proportion.llms/dplyr::n()) |>
  dplyr::group_by(benchmark.model) |>
  dplyr::summarise(`Mean Proportion Items Administered` = mean(proportion.items),
                   `Median Proportion Items Administered` = median(proportion.items))

# # =============================================================================
# # Combine summaries

overall.summary <- cat.accuracies.summary |>
  dplyr::inner_join(test.lengths.summary) |>
  dplyr::inner_join(test.lengths.prop.summary) |>
  dplyr::inner_join(rmses) |>
  dplyr::rename(`Benchmark (Model, Method)` = benchmark.model)

outpaths <- c(gpath("analysis/cat/cat-overall-summaries.rds"),
              gpath("analysis/cat/cat-overall-summaries.csv")
              )
saveRDS(overall.summary, outpaths[1])
write.csv(overall.summary, file = outpaths[2], row.names = FALSE)

rm(list = ls())
gprint("🏁 Finished!")
