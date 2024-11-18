# alternative test: item selection via clustering
box::use(./utils[parse.args, mkdir, gprint, gpath, napply, run.mirt, get.theta])
box::use(./reduce.utils[...])

# parse.args: benchmark, k-means
here::i_am("analysis/clustering.R")
parse.args(
   names = c("BM", "seed"),
   defaults = c("arc", 1),
   legal = list(
     BM = c("arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande")
   )
)
seed <- as.integer(seed)
set.seed(seed)
model.types <- c("2PL", "3PL", "4PL")
n.clust <- c(10, 15, 20, 25, 30)

# =============================================================================
# helper functions

merge.params <- function(items, model){
   mirt::coef(model, simplify=T, rotate="none")$items |>
      data.frame() |>
      tibble::rownames_to_column(var='item') |>
      dplyr::left_join(items, by="item")
}

get.params <- function(df.items){
   df.items |>
      dplyr::select(item, a1, d, g, u) |>
      tibble::column_to_rownames(var='item') |>
      as.matrix()
}

collect.all <- function(model.type){
   fitpath <- gpath("analysis/models/{BM}-{model.type}-1-cv-seed={seed}.rds")
   results <- readRDS(fitpath)
   model <- results$model
   df.items <- merge.params(items, model)
   get.params(df.items)
}

get.clusters <- function(item.params, model, k){
   df.items <- item.params[[model]]
   clusters <- kmeans(df.items, k, iter.max = 10, nstart = 1)$cluster
   tidyr::tibble(item = rownames(df.items), cluster = clusters)
}

sample.indices <- function(cluster.indices, cluster.table){
   subroutine <- function(i){
      cluster.table |>
         dplyr::filter(cluster.table$cluster == i) |>
         dplyr::sample_n(1) |>
         dplyr::pull(item)
   }
   indices <- lapply(cluster.indices, subroutine) |>
      unlist()
}
# cluster[cluster$item == indices[1],] # sanity check

sample.wrapper <- function(goal, cluster){
   # apply sample.indices to cluster repeatedy until the number of indices
   # is equal to the goal
   n <- 0
   indices <- c()
   while (n < goal){
      cluster.indices <- unique(cluster$cluster)
      item.indices <- sample.indices(cluster.indices, cluster)
      indices <- c(indices, item.indices)
      cluster <- cluster |>
         dplyr::filter(!item %in% indices)
      n <- n + length(item.indices)
   }
   indices[1:goal]
}

make.df.score <- function(scores, theta) {
   data.frame(score = scores, theta = theta[,1]) |>
      dplyr::mutate(rank.score = rank(score),
                    perc.score = rank.score / max(rank.score),
                    rank.theta = rank(theta),
                    perc.theta = rank.theta / max(rank.theta))
}

get.score.table <- function(theta.train, theta.test, scores.train, scores.test){
   df.train <- make.df.score(scores.train, theta.train)
   df.test <- make.df.score(scores.test, theta.test)
   mod.score <- mgcv::gam(score ~ s(theta, bs = "ad"), data = df.train)
   df.train$p <- predict(mod.score, df.train)
   df.test$p <- predict(mod.score, df.test)
   df.train$set <- "train"
   df.test$set <- "test"
   rbind(df.train, df.test) |>
      dplyr::mutate(error = score - p)
}

run.cluster.based <- function(k, model.type, theta.type){
   # k-means clustering
   cluster <- get.clusters(item.params, model.type, k)

   # sample items and prepare data
   indices <- sample.wrapper(100, cluster)
   data.train.sub <- data.train[,indices]
   data.val.sub <- data.val[,indices]

   # refit model
   model.sub <- run.mirt(data.train.sub, 1, model.type,
                         tol=1e-4, ncycles=1000)
   theta.train.sub <- get.theta(model.sub, theta.type, data.train.sub)
   theta.val.sub <- get.theta(model.sub, theta.type, data.val.sub)
   rownames(theta.train.sub) <- rownames(data.train.sub)
   rownames(theta.val.sub) <- rownames(data.val.sub)

   # evaluate subset
   score.table <- get.score.table(theta.train.sub, theta.val.sub,
                                  scores.train, scores.val)
   sfs.sub <- score.stats(score.table)
   list(k=k, model.type=model.type, theta.type=theta.type, fits=sfs.sub)
}
# =============================================================================
# prepare data
gprint("🚰 Loading {BM} data...")
datapath <- gpath("data/{BM}-sub-350-seed={seed}.rds")
full <- readRDS(datapath)
items <- full$items
items$item <- as.character(items$item)
data <- full$data.train
scores <- full$scores.train / full$max.points.orig * 100
indices <- caret::createDataPartition(scores, p = 0.1, list = F)
data.train <- data[-indices,]
data.val <- data[indices,]
data.test <- full$data.test
scores.train <- scores[-indices]
scores.val <- scores[indices]
scores.test <- full$scores.test / full$max.points.orig * 100
rm(full)

# prepare item parameters
gprint("🚰 Loading {BM} fits...")
item.params <- napply(model.types, collect.all)

