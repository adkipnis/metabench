#" @export
parse.args <- function(names, defaults, legal=NULL) {
   args <- commandArgs(trailingOnly = T)
   opts <- list()
   for (i in 1:length(names)) {
      name <- names[i]
      if (i <= length(args)) {
         opts[[name]] <- args[i]
      } else {
         opts[[name]] <- defaults[i]
      }
      if (!is.null(legal) && !opts[[name]] %in% legal[[name]]) {
         stop("Invalid option for ", name)
      }
   }
   invisible(list2env(opts, envir = globalenv()))
}

#" @export
mkdir <- function(relpath) {
   fullpath <- here::here(relpath)
   if (!dir.exists(fullpath)) {
      dir.create(fullpath)
   }
}


#" @export
gprint <- function(s) {
   eval(substitute(print(glue::glue(s))), parent.frame())
}


#" @export
gpath <- function(s) {
   eval(substitute(here::here(glue::glue(s))), parent.frame())
}

#" @export
rowmerge <- function(df1, df2){
   merge(df1, df2, by="row.names") |>
     tibble::column_to_rownames("Row.names")
}

#" @export
df2data <- function(df) {
   df |>
      dplyr::mutate(correct = as.integer(correct)) |>
      tidyr::pivot_wider(names_from = item, values_from = correct) |>
      tibble::column_to_rownames(var = "source")
}

#" @export
prop.indices <- function(scores, p = 0.1) {
   # sample indices proportional to distribution
   n <- length(scores)
   k <- round(n * p)
   pmf <- stats::density(scores)
   pmf$y <- pmf$y / sum(pmf$y)
   x.values <- findInterval(scores, pmf$x)
   sample(1:n, k, prob = pmf$y[x.values])
}


#" @export
run.mirt <- function(data, dimension, itemtype, tol=1e-4, ncycles=1000){
   if (dimension == 1){
      method <- "EM"
   } else {
      method <- "MHRM"
   }
   mirt::mirt(data, dimension, itemtype=itemtype,
               method=method,
               density="Gaussian",
               TOL=tol,
               technical=list(NCYCLES=ncycles))
}

#" @export
get.theta <- function(model, method="MAP", resp = NULL) {
  use_dentype_estimate <- method %in% c("EAPsum", "EAP")
  mirt::fscores(
    model,
    method = method,
    use_dentype_estimate = use_dentype_estimate,
    response.pattern = resp
  )
}

#" @export
do.fa <- function(raw, nfactors, verbose = T){
   if (verbose){
      gprint("\n\nStarting factor analysis...")
   }
   res <- psych::fa(
      raw,
      nfactors = nfactors,
      rotate = "oblimin",
      fm = "minres",
      n.obs = nrow(raw),
   )
   if (verbose){
      gprint("\n")
      evaluate.fa.fit(res)
   }
   res
}

#" @export
do.fa.cov <- function(covmat, nfactors, n.obs, verbose = T){
   if (verbose){
      gprint("\n\nStarting factor analysis...")
   }
   res <- psych::fa(covmat,
                    nfactors = nfactors,
                    rotate = "oblimin",
                    fm = "minres",
                    covar = T,
                    n.obs = n.obs)
   if (verbose){
      gprint("\n")
      evaluate.fa.fit(res)
   }
   res
}

#" @export
evaluate.fa.fit <- function(res.fa){
   gprint("🔍 Exploratory factor analysis results:")
   propVar <- data.frame(res.fa$Vaccounted)["Proportion Var",]
   gprint("Proportion of variance accounted for: {100 * round(propVar, 3)}%")
   gprint("RMSEA: {round(res.fa$RMSEA[1], 3)} (< 0.05: good, 0.05 - 0.08: reasonable, > 0.10: poor)")
   gprint("Corrected RMSR: {round(res.fa$crms, 3)} (< 0.08: good)")
   gprint("CFI: {round(res.fa$CFI, 3)} (> 0.95: good)")
   gprint("TLI: {round(res.fa$TLI, 3)} (> 0.95: good)\n\n")
}

#" @export
mytheme <- function(){
   box::use(ggplot2[...])
   theme_bw() +
   theme(axis.title.x = element_text(size = 18, margin = margin(t = 10)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        # plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.border = element_rect(size = 2),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        strip.placement = "outside")
}

#" @export
cbPalette <- function(){
 c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF")
}
