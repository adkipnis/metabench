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
df2data <- function(df) {
   df |>
      dplyr::mutate(correct = as.integer(correct)) |>
      tidyr::pivot_wider(names_from = item, values_from = correct) |>
      tibble::column_to_rownames(var = "source")
}

#" @export
run.mirt <- function(data, itemtype, large=F){
  mirt::mirt(data, 1, itemtype=itemtype,
               method="EM",
               density="Davidian-4",
               large=large,
               TOL=1e-4,
               technical=list(NCYCLES=3000))
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
