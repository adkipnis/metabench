#' @export
parse.args <- function(names, defaults) {
   args <- commandArgs(trailingOnly = T)
   opts <- list()
   for (i in 1:length(names)) {
      name <- names[i]
      if (i <= length(args)) {
         opts[[name]] <- args[i]
      } else {
         opts[[name]] <- defaults[i]
      }
   }
   invisible(list2env(opts, envir = globalenv()))
}

#' @export
mkdir <- function(relpath) {
   fullpath <- here::here(relpath)
   if (!dir.exists(fullpath)) {
      dir.create(fullpath)
   }
}


#' @export
gprint <- function(s) {
   eval(substitute(print(glue::glue(s))), parent.frame())
}


#' @export
gpath <- function(s) {
   eval(substitute(here::here(glue::glue(s))), parent.frame())
}


#' @export
df2data <- function(df) {
   df |>
      dplyr::mutate(correct = as.integer(correct)) |>
      tidyr::pivot_wider(names_from = item, values_from = correct) |>
      tibble::column_to_rownames(var = "source")
}

