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


