copyGamsFiles <- function(path, overwrite = FALSE) {

  gamsFiles <- file.path("inst", "gams", ".")
  file.copy(gamsFiles, path, recursive = TRUE, overwrite = overwrite)

}
