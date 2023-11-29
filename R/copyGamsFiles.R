#' Copy gams scripts to output folder
#'
#' @param path character vector with folders to write input data into
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @author Robin Hasse

copyGamsFiles <- function(path, overwrite = FALSE) {

  gamsFiles <- file.path(brick.file("gams"), ".")
  message("Copy gams files from ", gamsFiles)
  file.copy(gamsFiles, path, recursive = TRUE, overwrite = overwrite)

}
