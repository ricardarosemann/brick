#' Find the full file names of files in BRICK
#'
#' This is meant to work with the installed package BRICK but also when loading
#' the package via \code{devtools::load_all("path/to/brick")}
#'
#' @param ... character vectors, specifying subdirectory and files within brick
#' @param mustWork if TRUE, an error is given if there are no matching files
#' @returns A character vector of positive length, containing the file paths
#'   that matched \code{...} in BRICK.
#'
#' @author Robin Hasse
#'
#' @importFrom piamutils getSystemFile
#' @export

brick.file <- function(..., mustWork = TRUE) {
  getSystemFile(..., package = "brick", mustWork = mustWork)
}
