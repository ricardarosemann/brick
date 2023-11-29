#' Find the full file names of files in BRICK
#'
#' This is meant to work with the installed package BRICK but also when loading
#' the package via \code{devtools::load_all("path/to/brick")}
#'
#' @param ... character vectors, specifying subdirectory and files within brick
#' @param mustWork logical. If TRUE, an error is given if there are no matching
#' @returns A character vector of positive length, containing the file paths
#'   that matched \code{...}, or the empty string, \code{""}, if none matched
#'   (unless \code{mustWork = TRUE}).
#'   files.
#'
#' @author Robin Hasse
#'
#' @importFrom pkgload is_dev_package
#' @export

brick.file <- function(..., mustWork = FALSE) {

  path <- system.file(..., package = "brick",
                      mustWork = mustWork && !is_dev_package("brick"))

  if (path == "" && is_dev_package("brick")) {
    path <- system.file("inst", ..., package = "brick", mustWork = mustWork)
  }

  return(path)
}
