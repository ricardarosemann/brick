#' Copy initial gdx to output folder
#'
#' @param path character vector with folders to write input data into
#' @param startingPoint character with path to the starting point or the respective directory
#' @param outputFolder directory of output folder
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @author Robin Hasse

copyInitialGdx <- function(path, startingPoint, outputFolder, overwrite = FALSE) {

  if (is.null(startingPoint)) {
    return(invisible(NULL))
  }

  # find file for starting point
  initialGdxFile <- findGdxFile(startingPoint, outputFolder)

  # copy file
  copyGdxFile(initialGdxFile, path, "start.gdx", overwrite)
}
