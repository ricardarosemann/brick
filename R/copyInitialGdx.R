#' Copy initial gdx to output folder
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @author Robin Hasse

copyInitialGdx <- function(path, config, overwrite = FALSE) {
  # recognised file names for inital gdx (in order of priority)
  initialGdxNames <- c("output.gdx")

  # find file for starting point
  initialGdx <- config[["startingPoint"]]
  if (length(initialGdx) > 1) {
    stop("Don't give more than one starting point. You gave:\n",
         paste(initialGdx, collapse = "\n"))
  }
  initialGdxFiles <- file.path(initialGdx, initialGdxNames)
  initialGdxFile <- if (!is.null(initialGdx)) {
    if (file.exists(initialGdx) && !dir.exists(initialGdx)) {
      initialGdx
    } else if (dir.exists(initialGdx)) {
      if (any(file.exists(initialGdxFiles))) {
        head(initialGdxFiles[which(file.exists(initialGdxFiles))], 1)
      } else {
        stop("There is no file for the starting point in the given directory: ",
             initialGdx)
      }
    } else {
      stop("Unable to find a file for the starting point with given config ",
           "parameter: ", initialGdx)
    }
  } else {
    NULL
  }

  # copy file
  if (!is.null(initialGdxFile)) {
    file.copy(initialGdxFile, file.path(path, "start.gdx"),
              overwrite = overwrite)
    message("Using ", initialGdxFile, " as start.gdx")
  }

}
