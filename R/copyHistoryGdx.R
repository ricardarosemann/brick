#' Copy history gdx to output folder
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @author Robin Hasse

copyHistoryGdx <- function(path, config, overwrite = FALSE) {

  # recognised file names for inital gdx (in order of priority)
  historyGdxNames <- c("output.gdx")

  # find file for starting point
  historyGdx <- config[["historic"]]
  if (length(historyGdx) > 1) {
    stop("Don't give more than one historical. You gave:\n",
         paste(historyGdx, collapse = "\n"))
  }
  historyGdxFiles <- file.path(historyGdx, historyGdxNames)
  historyGdxFile <- if (!is.null(historyGdx)) {
    if (file.exists(historyGdx) && !dir.exists(historyGdx)) {
      historyGdx
    } else if (dir.exists(historyGdx)) {
      if (any(file.exists(historyGdxFiles))) {
        head(historyGdxFiles[which(file.exists(historyGdxFiles))], 1)
      } else {
        stop("There is no file for the historical in the given directory: ",
             historyGdx)
      }
    } else {
      stop("Unable to find a file for the historical with given config ",
           "parameter: ", historyGdx)
    }
  } else {
    NULL
  }

  # copy file
  if (!is.null(historyGdxFile)) {
    file.copy(historyGdxFile, file.path(path, "history.gdx"),
              overwrite = overwrite)
    message("Using ", historyGdxFile, " as history.gdx")
  }

}
