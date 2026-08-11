#' Find latest run
#'
#' Find the latest run of a given name in the output folder. If specified,
#' require files in the runs and point to them.
#'
#' @param run character, name of a run in the output folder
#' @param outputFolder character, path to output folder.
#' @param fileNames character, file names to consider when looking for files
#'   in run folders. If you allow multiple file names, they are tested in the
#'   given order and the first match is returned (start with highest priority).
#'   If left \code{NULL}, only the latest run folder is returned.
#'
#' @returns path to latest run folder or the file inside this folder
#'
#' @author Robin Hasse

findLatestRun <- function(run, outputFolder, fileNames = NULL) {
  # all runs with given name in output folder
  finds <- grep(
    pattern = paste0("^", run, "_", REGEX_STAMP, "$"),
    x = list.dirs(outputFolder, full.names = FALSE, recursive = FALSE),
    value = TRUE
  )
  finds <- normalizePath(file.path(outputFolder, finds))

  if (!is.null(fileNames)) {
    # keep only runs with accepted GDXs and point to GDX file
    finds <- unlist(lapply(finds, function(find) {
      gdxFiles <- file.path(find, fileNames)
      if (any(file.exists(gdxFiles))) {
        return(head(gdxFiles[which(file.exists(gdxFiles))], 1))
      }
      NULL
    }))
  }

  if (length(finds) > 0) {
    # return latest find
    return(tail(sort(finds), 1))
  }

  NULL
}
