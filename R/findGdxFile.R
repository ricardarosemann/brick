#' Find GDX file
#'
#' Find file path to the most explicitly referenced GDX file
#'
#' @param gdx character, either a file path, a run directory or the name of a
#'   run in the output folder
#' @param outputFolder character, path to output folder. This argument is
#'   required if only a run name is passed as \code{gdx}.
#' @param gdxNames character, file names to consider when looking for GDX files
#'   in run folders. If you allow multiple file names, they are tested in the
#'   given order and the first match is returned (start with highest priority).
#' @returns path to gdx file
#'
#' @author Robin Hasse

findGdxFile <- function(gdx, outputFolder = NULL, gdxNames = "output.gdx") {

  if (length(gdx) > 1) {
    stop("Don't give more than one gdx. You gave:\n",
         paste(gdx, collapse = "\n"))
  }

  if (file.exists(gdx) && !dir.exists(gdx)) {
    # file directly given as file path
    return(gdx)
  }
  if (dir.exists(gdx)) {
    # folder given, search for relevant files
    originGdxFiles <- file.path(gdx, gdxNames)
    # return first exisitng file
    if (any(file.exists(originGdxFiles))) {
      return(originGdxFiles[which(file.exists(originGdxFiles))][1])
    }
  }
  if (!is.null(outputFolder)) {
    # look for latest run in the output folder that has the given name
    latestGdx <- findLatestRun(gdx, outputFolder, gdxNames)
    if (!is.null(latestGdx)) {
      return(latestGdx)
    }
  }
  stop("Unable to find this GDX file: ", gdx)
}
