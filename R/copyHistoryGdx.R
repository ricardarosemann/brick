#' Copy history gdx to output folder
#'
#' @param path character vector with folders to write input data into
#' @param outputFolder directory of output folder
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#' @param thistOnly logical, crop temporal parameters to historic periods
#'
#' @author Robin Hasse
#'
#' @importFrom gamstransfer Container

copyHistoryGdx <- function(path,
                           outputFolder = NULL,
                           config,
                           overwrite = FALSE,
                           thistOnly = TRUE) {

  # find file
  originGdx <- config[["historic"]]
  if (is.null(originGdx)) {
    return(invisible(NULL))
  }
  if (length(originGdx) > 1) {
    stop("Don't give more than one historical. You gave:\n",
         paste(originGdx, collapse = "\n"))
  }
  originGdxFile <- .findOriginGdxFile(originGdx, outputFolder)

  # copy file
  targetGdxFile <- file.path(path, "history.gdx")
  if (!is.null(originGdxFile)) {
    file.copy(originGdxFile, targetGdxFile,
              overwrite = overwrite)
    message("Using ", originGdxFile, " as history.gdx")
  }

  # crop all temporal parameters to thist
  if (isTRUE(thistOnly) && !is.null(targetGdxFile)) {
    thist <- periodFromConfig(config, "thist")
    .cropParamsToThist(targetGdxFile, thist)
    message("Cropped all temporal parameters in ", targetGdxFile,
            " to historic time steps: ", paste(thist, collapse = ", "))
  }

  return(invisible(targetGdxFile))

}





#' find origin gdx file
#'
#' Depending on what is passed via \code{originGDX}, this function returns the
#' file path if it exists, looks for recognised file names in the given
#' directory if it exists or looks for the latest run with the given name in the
#' \code{outputFolder}.
#'
#' @param originGdx character, file path to run or gdx file used as historical
#'   or scenario name
#' @param outputFolder directory of output folder, only required if
#'   \code{originGDX} is a scenario name
#' @returns file path to origin gdx file
#'
#' @importFrom utils head tail

.findOriginGdxFile <- function(originGdx, outputFolder) {
  # recognised file names for initial gdx (in order of priority)
  gdxNames <- c("output.gdx")

  if (file.exists(originGdx) && !dir.exists(originGdx)) {
    # origin file directly given as file path
    return(originGdx)
  }
  if (dir.exists(originGdx)) {
    # folder given, search for relevant files
    originGdxFiles <- file.path(originGdx, gdxNames)
    if (any(file.exists(originGdxFiles))) {
      return(head(originGdxFiles[which(file.exists(originGdxFiles))], 1))
    }
  }
  if (!is.null(outputFolder)) {
    # look for latest run in the output folder that hase the given name
    previousRuns <- grep(
      pattern = paste0("^", originGdx, "_\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}$"),
      x = list.dirs(outputFolder, full.names = FALSE, recursive = FALSE),
      value = TRUE
    )
    previousRuns <- normalizePath(file.path(outputFolder, previousRuns))
    previousRuns <- unlist(lapply(previousRuns, function(run) {
      gdxFiles <- file.path(run, gdxNames)
      if (any(file.exists(gdxFiles))) {
        return(head(gdxFiles[which(file.exists(gdxFiles))], 1))
      }
      return(NULL)
    }))
    if (length(previousRuns) > 0) {
      return(tail(sort(previousRuns), 1))
    }
  }
  stop("Unable to find a file for the historical with given config ",
       "parameter: ", originGdx)
}





#' crop gdx parameters to historic periods
#'
#' remove all records of temporal parameters that are outside of \code{thist}
#' and make zero values explicit by filling with EPS.
#'
#' @param gdx character, file path to gdx
#' @param thist numeric vector of historic periods
#'
.cropParamsToThist <- function(gdx, thist) {
  m <- Container$new(gdx)

  missingPeriods <- setdiff(thist, readSymbol(m, "ttot")[[1]])
  if (length(missingPeriods) > 0) {
    stop(gdx, " is missing historic periods: ",
         paste(missingPeriods, collapse = ", "))
  }

  for (paramName in m$listParameters()) {
    param <- m$getSymbols(paramName)[[1]]
    if (!"ttot" %in% param$domainLabels) {
      next
    }
    param$records <- param$records %>%
      filter(.data[["ttot"]] %in% thist) %>%
      .explicitZero()
  }
  m$write(gdx, compress = TRUE)

  return(invisible(gdx))
}
