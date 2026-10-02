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
  originGdxFile <- findGdxFile(originGdx, outputFolder)

  # copy file
  targetGdxFile <- copyGdxFile(originGdxFile, path, "history.gdx", overwrite)

  # crop all temporal parameters to thist
  if (isTRUE(thistOnly) && !is.null(targetGdxFile)) {
    thist <- periodFromConfig(config, "thist")
    .cropParamsToThist(targetGdxFile, thist)
    message("Cropped all temporal parameters in ", targetGdxFile,
            " to historic time steps: ", paste(thist, collapse = ", "))
  }

  return(invisible(targetGdxFile))

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
