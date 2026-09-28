#' Load Input data from mredgebuildings
#'
#' @author Robin Hasse
#'
#' @param config named list with run configuration
#' @returns directory of input folder
#'
#' @importFrom madrat toolGetMapping regionscode getConfig
#' @importFrom gms download_distribute

loadMadratData <- function(config) {

  # regionmapping --------------------------------------------------------------

  regionmapping <- .findRegionMapping(config[["regionmapping"]])
  missingRegions <- setdiff(config[["regions"]], regionmapping[["RegionCode"]])
  if (length(missingRegions) > 0) {
    stop("The regions in your config don't match the region mapping. ",
         "The following regions are not part of the mapping:\n  ",
         paste(missingRegions, collapse = c(", ")))
  }



  # Identify madrat tgz files --------------------------------------------------

  # find package directory
  inputDir <- brick.file("input", mustWork = FALSE)

  # where are the old files from?
  sourceFiles <- file.path(inputDir, "source_files.log")
  if (file.exists(sourceFiles)) {
    madratOld <- readLines(sourceFiles)[1:2]
  } else {
    madratOld <- "noData"
  }

  regionmapHash <- paste0(regionscode(regionmapping), "_")
  granularityHash <- .argsHash(list(granularity = config[["granularity"]]), TRUE)
  inputRevision <- config[["inputRevision"]]
  extraMapHash <- ifelse(is.null(config[["extramappings_historic"]]),
                         "",
                         paste0("-", regionscode(config[["extramappings_historic"]])))

  # where to get new files from
  madratNew <- c(
    paste0("rev", inputRevision, "_", regionmapHash, granularityHash, "brick.tgz"),
    paste0("rev", inputRevision, "_", regionmapHash, extraMapHash, "_", "validationbrick.tgz")
  )

  if (!setequal(madratNew, madratOld) || isTRUE(config[["forceDownload"]])) {
    message(
      if (isTRUE(config[["forceDownload"]])) {
        "You set 'forceDownload = TRUE'."
      } else {
        "Your input data are outdated or in a different regional resolution. "
      },
      "New input data are downloaded and distributed."
    )

    # delete all files but source_files.log
    unlink(grep("source_files.log", list.files(inputDir, full.names = TRUE),
                value = TRUE, invert = TRUE),
           force = TRUE)

    # directory to look for the madrat tgz file
    repositories <- list(NULL)
    names(repositories) <- getConfig("outputfolder")

    # load tgz file and unpack it in input folder
    download_distribute(madratNew, repositories, brick.file(""))

  } else {
    message("No input data downloaded and distributed. To enable that, ",
            "delete input/source_files.log or set forceDownload to TRUE.")
  }



  # return ---------------------------------------------------------------------

  return(invisible(inputDir))
}



#' Get hash for further arguments
#'
#' This is copied code from madrat to create the hash for additional arguments
#' to `madrat::retrieveData` besides the regionmapping. This way, we can
#' recreate the file name of the tgz file with the input data.
#'
#' Ideally, this should be an exported function of madrat rather than copied
#' code.
#'
#' @param formals named list with arguments
#' @param useLabels, logical, should be \code{TRUE} for now
#' @returns character with hash
#'
#' @author Robin Hasse

.argsHash <- function(formals, useLabels) {
  if (length(formals) > 0) {
    hashs <- digest::digest(formals, algo = getConfig("hash"))
    if (useLabels) {
      hashs <- toolCodeLabels(hashs)
    }
    argsHash <- paste0(hashs, "_")
  } else {
    argsHash <- NULL
  }
  return(argsHash)
}
