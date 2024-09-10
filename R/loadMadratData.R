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
    madratOld <- readLines(sourceFiles)[1]
  } else {
    madratOld <- "noData"
  }

  # where to get new files from
  madratNew <- paste0("rev",
                      config[["inputRevision"]], "_",
                      regionscode(regionmapping), "_",
                      "brick.tgz")

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
    file.remove(grep("source_files.log", list.files(inputDir),
                     value = TRUE, invert = TRUE))

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



.findRegionMapping <- function(regionmapping) {

  if (is.null(regionmapping)) {
    # missing region mapping -> national resolution
    name <- "regionmappingNational.csv"
    where <- "mredgebuildings"
  } else if (is.character(regionmapping)) {
    name <- regionmapping[1]
    if (length(regionmapping) == 1) {
      if (!file.exists(name)) {
        stop("Can't find regionmapping. This file doesn't exist: ", name)
      }
      where <- "local"
    } else if (length(regionmapping) == 2) {
      where <- regionmapping[2]
    } else {
      stop("'regionmapping' in your config cannot have more than two elements.",
           " Either give the directory to a local mapping file or a vector of ",
           "the file name and the 'where' argument of toolGetMapping.")
    }
  } else {
    stop("'regionmapping' in your config can either be NULL, a filepath or a ",
         "character vector of length 2, not ", class(regionmapping), ".")
  }

  toolGetMapping(name, "regional", where)
}
