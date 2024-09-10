#' Create input data
#'
#' Create a complete set of input data for the gams optimisation.
#'
#' This function reads static input data according to the input data revision in
#' the config and creates all required sets and parameters for the gams
#' optimisation depending on the switches in the config.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @importFrom quitte calc_addVariable
#' @importFrom tidyr complete pivot_wider replace_na
#' @importFrom stats pweibull
#' @importFrom utils head
#' @importFrom dplyr %>% mutate select group_by filter ungroup arrange left_join
#'   .data rename lag all_of across inner_join everything cross_join
#' @importFrom gamstransfer Container

createInputData <- function(path,
                            config,
                            overwrite = FALSE) {

  if (length(path) != 1) {
    stop("'path' has to be of length 1 not ", length(path), ".")
  }

  # check file path
  inputFilePath <- file.path(path, "input.gdx")
  if (file.exists(inputFilePath)) {
    if (overwrite) {
      warning("Input file '", inputFilePath, "' overwritten.")
    } else {
      stop("Input file '", inputFilePath,
           "' cannot be created as it already exists.")
    }
  }

  inputDir <- loadMadratData(config)

  m <- Container$new()
  message("Start input data creation...")

  m <- createSets(m, config)
  message("  ... created sets...")

  m <- createParameters(m, config, inputDir)
  message("  ... created parameters...")

  m$write(inputFilePath, compress = TRUE)
  message("... done.")

  # browser()

  return(invisible(inputFilePath))
}
