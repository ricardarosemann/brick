#' Script to call the starting function of brick
#'
#' Load BRICK and start the model by calling startModel
#'
#' This function loads the package brick, reads in the arguments
#' passed via the command line and calls the startModel function.
#'
#' @author Ricarda Rosemann
#'

# Only if this file is run directly via Rscript startScriptSlurm.R, but not if this file
# is sourced, actually run
if (sys.nframe() == 0L) {

  # Extract command line arguments
  argsCL <- commandArgs(trailingOnly = TRUE)

  # Extract the path to the output folder and to the installation of brick used
  path <- argsCL[1]
  brickDir <- argsCL[2]

  # If we are in developing mode: load brick via devtools
  isDev <- as.logical(argsCL[3])
  if (isTRUE(isDev)) {
    devtools::load_all(brickDir)
    message(paste("This is a development run. Loading brick from local folder", brickDir))
  }

  brick::startModel(path)
}
