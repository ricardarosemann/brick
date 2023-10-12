#' Script to call the starting function of brick
#'
#' Load BRICK and start the model by calling startModel
#'
#' This function loads the package brick, reads in the arguments
#' passed via the command line and calls the startModel function.
#'
#' @author Ricarda Rosemann
#'

# Only if this file is run directly via Rscript startOutside.R, but not if this file
# is sourced, actually run
if (sys.nframe() == 0L) {
  # To be replaced by library(brick) or brick::startModel below
  library(devtools)
  load_all()

  # We assume here that our working directory is the correct output directory
  config <- file.path("config", "config.yaml")
  path <- getwd()
  brickDir <- file.path("..", "..", path)
  # Could also shift this to start model if adapted accordingly for the non-SLURM run

  brick::startModel(config = config, path = path, brickDir = brickDir)
}