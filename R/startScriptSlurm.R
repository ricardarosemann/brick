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
  # NEVER move this out of the if-clause to avoid recursive loading of brick. Or move the file outside the R folder.
  library(devtools)
  load_all()

  # We extract the path to the correct output folder to run the model in from the call to the script
  args <- commandArgs()
  path <- args[grep("--args", args) + 1]
  config <- file.path(path, "config", "config.yaml")
  # This needs to be adapted to the case when we run from installation
  brickDir <- dirname(dirname(path))
  # Could also shift this to start model if adapted accordingly for the non-SLURM run

  brick::startModel(config = config, path = path, brickDir = brickDir)
}