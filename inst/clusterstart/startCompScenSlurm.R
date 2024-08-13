#' Script to call compareScenarios
#'
#' Extract paramters and run compare Scenarios
#'
#' This script reads in the arguments
#' passed via the command line and calls the compareScenarios function.
#'
#' @author Ricarda Rosemann
#'

# Only if this file is run directly via Rscript startScriptSlurm.R, but not if this file
# is sourced, actually run
if (sys.nframe() == 0L) {

  # Extract command line arguments
  argsCL <- commandArgs(trailingOnly = TRUE)

  # Extract the mifs and the outputFile from command line arguments
  mifScen <- stringr::str_split_1(argsCL[1], ",")
  scenName <- stringr::str_split_1(argsCL[2], ",")
  names(mifScen) <- scenName

  outputFolder <- argsCL[3]
  outputFile <- argsCL[4]

  piamPlotComparison::compareScenarios(
    projectLibrary = "reportbrick",
    mifScen = mifScen, # vector of paths to MIF
    mifHist = mifScen[1], # pass any valid MIF
    outputFile = outputFile,
    outputDir = outputFolder,
    mainReg = "DEU",
    yearsHist = NULL
  )
}
