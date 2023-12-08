#' Start the model
#'
#' Run the model with given configuration.
#'
#' This function creates a run folder with necessary gams files if missing. It
#' then computes the input data and finally runs the optimisation.
#'
#' @author Robin Hasse
#'
#' @param config run configurations
#' @param path character vector with folders to run the model in
#' @param brickDir Directory of all Brick folders
#' @export
#'
startModel <- function(config,
                       path,
                       brickDir) {

  cfg <- readConfig(config, readDirect = TRUE)

  createInputData(path, cfg, brickDir)

  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    createMatchingData(path, cfg, references)
  } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
    aggregateMatching(path, cfg)
  }

  runGams(path,
          cfg[["gamsOptions"]],
          c(cfg[["switches"]], cfg[c("solverLP", "solverNLP", "solverQCP",
                                     "ignoreShell")]),
          gamsCall = cfg[["gamsCall"]])

  plotSummary(path, NULL, showHistStock = cfg[["switches"]][["RUNTYPE"]] %in% c("calibration", "matching") ||
                cfg[["title"]] == "iamc_base")

  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    plotRefDeviation(path)
  }
}
