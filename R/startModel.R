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
#' @param outputFolder directory of output folder
#' @param references named list of matching references
#' @export
#'
startModel <- function(config = NULL,
                       path = NULL,
                       outputFolder = "output",
                       references = NULL) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  cfg <- readConfig(config)
  title <- cfg[["title"]]

  if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
    title <- paste0(title, cfg[["parameters"]][["iteration"]], "Iter",
                    cfg[["parameters"]][["alpha"]], "A")
  }

  if (is.null(path)) {
    stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
    path <- file.path(outputFolder, paste0(title, stamp))
  }

  createRunFolder(path, cfg)

  copyGamsFiles(path)

  copyInitialGdx(path, cfg)

  copyHistoryGdx(path, cfg)

  createInputData(path, cfg)

  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    createMatchingData(path, cfg, references)
  # } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
  #   aggregateMatching(path, cfg)
  }

  runGams(path,
          cfg[["gamsOptions"]],
          c(cfg[["switches"]], cfg[c("solverLP", "solverNLP", "solverQCP")]),
          gamsCall = cfg[["gamsCall"]])

  plotSummary(path, NULL, showHistStock = cfg[["switches"]][["RUNTYPE"]] %in% c("calibration", "matching") ||
                cfg[["title"]] == "iamc_base", compareGdx = cfg[["compareGdx"]])

  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    plotRefDeviation(path)
  }

  return(path)
}
