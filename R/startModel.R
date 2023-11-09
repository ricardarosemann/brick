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
                       outputFolder = NULL,
                       references = NULL) {

  # if no outputFolder is defined, use this or create it in current wd
  defaultOutputFolderName <- "output"

  cfg <- readConfig(config)
  title <- cfg[["title"]]

  # set (if needed create) outputFolder and set path to run
  if (is.null(path)) {
    if (is.null(outputFolder)) {
      if (dir.exists(defaultOutputFolderName)) {
        outputFolder <- defaultOutputFolderName
        message("No outputFolder defined but a folder called '",
                defaultOutputFolderName, "' is found and used:\n  ",
                file.path(getwd(), outputFolder))
      } else {
        outputFolder <- "."
        message("No outputFolder defined. Current working directory is used:\n  ",
                getwd())
      }
    } else if (!dir.exists(outputFolder)) {
      dir.create(outputFolder)
      message("outputFolder did not exist and is created:\n  ",
              file.path(getwd(), outputFolder))
    }

    stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
    path <- file.path(outputFolder, paste0(title, stamp))
  } else if (dir.exists(path)) {
    if (!is.null(outputFolder)) {
      message("outputFolder is ignored as specific path is defined.")
    }
  } else {
    stop("This path does not exist: ", path)
  }

  createRunFolder(path, cfg)

  copyGamsFiles(path)

  copyInitialGdx(path, cfg)

  copyHistoryGdx(path, cfg)

  createInputData(path, cfg)

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
