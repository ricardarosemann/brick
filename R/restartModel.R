#' Start the model
#'
#' Run the model with given configuration.
#'
#' This function creates a run folder with necessary gams files if missing. It
#' then computes the input data and finally runs the optimisation.
#'
#' @param path character vector with folders to run the model in
#' @param outputFolder directory of output folder
#' @param copyScripts boolean, copy gams scripts from source
#' @param copyStartPoint boolean, copy initial gdx
#' @param recreateInputData boolean, recreate input data
#' @param copyHistory boolean, copy history gdx
#' @param recreateMatchingData boolean, recreate matching data
#' @param reaggregateMatching boolean, reaggregate matching results
#' @param references named list, not implemented yet
#'
#' @author Robin Hasse
#'
#' @importFrom utils tail
#' @export
#'
restartModel <- function(path = NULL,
                         outputFolder = "output",
                         copyScripts = FALSE,
                         copyStartPoint = FALSE,
                         recreateInputData = FALSE,
                         copyHistory = FALSE,
                         recreateMatchingData = FALSE,
                         reaggregateMatching = FALSE,
                         references = NULL) {

  if (is.null(path)) {
    if (dir.exists(outputFolder)) {
      paths <- list.dirs(outputFolder, recursive = FALSE)
      regexStamp <- "\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}"
      stamp <- sub(paste0("^.*_(", regexStamp, ")$"), "\\1", paths)
      isStamp <- grepl(paste0("^", regexStamp, "$"), stamp)
      path <- if (any(isStamp)) {
        paths <- paths[isStamp]
        stamp <- stamp[isStamp]
        path <- tail(paths[order(stamp)], 1)
      } else if (length(paths) == 1) {
        paths
      } else {
        stop("Cannot identify the most recent run. Please provide a path")
      }

    } else {
      stop("Cannot find last run as this outputFolder does not exist: ",
           outputFolder)
    }

    message("Restarting most recent run: ", path)
  } else {
    if (!dir.exists(path)) {
      stop("There is no folder with the given path: ", path)
    }
  }

  # config file
  cfgPath <- file.path(path, "config", "config.yaml")
  if (file.exists(cfgPath)) {
    cfg <- readConfig(cfgPath)
  } else {
    stop("Missing config file: ", cfgPath)
  }

  if (copyStartPoint) {
    copyInitialGdx(path, cfg, TRUE)
  }

  if (copyHistory) {
    copyHistoryGdx(path, cfg, TRUE)
  }

  if (copyScripts) {
    copyGamsFiles(path, TRUE)
  }

  if (recreateInputData) {
    createInputData(path, cfg, overwrite = TRUE)
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "matching" && recreateMatchingData) {
    createMatchingData(path, cfg, references, overwrite = TRUE)
  } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration" && reaggregateMatching) {
    aggregateMatching(path, cfg, overwrite = TRUE)
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
