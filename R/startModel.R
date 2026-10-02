#' Start the model
#'
#' Run the model with given configuration.
#'
#' This function creates a run folder with necessary gams files if missing. It
#' then computes the input data and finally runs the optimisation.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to run the model in
#' @param runReporting logical, whether to run the reporting, i.e. write the mif
#'
#' @importFrom utils read.csv2
#' @importFrom reportbrick showSankey plotMatchingComparison reportCalibration plotBRICKCalib
#' @export
#'
startModel <- function(path, runReporting = TRUE) {

  cfg <- readConfig(file.path(path, "config", CONFIG_COMPILED), readDirect = TRUE)

  restart <- .readInitArgs(path)$restart

  if (isFALSE(restart) || "createInput" %in% restart) {
    createInputData(path, cfg, overwrite = !isFALSE(restart))
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "matching" &&
        (isFALSE(restart) || "createMatching" %in% restart)) {
    createMatchingData(path, cfg, overwrite = !isFALSE(restart))
  }

  if (isFALSE(restart) || any(c("createMatching", "copyGams") %in% restart)) {
    if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
      insertMatchingCode(path)
    } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
      # ????
    }
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "matching" &&
        "reweightMatching" %in% restart) {
    reweightMatchingReferences(path)
  }


  if (cfg[["switches"]][["SOLVEPROBLEM"]] == "auto") {
    cfg[["switches"]][["SOLVEPROBLEM"]]  <- if (is.null(cfg[["startingPoint"]])) {
      "lpnlp"
    } else {
      "nlp"
    }
  }

  if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
    runCalibration(path,
                   parameters = cfg[["calibrationParameters"]],
                   tcalib = periodFromConfig(cfg, "tcalib"),
                   gamsOptions = cfg[["gamsOptions"]],
                   switches = c(cfg[["switches"]],
                                cfg[c("solverLP", "solverNLP", "solverQCP", "ignoreShell")]),
                   gamsCall = cfg[["gamsCall"]])
  } else {
    runGams(path,
            cfg[["gamsOptions"]],
            c(cfg[["switches"]], cfg[c("solverLP", "solverNLP", "solverQCP",
                                       "ignoreShell")]),
            gamsCall = cfg[["gamsCall"]])

    gamsSuccess <- checkGamsSuccess(path, cfg[["switches"]][["RUNTYPE"]])

    if (isTRUE(all(gamsSuccess$success))) {
      message("Gams was succesful for all subsets.")
    } else {
      stop("Gams failed for at least one subset. For details, see the model and solver summaries.")
    }
  }


  if (isTRUE(runReporting)) {
    plotSummary(path, NULL)

    # sankey plots
    try(showSankey(path, "hs", maxPeriodsInRow = 5))
    if (!isTRUE(cfg[["ignoreShell"]])) {
      try(showSankey(path, "bs", maxPeriodsInRow = 5))
    }

    if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
      plotRefDeviation(path)
      plotMatchingComparison(normalizePath(path))
      plotSummary(path, c("loc", "typ"))
    } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
      reportCalibration(file.path(path, "calibration_0.gdx"))
      plotBRICKCalib(path)
    }

    try(reportMif(path))

  }

  if (isFALSE(restart)) {
    .updateBundleSettings(path, cfg[["title"]])
    .initNextScens(path)
  }

}



#' Read model initialisation arguments
#'
#' Since its function call, \code{initModel} might have changed arguments,
#' namely the restart options.
#'
#' @param path character, path to run folder
#' @returns named list of arguments to \code{initModel}
.readInitArgs <- function(path) {
  yaml::read_yaml(file.path(path, "config", INIT_ARGS))
}



#' Initialise next scenarios
#'
#' Initialise the runs that start from a given run as historic run
#'
#' @param path character, path to the historic run folder

.initNextScens <- function(path) {
  pathNextConfigs <- file.path(path, "config", "nextRuns")
  if (dir.exists(pathNextConfigs)) {
    nextConfigs <- readNextConfigs(pathNextConfigs)
    nextConfigs <- .setSwitch(nextConfigs, historic = path)
    args <- .readInitArgs(path)
    args$config <- nextConfigs
    args$path <- NULL
    do.call(initModel, args)
  }
}



#' Update bundle settings
#'
#' Write run path into settings file
#'
#' @param path character, path to the historic run folder
#' @param title character, path to the historic run folder
.updateBundleSettings <- function(path, title) {
  settingsPath <- .readInitArgs(path)$.bundleSettings
  if (is.null(settingsPath)) {
    return(invisible(NULL))
  }
  if (!file.exists(settingsPath)) {
    warning("Can't find bundle settings file: ", settingsPath)
    return(invisible(NULL))
  }
  settings <- .readSettings(settingsPath)
  outputFolder <- .getSettingsPath(settings, "outputFolder")
  settings$run[[title]] <- if (.identicalPath(outputFolder, dirname(path))) {
    basename(path)
  } else {
    warning("This run is associated with the settings file ", settingsPath,
            "but is not located in the defined outputFolder: ", outputFolder)
    normalizePath(path)
  }
  if (!settings$newRunAlways) {
    settings$newRunNext[[title]] <- FALSE
  }
  write_yaml(settings, settingsPath)
  return(invisible(settingsPath))
}
