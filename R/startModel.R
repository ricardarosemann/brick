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
#'
#' @importFrom utils read.csv2
#' @export
#'
startModel <- function(path) {

  cfg <- readConfig(file.path(path, "config", "config.yaml"), readDirect = TRUE)

  if (file.exists(file.path(path, "config", "restartOptions.csv"))) {
    restart <- read.csv2(file.path(path, "config", "restartOptions.csv"))[["restart"]]
  } else {
    restart <- NULL
  }

  if (is.null(restart) || "crInp" %in% restart) {
    createInputData(path, cfg, overwrite = !is.null(restart))
  }

  if (is.null(restart) || "crMatch" %in% restart) {
    if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
      createMatchingData(path, cfg, overwrite = !is.null(restart))
    } else if (cfg[["switches"]][["RUNTYPE"]] == "calibration") {
      aggregateMatching(path, cfg, overwrite = !is.null(restart))
    }
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
