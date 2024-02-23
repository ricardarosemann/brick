#' Initialize the model:
#'
#' Preparations of a model run, send the model to SLURM if desired
#'
#' This function creates the run folder with the necessary config and gams files.
#' It then either calls the function to start the model directly or passes the model to SLURM.
#'
#' @author Ricarda Rosemann
#'
#' @param config run configurations
#' @param path character vector with folders to run the model in
#' @param outputFolder directory of output folder
#' @param references named character vector of matching references
#' @param restart character vector of elements to be restarted.
#' Allowed elements are:
#'  "cpGms" to recopy the Gams scripts (necessary if changes were made in Gams code)
#'  "crInp" to recreate input data,
#'  "crMatch" to either recreate the matching data or reaggregate the matching
#'  "none" (or any other string) to do none of the above
#' @param sendToSlurm boolean whether or not the run should be started via SLURM
#' @param slurmQOS character, slurm QOS to be used
#' @param tasks32 boolean whether or not the SLURM run should be with 32 tasks
#' @importFrom pkgload is_dev_package
#' @importFrom utils write.csv2
#' @export
initModel <- function(config = NULL,
                      path = NULL,
                      outputFolder = "output",
                      references = NULL,
                      restart = NULL,
                      sendToSlurm = TRUE,
                      slurmQOS = "default",
                      tasks32 = FALSE) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  # Generate SLURM configuration if sending to SLURM
  if (sendToSlurm) {
    slurmConfig <- setSlurmConfig(slurmQOS = slurmQOS, tasks32 = tasks32)
  }

  # Check if an already existing path was given
  if (!is.null(path) && file.exists(path)) {
    message("Given path already exists. Restarting on this path.")
    if (is.null(restart)) {
      message("No restart options were specified. ",
              "Default options are applied: Recreating input data and recreate/reaggregate matching.")
      restart <- c("crInp", "crMatch")
    }
    write.csv2(data.frame(restart = restart), file.path(path, "config", "restartOptions.csv"))

    if (!is.null(config)) {
      warning("You passed a config in a restart run. ",
              "This config will be ignored and the existing config in 'config/config.yaml' will be used.")
    }

    cfg <- readConfig(file.path(path, "config", "config.yaml"))
    title <- cfg[["title"]]
  } else {
    if (!is.null(restart)) {
      message("Restart options were given, but no existing path was specified. Starting a new run.")
      restart <- NULL
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
  }

  # Copy gams files if this is not a restart run or if this is specified in restart parameters
  if (is.null(restart) || "cpGms" %in% restart) {
    copyGamsFiles(path, overwrite = !is.null(restart))
  }

  copyInitialGdx(path, cfg)

  copyHistoryGdx(path, cfg)

  # In matching run: Save references to csv
  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    write.csv2(data.frame(references), file.path(path, "references.csv"))
  }

  if (isFALSE(sendToSlurm)) {
    startModel(path)
  } else {
    brickDir <- find.package("brick")

    isDev <- as.character(is_dev_package("brick"))
    slurmScriptPath <- brick.file("clusterstart", "startScriptSlurm.R")
    logFilePath <- file.path(path, "log.txt")

    exitCode <- system(paste0("sbatch --job-name=",
                              title,
                              " --output=", logFilePath,
                              " --mail-type=END",
                              " --comment=BRICK",
                              " --wrap=\"",
                              paste("Rscript", slurmScriptPath, path, brickDir, isDev),
                              "\" ",
                              slurmConfig))
    Sys.sleep(1)

    if (exitCode > 0) {
      message("Executing initModel failed with exit code ", exitCode, ".")
    }
  }
}
