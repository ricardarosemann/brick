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
#' @param configFolder character, directory to search for configs. If NULL, the
#'   BRICK-internal config folder is used.
#' @param outputFolder directory of output folder
#' @param references named character vector of matching references
#' @param restart character vector of elements to be restarted.
#'   Allowed elements are:
#'   \itemize{
#'   \item \code{"cpGms"} to recopy the Gams scripts (necessary if changes were
#'         made in Gams code)
#'   \item \code{"crInp"} to recreate input data,
#'   \item \code{"crMatch"} to either recreate the matching data or reaggregate
#'         the matching
#'   \item \code{"none"} (or any other string) to do none of the above
##'  }
#' @param sendToSlurm boolean whether or not the run should be started via SLURM
#' @param slurmQOS character, slurm QOS to be used
#' @param tasksPerNode numeric, number of tasks per node to be requested
#' @param tasks32 boolean whether or not the SLURM run should be with 32 tasks
#' @returns path (invisible)
#'
#' @importFrom pkgload is_dev_package
#' @importFrom utils write.csv2
#' @export
initModel <- function(config = NULL,
                      path = NULL,
                      configFolder = NULL,
                      outputFolder = "output",
                      references = NULL,
                      restart = NULL,
                      sendToSlurm = NULL,
                      slurmQOS = NULL,
                      tasksPerNode = NULL,
                      timeLimit = NULL,
                      tasks32 = FALSE) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  # Check if SLURM is available. Start via SLURM if available, and directly otherwise.
  if (is.null(sendToSlurm)) {
    if (isSlurmAvailable()) {
      message("SLURM is available. Run will be sent to SLURM.")
      sendToSlurm <- TRUE
    } else {
      message("SLURM is not available. Run will be executed directly.")
      sendToSlurm <- FALSE
    }
  } else if (isTRUE(sendToSlurm) && !isSlurmAvailable()) {
    stop("sendToSlurm is TRUE, but SLURM is not available. Stopping.")
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

    cfg <- readConfig(config = file.path(path, "config", "config.yaml"),
                      configFolder = configFolder,
                      readDirect = TRUE)
    title <- cfg[["title"]]
  } else {
    if (!is.null(restart)) {
      message("Restart options were given, but no existing path was specified. Starting a new run.")
      restart <- NULL
    }

    cfg <- readConfig(config = config,
                      configFolder = configFolder)
    title <- cfg[["title"]]

    if (is.null(path)) {
      stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
      path <- file.path(outputFolder, paste0(title, stamp))
    }

    createRunFolder(path, cfg)
  }

  # Generate SLURM configuration if sending to SLURM
  if (sendToSlurm) {
    if (is.null(slurmQOS) && !is.null(cfg[["slurmQOS"]])) slurmQOS <- cfg[["slurmQOS"]]
    if (is.null(tasksPerNode) && !is.null(cfg[["tasksPerNode"]])) tasksPerNode <- cfg[["tasksPerNode"]]
    if (isFALSE(tasks32) && isTRUE(cfg[["tasks32"]])) {
      tasks32 <- cfg[["tasks32"]]
      warning("Using 32 tasks as defined in the config file.")
    }
    slurmConfig <- setSlurmConfig(slurmQOS = slurmQOS, tasksPerNode = tasksPerNode,
                                  tasks32 = tasks32, timeLimit = timeLimit)
  }

  # Copy gams files if this is not a restart run or if this is specified in restart parameters
  if (is.null(restart) || "cpGms" %in% restart) {
    copyGamsFiles(path, overwrite = !is.null(restart))
  }

  copyInitialGdx(path, cfg)

  copyHistoryGdx(path, outputFolder, cfg)

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

  invisible(path)
}
