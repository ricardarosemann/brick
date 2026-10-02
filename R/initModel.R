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
#' @param restart logical or character vector of elements to be restarted.
#'   If FALSE (default), then no restart is initiated.
#'   If TRUE, then the run in the given path or the latest run is restarted with default settings.
#'   Allowed elements of the character vector are:
#'   \itemize{
#'   \item \code{"copyGams"} to recopy the Gams scripts (necessary if changes were
#'         made in Gams code)
#'   \item \code{"createInput"} to recreate the input data
#'   \item \code{"createMatching"} to recreate the matching data
#'   \item \code{"reweightMatching"} to update the matching reference selection and weighting
#'   \item \code{"useAsStart"} to use the run from which we restart as the starting point
#'   \item \code{"none"} (or any other string) to do none of the above
#'  }
#' @param runReporting logical, whether to run the reporting, i.e. write the mif
#' @param sendToSlurm boolean whether or not the run should be started via SLURM
#' @param slurmQOS character, slurm QOS to be used
#' @param tasksPerNode numeric, number of tasks per node to be requested
#' @param timeLimit character, time limit of the slurm job given in the format hh:mm:ss
#' @param tasks32 boolean whether or not the SLURM run should be with 32 tasks
#' @param .bundleSettings character, path to bundle settings file. Internal
#'   argument. Do not set manually!
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
                      restart = FALSE,
                      runReporting = TRUE,
                      sendToSlurm = NULL,
                      slurmQOS = NULL,
                      tasksPerNode = NULL,
                      timeLimit = NULL,
                      tasks32 = FALSE,
                      .bundleSettings = NULL) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  # Multiple scenarios ---------------------------------------------------------

  # checks each item for isFALSE
  .isFALSE <- function(x) {
    vapply(x, function(item) isFALSE(item), logical(1))
  }
  .isTRUE <- function(x) {
    vapply(x, function(item) isTRUE(item), logical(1))
  }

  if (!.isConfig(config) &&
        (length(path) > 1 || is.list(path) || length(config) > 1 || is.list(config))) {

    return(invisible(
      if (any(.isTRUE(restart))) {
        ## independent restarts ====
        Map(function(.path, .restart) {
          initModel(path = .path,
                    configFolder = configFolder,
                    outputFolder = outputFolder,
                    references = references,
                    restart = .restart,
                    runReporting = runReporting,
                    sendToSlurm = sendToSlurm,
                    slurmQOS = slurmQOS,
                    tasksPerNode = tasksPerNode,
                    timeLimit = timeLimit,
                    tasks32 = tasks32,
                    .bundleSettings = .bundleSettings)
        },
        .path = path,
        .restart = restart
        )
      } else if (all(.isFALSE(restart))) {
        ## model cascade ====
        cascade <- if (.isCascade(config)) {
          config
        } else {
          makeCascade(config, configFolder)
        }
        Map(function(.config) {
          initModel(config = .config,
                    path = path,
                    configFolder = configFolder,
                    outputFolder = outputFolder,
                    references = references,
                    runReporting = runReporting,
                    sendToSlurm = sendToSlurm,
                    slurmQOS = slurmQOS,
                    tasksPerNode = tasksPerNode,
                    timeLimit = timeLimit,
                    tasks32 = tasks32,
                    .bundleSettings = .bundleSettings)
        },
        .config = cascade
        )
      } else {
        stop("Don't initialise restarts and new runs at the same time.")
      }
    ))
  }


  # Determine whether to send to SLURM -----------------------------------------

  if (isTRUE(sendToSlurm)) {
    if (isSlurmAvailable()) {
      message("Run will be sent to SLURM")
    } else {
      stop("sendToSlurm is TRUE, but SLURM is not available.")
    }
  } else if (isFALSE(sendToSlurm)) {
    message("Run will be executed directly.")
  } else {
    # Check if SLURM is available. Start via SLURM if available, and directly otherwise.
    if (isSlurmAvailable()) {
      message("SLURM is available. Run will be sent to SLURM.")
      sendToSlurm <- TRUE
    } else {
      message("SLURM is not available. Run will be executed directly.")
      sendToSlurm <- FALSE
    }
  }



  # Check if this is a restart run and determine the path to be restarted ------

  if (isTRUE(restart) || is.character(restart)) {


    ## Restart run: Prepare the config and restart settings ====

    if (!is.null(path) && file.exists(path)) {
      message("Restarting on given path: ", path)
    } else if (is.null(path)) {
      path <- findLatestRun(outputFolder)
      message("Restart: No path given or given path does not exist. Restarting on the latest run: ", path)
    } else {
      stop("Restart: You passed a non-existing path in a restart run.")
    }
    if (isTRUE(restart)) {
      message("No restart options were specified. ",
              "Default options are applied: Copy Gams files, recreate input data, ",
              "recreate and reweight matching if applicable, and use output gdx as starting point if existent.")
      restart <- c("copyGams", "createInput", "createMatching", "reweightMatching", "useAsStart")
    }

    if (!is.null(config)) {
      warning("You passed a config in a restart run. ",
              "This config will be ignored and the existing config in 'config/", CONFIG_COMPILED, "' will be used.")
    }

    cfg <- readConfig(config = file.path(path, "config", CONFIG_COMPILED),
                      configFolder = configFolder,
                      readDirect = TRUE)
    title <- cfg[["title"]]
  } else {


    ## New run: Read the config and prepare the run folder ====

    restart <- FALSE
    if (!is.null(path) && file.exists(path)) {
      stop("You passed an existing path, but did not set this as a restart run.")
    }

    cfg <- if (.isConfig(config)) {
      config
    } else {
      readConfig(config = config,
                 configFolder = configFolder)
    }

    title <- paste(cfg[["title"]], sep = "-")

    if (is.null(path)) {
      path <- file.path(outputFolder, .addTimeStamp(title))
    }

    createRunFolder(path, cfg)
  }



  # Save function arguments ----------------------------------------------------

  args <- as.list(environment(), all.names = TRUE)[names(formals(initModel))]
  yaml::write_yaml(args, file.path(path, "config", INIT_ARGS))



  # Prepare the run ------------------------------------------------------------

  # Generate SLURM configuration if sending to SLURM
  if (isTRUE(sendToSlurm)) {
    if (is.null(slurmQOS) && !is.null(cfg[["slurmQOS"]])) slurmQOS <- cfg[["slurmQOS"]]
    if (is.null(tasksPerNode) && !is.null(cfg[["tasksPerNode"]])) tasksPerNode <- cfg[["tasksPerNode"]]
    if (isFALSE(tasks32) && isTRUE(cfg[["tasks32"]])) {
      tasks32 <- cfg[["tasks32"]]
      warning("Using 32 tasks as defined in the config file.")
    }
    slurmConfig <- setSlurmConfig(slurmQOS = slurmQOS, tasksPerNode = tasksPerNode, tasks32 = tasks32,
                                  timeLimit = timeLimit)
  }

  # Copy gams files if this is not a restart run or if this is specified in restart parameters
  if (isFALSE(restart) || "copyGams" %in% restart) {
    copyGamsFiles(path, overwrite = !isFALSE(restart))
  }

  # Copy the initial gdx. In restart runs, this can be the output gdx of the restarted folder
  if ("useAsStart" %in% restart && file.exists(file.path(path, "output.gdx"))) {
    cfg[["startingPoint"]] <- file.path(path, "output.gdx")
    message("Using the output-gdx of the restarted run as starting point.")
  }
  copyInitialGdx(path, cfg[["startingPoint"]], outputFolder)

  copyHistoryGdx(path, outputFolder, cfg)

  # In matching run: Save references to csv
  if (cfg[["switches"]][["RUNTYPE"]] == "matching") {
    write.csv2(data.frame(references), file.path(path, "references.csv"))
  }



  # Start the run --------------------------------------------------------------

  if (isFALSE(sendToSlurm)) {
    startModel(path, runReporting = runReporting)
  } else {
    brickDir <- find.package("brick")

    isDev <- as.character(is_dev_package("brick"))
    slurmScriptPath <- brick.file("clusterstart", "startScriptSlurm.R")
    logFilePath <- file.path(path, "log.txt")
    loadMredgebuildings <- as.character(cfg[["switches"]][["RUNTYPE"]] == "matching")

    exitCode <- system(paste0("sbatch --job-name=",
                              title,
                              " --output=", logFilePath,
                              " --mail-type=END,FAIL",
                              " --comment=BRICK",
                              " --wrap=\"",
                              paste("Rscript", slurmScriptPath, path, brickDir, isDev,
                                    loadMredgebuildings, runReporting),
                              "\" ",
                              slurmConfig))
    Sys.sleep(1)

    if (exitCode > 0) {
      message("Executing initModel failed with exit code ", exitCode, ".")
    }

  }

  invisible(path)
}
