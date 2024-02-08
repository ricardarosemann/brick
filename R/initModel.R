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
#' @param references named list of matching references
#' @param sendToSlurm boolean whether or not the run should be started via SLURM
#' @param tasks32 boolean whether or not the SLURM run should be with 32 tasks
#' @export
initModel <- function(config = NULL,
                      path = NULL,
                      outputFolder = "output",
                      references = NULL,
                      sendToSlurm = TRUE,
                      slurmQOS = "default",
                      tasks32 = FALSE) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  cfg <- readConfig(config)
  title <- cfg[["title"]]

  if (is.null(path)) {
    stamp <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
    path <- file.path(outputFolder, paste0(title, stamp))
  }

  createRunFolder(path, cfg)

  copyGamsFiles(path)

  copyInitialGdx(path, cfg)

  copyHistoryGdx(path, cfg)

  # TODO: This needs to be adapted to the case when Brick is called from the installation
  brickDir <- getwd()

  if (!sendToSlurm) {
    config <- file.path(path, "config", "config.yaml")
    startModel(config, path, brickDir)
  } else {
    slurmScriptPath <- file.path("R", "startScriptSlurm.R")
    file.copy(slurmScriptPath, path)

    logFilePath <- file.path(path, "log.txt")
    slurmScriptRun <- file.path(path, "startScriptSlurm.R")
    slurmConfig <- setSlurmConfig(slurmQOS = slurmQOS, tasks32 = tasks32)

    exitCode <- system(paste0("sbatch --job-name=",
                              title,
                              " --output=", logFilePath,
                              " --mail-type=END",
                              " --comment=BRICK",
                              " --wrap=\"",
                              paste("Rscript", slurmScriptRun, path),
                              "\" ",
                              slurmConfig))
    Sys.sleep(1)

    if (exitCode > 0) {
      print("Executing startModel failed.")
    }
  }
}