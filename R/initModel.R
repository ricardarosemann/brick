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

  brickDir <- getwd()

  if (!sendToSlurm) {
    startModel(config, path, brickDir)
  } else {
    slurmScriptPath <- file.path("inst", "startScriptSlurm.R")
    file.copy(slurmScriptPath, path)

    on.exit(setwd(brickDir))
    setwd(path)

    if (tasks32) {
      slurmConfig <- paste("--qos=short --nodes=1 --tasks-per-node=32",
                          "--constraint=broadwell --time=01:00:00")
    } else {
      slurmConfig <- "--qos=priority --nodes=1 --tasks-per-node=16"
    }

    exitCode <- system(paste0("sbatch --job-name=",
                                title,
                                " --mail-type=END",
                                " --comment=BRICK",
                                " --wrap=\"Rscript startScriptSlurm.R\" ",
                                slurmConfig))
    Sys.sleep(1)

    if (exitCode > 0) {
      print("Executing startModel failed.")
    }
  }
}