#' Set the SLURM configuration
#'
#' Based on user input, construct the SLURM configuration string.
#'
#' Check if SLURM configuration is admissible.
#'
#' @author Ricarda Rosemann
#'
#' @param slurmQOS string, name of the desired QOS (Quality of Service)
#' @param tasks32 boolean, specify whether a node with 32 tasks should be requested
#' @returns string with SLURM configuration
#'
#' @export

setSlurmConfig <- function(slurmQOS, tasks32) {

  allowedQOS <- c("default", "priority", "standby", "short", "medium", "long")

  # Throw error if non-existing QOS is given
  if (!slurmQOS %in% allowedQOS) {
    stop(paste0("Invalid QOS given. Available cluster QOS are:\t",
                paste(allowedQOS, collapse = ", "), ".\n",
                "The default QOS is priority for 16 tasks and short for 32 tasks.\n"))
  }

  # set default QOS
  if (tasks32) {
    if (slurmQOS == "default") {
      slurmQOS <- "short"
    } else if (slurmQOS %in% c("standby", "priority")) {
      # Throw an error if a job is started with 32 tasks on the priority partition
      stop(paste("32 tasks are only available on the standard partition.",
                 "QOS must be one of short, medium or long."))
    }
    slurmConfig <- paste0("--qos=", slurmQOS, " --nodes=1 --tasks-per-node=32",
                          " --constraint=broadwell --time=01:00:00")
  } else {
    if (slurmQOS == "default") {
      slurmQOS <- "priority"
    }
    slurmConfig <- paste0("--qos=", slurmQOS, " --nodes=1 --tasks-per-node=16")
  }

  return(slurmConfig)
}