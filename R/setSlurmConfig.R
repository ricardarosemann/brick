#' Set the SLURM configuration
#'
#' Based on user input, construct the SLURM configuration string.
#'
#' Check if SLURM configuration is admissible.
#'
#' @author Ricarda Rosemann
#'
#' @param slurmQOS string, name of the desired QOS (Quality of Service)
#' @param tasksPerNode numeric, number of tasks per node to be requested
#' @param tasks32 boolean, specify whether a node with 32 tasks should be requested
#' @param timeLimit character, time limit of the slurm job given in the format hh:mm:ss
#' @returns string with SLURM configuration
#'
#' @export

setSlurmConfig <- function(slurmQOS, tasksPerNode = 16, tasks32 = FALSE, timeLimit = NULL) {

  allowedQOS <- c("default", "priority", "standby", "short", "medium", "long")

  # Throw error if non-existing QOS is given
  if (!is.null(slurmQOS) && (!slurmQOS %in% allowedQOS || length(slurmQOS) != 1)) {
    stop(paste0("Invalid or more than one QOS given. Available cluster QOS are:\t",
                paste(allowedQOS, collapse = ", "), ".\n",
                "The default QOS is priority for 16 tasks and short for 32 tasks.\n"))
  }

  # set default QOS
  if (isTRUE(tasks32)) {
    if (is.null(slurmQOS) || slurmQOS == "default") {
      slurmQOS <- "short"
    } else if (slurmQOS %in% c("standby", "priority")) {
      # Throw an error if a job is started with 32 tasks on the priority partition
      stop(paste("32 tasks are only available on the standard partition.",
                 "QOS must be one of short, medium or long."))
    }
    slurmConfig <- paste0("--qos=", slurmQOS, " --nodes=1 --tasks-per-node=32",
                          " --constraint=broadwell --time=01:00:00")
    message("SLURM QOS is set to ", slurmQOS, ", using a Broadwell node with 32 CPUs.")
  } else {
    if (is.null(slurmQOS) || slurmQOS == "default") slurmQOS <- "priority"
    if (is.null(tasksPerNode)) tasksPerNode <- 16
    slurmConfig <- paste0("--qos=", slurmQOS, " --nodes=1 --tasks-per-node=", tasksPerNode)
    message("SLURM QOS is set to ", slurmQOS, " with ", tasksPerNode, " CPUs.")
    if (!is.null(timeLimit)) slurmConfig <- paste0(slurmConfig, " --time=", timeLimit)
  }

  return(slurmConfig)
}
