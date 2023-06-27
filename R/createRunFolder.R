#' Create new run folder
#'
#' Create a folder for the model to run in and copy required gams files there.
#'
#' Create either one or multiple
#'
#' @author Robin Hasse
#'
#' @param path character vector, containing
#' @param config list with run configuration
#' @param overwrite logical; Should exiting folders be overwritten?
#' @param recursive logical; Should exiting folders be overwritten?
#' @param showWarnings logical; Should exiting folders be overwritten?
#'
#' @importFrom yaml write_yaml
#' @export
#'
createRunFolder <- function(path,
                            config = NULL,
                            overwrite = FALSE,
                            recursive = FALSE,
                            showWarnings = TRUE) {

  # find files -----------------------------------------------------------------




  # create new run folders -----------------------------------------------------

  newPaths <- unique(path[!dir.exists(path) | overwrite])
  missingPaths <- setdiff(path, newPaths)
  dir.create(newPaths, recursive = recursive)

  # warnings for paths that could not be created
  if (showWarnings && length(missingPaths > 0)) {
    warning(length(missingPaths), " out of ", length(path), " paths have not ",
            "been created:\n  ",
            paste(missingPaths, collapse = "\n  "))
  }




  ## config ====
  configFile <- attr(config, "file", exact = TRUE)
  configFolder <- file.path(newPaths, "config")
  if (!dir.exists(configFolder)) {
    dir.create(configFolder)
  }
  if (file.exists(configFile)) {
    file.copy(configFile, configFolder, overwrite = overwrite)
  }
  write_yaml(config, file.path(configFolder, "config.yaml"))

}
