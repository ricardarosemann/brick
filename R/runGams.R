#' Run gams optimisation
#'
#' Run the gams model in a given directory.
#'
#' The file `input.gdx` has to exist in the given directory.
#'
#' @param path character vector with folders to run gams in
#' @param gamsOptions named list of GAMS options
#' @param switches named list of model switches
#' @param fileName character vector with gams file names
#' @param gamsCall system command to call gams
#'
#' @author Robin Hasse
#'
#' @importFrom purrr pmap
#' @importFrom withr with_dir

runGams <- function(path,
                    gamsOptions = NULL,
                    switches = NULL,
                    fileName = "main.gms",
                    gamsCall = "gams") {

  # check length of parameters
  if (!length(fileName) %in% c(1, length(path))) {
    stop("'fileName' has to have a length of ", length(path),
         ifelse(length(path) > 1, " or 1", ""), ", not ", length(fileName))
  }
  if (!length(gamsCall) %in% c(1, length(path))) {
    stop("'gamsCall' has to have a length of ", length(path),
         ifelse(length(path) > 1, " or 1", ""), ", not ", length(gamsCall))
  }

  # call this function for each path
  if (length(path) > 1) {
    stop("You need to run gams in one path at a time! ",
         "Multiple paths are not supported yet.")
    pmap(list(path, gamsOptions, switches, fileName, gamsCall), runGams)
  } else {

    # standard gams options
    if (is.null(gamsOptions)) {
      gamsOptions <- list()
    }
    if (!"logoption" %in% tolower(names(gamsOptions))) {
      gamsOptions[["logoption"]] <- 2  # Log output to logfile
    }

    # helpers
    flagString <- paste(makeHandle(gamsOptions),
                        makeHandle(switches, "model"))

    # check if input.gdx exists
    if (!file.exists(file.path(path, "input.gdx"))) {
      stop("There is no 'input.gdx' in ", path)
    }

    # run gams
    message("Start Gams: ", file.path(path, fileName))
    tic <- Sys.time()
    with_dir(path, system(paste(gamsCall, fileName, flagString)))
    toc <- Sys.time()
    message("Gams finished after ",
            round(as.numeric(difftime(toc, tic, units = "s")), 0), " sec.")

  }
}
