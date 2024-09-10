#' Check whether Gams finished successfully
#'
#' Check which output file was written and derive state of Gams run
#'
#' If \code{output.gdx} was written, give success message.
#' If \code{abort.gdx} was written or no output file exists, stop with error message.
#'
#' @author Ricarda Rosemann
#'
#' @param path character, path to search for gams output files
#'
checkGamsSuccess <- function(path) {
  if (file.exists(file.path(path, "output.gdx"))) {
    message("Gams was successful: \"output.gdx\" exists.")
  } else if (file.exists(file.path(path, "abort.gdx"))) {
    stop("Gams aborted with \"abort.gdx\" due to errors in at least one variable.")
  } else {
    stop("Gams did not finish successfully.\n",
         "Check the files main.log and main.lst for more information.")
  }
}
