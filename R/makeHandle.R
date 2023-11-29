#' Named list to handle string
#'
#' Helper function to turn a named list into a string appended to the gams
#' command line call
#'
#' Gams parameters are lower case flags and follow one minus `-`. Model switches
#' are upper case flags and follow two minuses `--`.
#'
#' @param lst named list of flags
#' @param type character, type of flags (either `"gams"` or `"model`)
#' @returns character of flags
#'
#' @author Robin Hasse
#'
#' @importFrom purrr pmap

makeHandle <- function(lst, type = c("gams", "model")) {

  type <- match.arg(type)

  handleSign <- switch(type, gams = "-", model = "--")
  transf <- switch(type, gams = tolower, model = toupper)

  if (is.null(lst)) {
    return("")
  } else if (is.list(lst)) {
    if (is.null(names(lst))) {
      stop("'lst' has to be a named list.")
    }
    paste(
      paste0(
        handleSign,
        pmap(list(transf(names(lst)), lst[]), paste, sep = "=")),
      collapse = " "
    )
  } else {
    stop("'lst' has to be a named list, not a ", class(lst), ".")
  }
}
