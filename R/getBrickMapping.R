#' Retrieve mapping file from BRICK
#'
#' @param name character, file name of the mapping file
#' @param type character, Mapping type (e.g. "regional", or "sectoral")
#' @param error.missing logical, return error if the file does not exist
#' @param returnPathOnly logical, file name of the mapping file
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping
#' @export

getBrickMapping <- function(name,
                            type = "sectoral",
                            error.missing = TRUE,
                            returnPathOnly = FALSE) {

  toolGetMapping(name = system.file("extdata", type, name, package = "brick"),
                 where = "local",
                 error.missing = error.missing,
                 returnPathOnly = returnPathOnly)
}
