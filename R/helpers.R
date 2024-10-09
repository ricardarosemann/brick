#' Make zeros explicit
#'
#' Replace zeros in value column with the gams special value EPS. This way, the
#' value is saved in gdx files by gams.
#'
#' @author Robin Hasse
#'
#' @param x data.frame
#' @param value character, name of value column
#' @returns data.frame with explicit zeros in value column
#'
#' @importFrom gamstransfer SpecialValues

.explicitZero <- function(x, value = "value") {
  x[unlist(lapply(x[[value]] == 0, isTRUE)), value] <- SpecialValues[["EPS"]]
  return(x)
}
