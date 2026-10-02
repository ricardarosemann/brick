#' Copy GDX file
#'
#' @param copyFile character, path to origin file
#' @param pasteDir character, path to target directory
#' @param pasteFileName character, target file name
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @author Robin Hasse

copyGdxFile <- function(copyFile, pasteDir, pasteFileName, overwrite = FALSE) {
  pastePath <- file.path(pasteDir, pasteFileName)
  if (!is.null(copyFile)) {
    file.copy(copyFile, pastePath, overwrite = overwrite)
    message("Using ", copyFile, " as ", pasteFileName)
  }
  return(invisible(pastePath))
}
