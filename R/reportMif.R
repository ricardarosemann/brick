#' Create mif file for model run
#'
#' The mif file contains reporting variables for the given model run.
#'
#' @param path character, path to the run
#' @param file character, path of mif file. If \code{NULL}, default file name
#'   in run folder will be used. If \code{FALSE}, no file is written and the mif
#'   data is returned instead.
#' @param tmpl character, BRICK reporting template. There has to be a brickSets
#'   mapping named with the same suffix: \code{brickSets_<tmpl>.yaml}. If NNULL,
#'   the config setting is used.
#'
#' @author Robin Hasse
#'
#' @importFrom reportbrick convGDX2MIF
#' @export

reportMif <- function(path, file = NULL, tmpl = NULL) {

  if (!dir.exists(path)) {
    stop("The given directory does not exist: ", path)
  }

  # recognised file names for model output (in descending order of priority)
  gdxNames <- c("output.gdx")

  # find gdx file
  gdx <- head(file.path(path, gdxNames), 1)
  if (is.null(gdx)) {
    stop("No gdx file found for reporting in given path: ", path)
  }

  # read config
  configFile <- file.path(path, "config", "config_COMPILED.yaml")
  config <- readConfig(configFile, readDirect = TRUE)
  neededConfigParams <- c("title", "periods")
  if (is.null(tmpl)) {
    neededConfigParams <- c(neededConfigParams, "reportingTemplate")
  }
  missingConfigParams <- setdiff(neededConfigParams, names(config))
  if (length(missingConfigParams) > 0) {
    stop("The settings ", paste(missingConfigParams, collapse = ", "),
         " are missing in the config: ", configFile)
  }

  # mif file
  file <- if (is.null(file)) {
    file.path(path, "BRICK_general.mif")
  } else if (isFALSE(file)) {
    NULL # return mif, no file written
  } else {
    file
  }

  # reporting template
  if (is.null(tmpl)) {
    tmplFile <- file.path(path, "config", "brickSets_COMPILED.yaml")
    tmpl <- if (file.exists(tmplFile)) {
      tmplFile
    } else {
      config[["reportingTemplate"]]
    }
  }

  # write mif
  convGDX2MIF(gdx,
              tmpl = tmpl,
              file =  file,
              scenario = config[["title"]],
              t = config[["periods"]])
}
