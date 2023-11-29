#' Check if gamstransfer is available
#'
#' Just a check to get a helpful error if gamstransfer is missing. Once the
#' package is available on GitHub, it should be listed as a dependency and this
#' file can be removed. The check is disabled during contiunous integration (CI)
#' i.a. GitHub actions and during \code{devtools:check} which is called by
#' \code{lucode2::buildLibrary}.
#'
#' @param libname not used
#' @param pkgname not used
#'
#' @author Robin Hasse

.onLoad <- function(libname, pkgname) {
  if (isFALSE(requireNamespace("gamstransfer", quietly = TRUE)) &&
        isFALSE(as.logical(Sys.getenv("NOT_CRAN", "FALSE"))) && # don't check in devtools::check
        isFALSE(as.logical(Sys.getenv("CI", "FALSE")))) {
    stop("BRICK requires the package gamstransfer. Currently, it can only be ",
         "installed from source using the gams installation. See here: ",
         "https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html",
         "#R_GAMSTRANSFER_INSTALL")
  }
}
