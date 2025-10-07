#' interpolate and filter to get model resolution
#'
#' Missing periods are interpolated linearly and extrapolated constantly,
#' additional periods are removed and all other dimensions are filtered to the
#' elements defined in the model sets.
#'
#' @param x data.frame with temporal dimension
#' @param m gams Container with sets as known dimensions
#' @param value character, name of value column
#' @param unfilteredDims character, dimensions to be neither filtered nor extrapolated
#' @returns data.frame with temporal resolution according to model
#'
#' @author Robin Hasse
#'
#' @importFrom quitte interpolate_missing_periods_

toModelResolution <- function(x, m, value = "value", unfilteredDims = NULL) {

  if (!value %in% colnames(x)) {
    stop("Can't find the value column '", value, "'.")
  }

  # all temporal periods that should be interpolated
  periodDims <- setdiff("ttot", unfilteredDims)

  # drop lines with unknown dimension elements
  for (f in setdiff(colnames(x), c(value, periodDims, unfilteredDims))) {
    x <- x[x[[f]] %in% m$getSymbols(f)[[1]]$getUELs(), ]
  }

  # interpolate missing periods and remove additional ones
  periods <- setNames(nm = periodDims) %>%
    lapply(function(d) as.numeric(m$getSymbols(d)[[1]]$getUELs()))
  for (dim in intersect(periodDims, colnames(x))) {
    x <- x %>%
      interpolate_missing_periods_(periods[dim],
                                   expand.values = TRUE,
                                   value = value) %>%
      filter(.data[[dim]] %in% periods[[dim]])
  }

  return(x)
}
