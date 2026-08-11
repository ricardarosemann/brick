#' Get Brick dimension mapping
#'
#' @param dim character, dimension name
#' @param granularity character, name of BRICK granularity
#' @returns data.frame with dimension mapping
#'
#' @author Robin Hasse

getDimMap <- function(dim, granularity = NULL) {

  dimGranularity <- if (is.null(granularity)) {
    NA
  } else {
    granularities <- .getMapping("granularity.csv")
    if (!granularity %in% granularities$granularity) {
      stop("The granularity '", granularity,
           "' is not defined in the granularity mapping.")
    }
    .pick(granularities, granularity = granularity)[[dim]]
  }
  if (is.na(dimGranularity)) {
    map <- .getMapping("dim")[[dim]]
  } else {
    map <- .getMapping("agg")[[dim]]
    map <- .pick(map, dimGranularity = dimGranularity)
    map[c("dimGranularity", "dim")] <- NULL
    colnames(map)[colnames(map) == "dimAgg"] <- dim
    map <- unique(map)
  }

  return(map)
}



#' Get BRICK mapping
#'
#' @param mapping character or list of characters, name of BRICK mapping. If
#'   \code{"dim"} or \code{"agg"} is passed, all corresponding dimension
#'   mappings are returned as named list.
#' @returns data frame or list of data frames
#'
#' @author Robin Hasse

.getMapping <- function(mapping) {
  if (mapping %in% c("dim", "agg")) {
    folder <- brick.file("extdata", "sectoral")
    pattern <- paste0(mapping, "_(.*)\\.csv")
    files <- list.files(folder, pattern)
    dims <- sub(pattern, "\\1", files)
    mapping <- setNames(files, dims)
  }
  m <- lapply(mapping, madrat::toolGetMapping, type = "sectoral", where = "brick")
  if (is.null(names(mapping)) && length(mapping) == 1) m[[1]] else m
}
