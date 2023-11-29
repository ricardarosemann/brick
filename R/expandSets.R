#' Expand set values to data frame
#'
#' Create a data frame that has a column for each set and the full crossing of
#' all set entries as rows
#'
#' @param ... gams sets
#' @returns data.frame with full crossing of set entries
#'
#' @author Robin Hasse

expandSets <- function(...) {

  lst <- list(...)

  # read set elements and dim name from gams objects
  setElements <- lapply(lst, function(l) as.character(l$getUELs()))[]
  setNames <- as.character(lapply(lst, function(l) l$name))

  # convert elements of temporal dims to numeric
  temporalSets <- c("ttot", "ttot2", "t", "thist")
  for (i in which(setNames %in% temporalSets)) {
    setElements[[i]] <- as.numeric(setElements[[i]])
  }

  # rename dims if given explicitly
  if (!is.null(names(lst))) {
    setNames <- ifelse(names(lst) == "",
                       setNames,
                       names(lst))
  }

  # create df with full crossing of all dimensions
  expand.grid(setNames(setElements, setNames),
              stringsAsFactors = FALSE,
              KEEP.OUT.ATTRS = FALSE)
}
