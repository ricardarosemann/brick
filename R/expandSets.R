#' Expand set values to data frame
#'
#' Create a data frame that has a column for each set and the full crossing of
#' all set entries as rows
#'
#' @param ... gams sets
#' @param .m gams Container with sets referenced in \code{...}.
#' @returns data.frame with full crossing of set entries
#'
#' @author Robin Hasse

expandSets <- function(..., .m = NULL) {

  lst <- list(...)

  # read set elements and dim name from gams objects
  setElements <- lapply(lst, function(l) as.character(.getSet(l, .m)$getUELs()))[]
  setNames <- as.character(lapply(lst, function(l) .getSet(l, .m)$name))

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



.getSet <- function(l, .m) {
  if (is.character(l)) {
    if (is.null(.m)) {
      stop("'.m' cannot be NULL if you provide a set as character: ", l)
    } else {
      if (l %in% c(.m$listSets(), .m$listAliases())) {
        return(.m$getSymbols(l)[[1]])
      } else {
        stop("Cannot find a set called '", l, "' in '.m'")
      }
    }
  } else if (class(l)[[1]] %in% c("Set", "Alias")) {
    return(l)
  } else {
    stop("Sets can only be passed as gams set or alias object or character, ",
         "not ", class(l), ".")
  }
}
