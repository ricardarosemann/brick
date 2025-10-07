#' Convert nested named list to long data.frame
#'
#' @param x named list with identical depth on each branch
#' @param split character, split names by this pattern and assume same value for
#'   each separated item
#' @param n Integer, number of nested function calls, leave default
#' @returns data.frame with column for each level of the named list
#'
#' @author Robin Hasse

listToDf <- function(x, split = NULL, n = 1) {
  if (!is.null(split)) {
    slitNames <- strsplit(names(x), split)
    x <- setNames(rep(x, lengths(slitNames)), unlist(slitNames)
    )
  }

  do.call(rbind, lapply(names(x), function(i) {
    out <- if (is.list(x[[i]])) listToDf(x[[i]], split, n + 1)  else data.frame(value = x[[i]])
    out <- cbind(i, out)
    colnames(out)[1] <- n
    return(out)
  }))
}
