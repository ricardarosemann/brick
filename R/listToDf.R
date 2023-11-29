#' Convert nested named list to long data.frame
#'
#' @param x named list with identical depth on each branch
#' @param n Integer, number of nested function calls, leave default
#' @returns data.frame with column for each level of the named list
#'
#' @author Robin Hasse

listToDf <- function(x, n = 1) {
  do.call(rbind, lapply(names(x), function(i) {
    out <- if (is.list(x[[i]])) listToDf(x[[i]], n + 1) else data.frame(value = x[[i]])
    out <- cbind(i, out)
    colnames(out)[1] <- n
    return(out)
  }))
}
