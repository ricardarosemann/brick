#' guess column names based on column values
#'
#' @param x data.frame with unknown column names
#' @returns data.frame with guessed column names
#'
#' @author Robin Hasse

guessColnames <- function(x) {
  if (is.null(x)) return(NULL)

  for (j in seq_len(ncol(x))) {
    if (colnames(x)[j] == "value") next
    if (all(grepl("^[A-Z]{3}$", x[[j]]))) {
      colnames(x)[j] <- "reg"
    } else if (all(grepl("^\\d{4}$", as.character(x[[j]])))) {
      colnames(x)[j] <- "ttot"
      x[[j]] <- as.numeric(x[[j]])
    } else if (all(x[[j]] %in% get("var")$getUELs())) {
      colnames(x)[j] <- "var"
    } else if (all(x[[j]] %in% get("hs")$getUELs())) {
      colnames(x)[j] <- "hs"
    } else {
      stop("Cannot identify dimension with the following elements:\n  ",
           paste(head(unique(x[[j]]), 8), collapse = ", "))
    }
  }

  return(x)
}
