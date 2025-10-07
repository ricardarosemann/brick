#' guess column names based on column values
#'
#' @param x data.frame with unknown column names
#' @param m gams Conatiner with sets
#' @returns data.frame with guessed column names
#'
#' @author Robin Hasse

guessColnames <- function(x, m) {
  if (is.null(x)) return(NULL)

  for (j in seq_len(ncol(x))) {
    if (colnames(x)[j] == "value") next
    if (all(grepl("^[A-Z]{3}$", x[[j]]))) {
      colnames(x)[j] <- "region"
    } else if (all(grepl("^\\d{4}$", as.character(x[[j]])))) {
      colnames(x)[j] <- "ttot"
      x[[j]] <- as.numeric(x[[j]])
    } else if (all(x[[j]] %in% readSymbol(m, "var"))) {
      colnames(x)[j] <- "var"
    } else if (all(x[[j]] %in% readSymbol(m, "hs"))) {
      colnames(x)[j] <- "hs"
    } else {
      success <- FALSE
      for (s in m$getSets()) {
        if (s$dimension > 1) {
          next
        }
        if (all(x[[j]] %in% s$getUELs())) {
          colnames(x)[j] <- s$name
          success <- TRUE
        }
      }
      if (!success) {
        stop("Cannot identify dimension with the following elements:\n  ",
             paste(head(unique(x[[j]]), 8), collapse = ", "))
      }
    }
  }

  return(x)
}
