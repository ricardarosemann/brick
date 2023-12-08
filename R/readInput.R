#' Read madrat input files from input folder
#'
#' @param filename character, filename
#' @param dims character vector, column names
#' @param inputDir character, directory of input folder
#' @returns data.frame with data of the given input file
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv

readInput <- function(filename, dims, inputDir = NULL) {

  if (is.null(inputDir)) {
    inputDir <- brick.file("input")
  }

  out <- read.csv(file.path(inputDir, filename),
                  header = FALSE,
                  comment.char = "*")

  # call last column 'value' if name is missing
  if (length(dims) == ncol(out) - 1 && all(is.numeric(out[, ncol(out)]))) {
    dims <- c(dims, "value")
  }

  colnames(out) <- dims
  return(out)
}
