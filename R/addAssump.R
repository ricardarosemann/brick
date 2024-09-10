#' Add assumed intangible costs
#'
#' @param df data.frame for the cost of construction or renovation
#' @param assumpFile character, file path to assumption file
#' @returns data frame with added intangible cost
#'
#' @author Robin Hasse
#'
#' @importFrom quitte interpolate_missing_periods
#' @importFrom utils read.csv2
#' @importFrom dplyr %>% .data mutate left_join select

addAssump <- function(df, assumpFile) {

  assump <- read.csv2(assumpFile, stringsAsFactors = TRUE, na.strings = "", comment.char = "#")
  assump[["value"]] <- as.numeric(as.character(assump[["value"]]))

  nos <- sort(unique(assump$.chunk))

  df[["value"]] <- 0
  df[["ttot"]] <- as.numeric(as.character(df[["ttot"]]))
  periods <- unique(df[["ttot"]])

  for (no in nos) {
    chunk <- assump[assump$.chunk == no, ]
    cols <- apply(chunk, 2, function(c) sum(!is.na(c)))
    if (length(setdiff(unique(cols), c(0, nrow(chunk))))) {
      stop("Problem in chunk no ", no, ". Every dimension of a chunk has to ",
           "be either entirely empty or entirely defined")
    }
    cols <- setdiff(union(colnames(chunk[, cols > 0]), "value"), ".chunk")
    dims <- setdiff(cols, "value")

    if ("ttot" %in% cols) {
      chunk <- chunk %>%
        interpolate_missing_periods(ttot = periods, expand.values = TRUE)
    }

    df <- left_join(df, chunk[, cols], by = dims) %>%
      mutate(value = ifelse(is.na(.data[["value.y"]]),
                            .data[["value.x"]],
                            .data[["value.y"]])) %>%
      select(-"value.x", -"value.y")
  }
  return(df)
}
